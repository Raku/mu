package Pugs::Compiler::Perl6;
$Pugs::Compiler::Perl6::VERSION = '0.05';

# Documentation in the __END__
use 5.006;
use strict;
use warnings;

use base 'Pugs::Compiler::Regex';
use Pugs::Grammar::Perl6;
use Pugs::Compiler::Token;
use Carp;
# use Scalar::Util 'blessed';
use Data::Dumper;
use base 'Pugs::Grammar::BaseCategory';  # <ws>

*skip_spaces = Pugs::Compiler::Token->compile( q(
        [ <?ws> | ; ]*
) )->code;

sub compile {
    my ( $class, $rule_source, $param ) = @_;
    my $self = { source => $rule_source };

    $self->{grammar}  = delete $param->{grammar}  || 
                        'Pugs::Grammar::Perl6';
    $self->{p}        = delete $param->{pos}      ||
                        delete $param->{p};
    $self->{backend}  = delete $param->{backend}  ||
                        'perl5';

    warn "Error in compile: unknown parameter '$_'" 
        for keys %$param;
    #print 'rule source: ', $self->{source}, "\n";
    local $@;

    $self->{backend}  = 'perl5:Pugs::Emitter::Perl6::Perl5'
        if $self->{backend} eq 'perl5';
    $self->{backend} =~ s/^perl5://;
    #print "backend: ", $self->{backend}, "\n";
    eval " require $self->{backend} ";
        if ( $@ ) {
            carp "Error loading backend module: $@";
            return;
        }

    $self->{grammar} =~ s/^perl5://;
    #print "grammar: ", $self->{grammar}, "\n";
    eval " require $self->{grammar} ";
        if ( $@ ) {
            carp "Error loading grammar module: $@";
            return;
        }

    # in order to reduce the memory footprint:
    #       loop parsing '<ws> <statement>'; 
    #       keep the grammar tree and discard the match
    # AST = { statements => \@statement }

    my $source = $self->{source};
    my $pos = $self->{p} || 0;

    # generic grammar?
    if ( $self->{grammar} ne 'Pugs::Grammar::Perl6' ) {

        #print "Parsing $self->{grammar} \n";
        eval {
            no strict 'refs';
            $self->{ast} = &{$self->{grammar} . '::parse'}( $self->{grammar}, $source, { pos => $pos } );
            $pos = $self->{ast}->to if $self->{ast};
        };
        #print "Done Parsing $self->{grammar} \n";
        #print Dumper( $self->{ast} );
        if ( $@ ) {
            carp "Error in parser: $@";
            return;
        }
        elsif ( ! defined $self->{ast} ) {
            carp "Error in parser: No match found";
            return;
        }
        $self->{ast} = $self->{ast}->();

        if ( $self->{ast} ) {
            eval {
                no strict 'refs';
                $self->{perl5} = &{$self->{backend} . '::emit'}( 
                    $self->{grammar}, $self->{ast}, $self );
            };
            {
            no warnings 'uninitialized';
            carp "Error in perl 5 emitter: $@\nSource:\n$self->{perl5}\n" if $@;
            }
            #print 'rule perl5: ', do{use Data::Dumper; Dumper($self->{perl5})};
        }
        bless $self, $class;

    } # / generic grammar

    my @statement;
    my $error = 0;

    my $source_line_number = 1;
    my $source_pos = 0;

    # POD parser needs tail calls
    #no warnings 'recursion'; # doesn't seem to work here
    local $SIG{'__WARN__'} = sub { warn $_[0] if $_[0] !~ /recursion/ };

    while ( $pos < length( $source ) ) {

        while ( $source_pos < $pos ) {
            my $i = index( $source, "\n", $source_pos + 1);
            last if $i < 0;
            $source_pos = $i;
            $source_line_number++;
            #print "line $source_line_number at pos $source_pos\n";
            #print "line $source_line_number\n";
        }

        my $match = __PACKAGE__->skip_spaces( $source, { pos => $pos } );
        $pos = $match->to if $match;
        last if $pos >= length( $source );

        eval {
            #print "<ws> until $pos; tail [",substr( $source, $pos, 10 ),"...]\n";

            no strict 'refs';
            $self->{ast} = &{$self->{grammar} . '::statement'}( $self->{grammar}, $source, { pos => $pos } );
            #print 'match: ', Dumper( $self->{ast}() );
            #print 'match: ', Dumper( $self->{ast}->data );
            $pos = $self->{ast}->to if $self->{ast};
        };
        # print 'rule ast: ', Dumper( $self->{ast}() );

        if ( $@ ) {
            carp "Error in perl 6 parser: $@\nSource:\n'" .
                 substr( $rule_source, 0, 30 ) . "...\n";
            $error = 1;
            last;
        }
        elsif ( ! defined $self->{ast} ) {
            carp "Error in perl 6 parser: No match found for:\n'" .
                 substr( $rule_source, 0, 30 ) . "...\n";
            $error = 1;
            last;
        }

        push @statement, $self->{ast}()
            if ref $self->{ast}();
        last unless $self->{ast}->tail;
        #print "next statement: $tail \n";

    }

    if ( $pos < length( $source ) ) {
        carp "Error in perl 6 parser:\nSource:\n'" .
             substr( $rule_source, 0, 30 ) . "...\nat:\n'" .
             substr( $source, $pos, 30 ) . "...\n";
        $error = 1;
    }

    return if $error;

    $self->{ast} = { statements => \@statement };

    if ( @statement ) {
        eval {
            no strict 'refs';
            $self->{perl5} = &{$self->{backend} . '::emit'}( 
                $self->{grammar}, $self->{ast}, $self );
        };
        {
            no warnings 'uninitialized';
            carp "Error in perl 5 emitter: $@\nSource:\n$self->{perl5}\n" if $@;
        }
        #print 'rule perl5: ', do{use Data::Dumper; Dumper($self->{perl5})};
    }
    bless $self, $class;
}

1;

__END__


=head1 NAME 

Pugs::Compiler::Perl6 - An experimental compiler for Perl 6

=head1 DESCRIPTION

This module provides an implementation for Perl 6.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
