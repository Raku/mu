package Pugs::Compiler::Perl6;
$Pugs::Compiler::Perl6::VERSION = '0.05';

# Documentation in the __END__
use 5.006;
use strict;
use warnings;

use base 'Pugs::Compiler::Regex';
use Pugs::Grammar::Perl6;

use Data::Dumper;

sub compile {
    my ( $class, $rule_source, $param ) = @_;
    my $self = { source => $rule_source };

    $self->{grammar}  = delete $param->{grammar}  || 
                        'Pugs::Grammar::Perl6';
    $self->{p}        = delete $param->{pos}      ||
                        delete $param->{p};
    warn "Error in rule: unknown parameter '$_'" 
        for keys %$param;
    #print 'rule source: ', $self->{source}, "\n";
    $self->{ast} = Pugs::Grammar::Perl6->rule( 
        $self->{source} );
    die "Error in rule: '$rule_source' at: '$self->{ast}{tail}'\n" if $self->{ast}{tail};
    #print 'rule ast: ', do{use Data::Dumper; Dumper($self->{ast}{capture})};

    $self->{perl5} = Pugs::Emitter::Perl6::Perl5::emit( 
        $self->{grammar}, $self->{ast}{capture}, $self );
    #print 'rule perl5: ', do{use Data::Dumper; Dumper($self->{perl5})};

    local $@;
    $self->{code} = eval 
        $self->{perl5};
    die "Error in evaluation: $@\nSource:\n$self->{perl5}\n" if $@;

    bless $self, $class;
}

1;

__END__

=head1 NAME 

Pugs::Compiler::Perl6 - Compiler for Perl 6

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
