use strict;
use warnings;
#use Smart::Comments;

package Pugs::Compiler::Grammar;

use Pugs::Grammar::Rule;
use Pugs::Emitter::Grammar::Perl5;
use Carp qw(carp croak);

sub compile {
    my ($class, $src) = @_;
    my $ast = Pugs::Grammar::Rule->grammars($src)->();
    if ($ast) {
        ## $ast
        my $perl5;
        for my $g (@$ast) {
            ### Grammar found...
            $g = $g->();
            my ($name) = keys %$g;
            ### Grammar: $name
            $perl5 .= Pugs::Emitter::Grammar::Perl5::emit($g);
        }
        bless {
            source => $src,
            ast => $ast,
            perl5 => $perl5,
        }, $class;
    } else {
        carp "Failed to compile the grammar source";
    }
}

sub perl5 {
    $_[0]->{perl5};
}

1;
__END__

=head1 NAME

Pugs::Compiler::Grammar - Compiler for Perl 6 Grammars

=head1 SYNOSPIS

    use Pugs::Compiler::Grammar;
    my $grammar = q{
        grammar MyC;

        token def {
            <type> <?ws> <var_list> <?ws>? ';'
        }

        token type { int | float | double | char }

        token var_list {
            <ident>**{1} <?ws>? [ ',' <?ws>? <ident> ]*
        }

        grammar MyVB;

        token def {
            'Dim' <?ws> <MyC.var_list>
            [ <?ws> 'As' <?ws> <MyC.type> ]? <?ws>? ';'
        }
    };
    my $obj = Pugs::Compiler::Grammar->compile($grammar);
    my $perl5 = $obj->perl5;
    eval $perl5; die $@ if $@;
    my $match = MyC->def("float foo;");
    print "type: ", $match->{type}, "\n";
    print "vars: ", $match->{var_list}, "\n";
    $match = MyVB->def("Dim foo, bar;");
    print "vars: ", $match->{'MyC.var_list'}, "\n";

=head1 METHODS

=over

=item C<< $obj = Pugs::Compile::Grammar->compile($src) >>

Acts as contructor. Accepts a string containing the grammar
specs and returns a Pugs::Compiler::Grammar instance.

=item C<< $perl5 = $obj->perl5() >>

Returns a string containing the Perl source for the grammars
compiled.

=back

=head1 AUTHOR

The Pugs contributors E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2007 by Agent Zhang and others.

This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=head1 SEE ALSO

L<compile_p6grammar>,
L<Pugs::Compiler::Regex>,
L<Pugs::Grammar::Rule>,
L<Pugs::Emitter::Grammar::Perl5>.

