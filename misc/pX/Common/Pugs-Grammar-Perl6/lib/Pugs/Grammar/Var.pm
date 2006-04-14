package Pugs::Grammar::Var;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

=for pod

Parses the text inside strings like:

    $a
    @a
    %a
    &a
    
and maybe subscripts, dereferences, and method calls

    @baz[3](1,2,3){$xyz}<blurfl>.attr()

and maybe

    \(...)
    ^T

=head1 See also

=cut

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

#~ *ident = Pugs::Compiler::Rule->compile( '
            #~ ([
                #~ [ \:\: ]?
                #~ [ \_ | <alnum> ]+
            #~ ]+)
            #~ { return { ident => $_[0][0]() ,} }
        #~ ' )->code;

BEGIN {
    __PACKAGE__->add_rule( '$' => q(
                <Pugs::Grammar::Rule.ident>
                { return { scalar => '$' . $_[0]->{'Pugs::Grammar::Rule.ident'}() ,} }
            ) );
    __PACKAGE__->add_rule( '@' => q(
                <Pugs::Grammar::Rule.ident>
                { return { array => "\@" . $_[0]->{'Pugs::Grammar::Rule.ident'}() ,} }
            ) );
    __PACKAGE__->add_rule( '%' => q(
                <Pugs::Grammar::Rule.ident>
                { return { hash  => "\%" . $_[0]->{'Pugs::Grammar::Rule.ident'}() ,} }
            ) );
    __PACKAGE__->recompile;
}

1;
