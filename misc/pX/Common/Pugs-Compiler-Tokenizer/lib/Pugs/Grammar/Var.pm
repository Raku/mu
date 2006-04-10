package Pugs::Grammar::Var;
use strict;
use warnings;
use Pugs::Compiler::Rule;
use base qw(Pugs::Grammar::Base);
#use Pugs::Runtime::Match;
#use Text::Balanced; 

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

*ident = Pugs::Compiler::Rule->compile( '
            [
                [ \:\: ]?
                [ \_ | <alnum> ]+
            ]+
            { return { scalar => "\$" . $() ,} }
        ' )->code;

our %hash = (
    '$' => Pugs::Compiler::Rule->compile( '
                <Pugs::Grammar::Var.ident>
                { return { scalar => "\$" . $() ,} }
            ', 
            grammar => 'Pugs::Grammar::Var',
        ),
    '@' => Pugs::Compiler::Rule->compile( '
                <Pugs::Grammar::Var.ident>
                { return { array => "\@" . $() ,} }
            ', 
            grammar => 'Pugs::Grammar::Var',
        ),
    '%' => Pugs::Compiler::Rule->compile( '
                <Pugs::Grammar::Var.ident>
                { return { hash => "\%" . $() ,} }
            ', 
            grammar => 'Pugs::Grammar::Var',
        ),
);

sub capture {
    # print Dumper ${$_[0]}->{match}[0]{match}[1]{capture}; 
    return ${$_[0]}->{match}[0]{match}[1]{capture};
}

*parse = Pugs::Compiler::Rule->compile( '
            %Pugs::Grammar::Var::hash
            { return Pugs::Grammar::Var::capture( $/ ) }
        ', 
        grammar => 'Pugs::Grammar::Var',
    )->code;

1;
