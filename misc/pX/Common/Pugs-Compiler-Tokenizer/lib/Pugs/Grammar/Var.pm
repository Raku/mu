package Pugs::Grammar::Var;
use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;
use Pugs::Runtime::Match;
use Text::Balanced; 

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

my $ident = Pugs::Compiler::Rule->compile( '
        [
            [ \:\: ]?
            [ \_ | <alnum> ]+
        ]+
    ', 
    grammar => 'Pugs::Grammar::Str',
);

our %hash = (
    map {
        $_ => $ident
    }
    qw( $ % @ )
);

*parse = Pugs::Compiler::Rule->compile( '
    %Pugs::Grammar::Var::hash
' )->code;

1;
