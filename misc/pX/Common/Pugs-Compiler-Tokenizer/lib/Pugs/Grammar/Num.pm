package Pugs::Grammar::Num;
use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;
use Pugs::Runtime::Match;

=for pod

Parses the text inside strings like:

    1
    1.1
    +1
    1e10
    
=head1 See also

=cut

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

our %hash = (
    q() => Pugs::Compiler::Rule->compile( q(
                \d+ 
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
    q(Inf) => 1,
    q(NaN) => 1,
);

*parse = Pugs::Compiler::Rule->compile( '
        %Pugs::Grammar::Num::hash
' )->code;

1;
