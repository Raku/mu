package Pugs::Grammar::Str;
use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;
use Pugs::Runtime::Match;
use Text::Balanced; 

=for pod

Parses the text inside strings like:

    '...' 
    "..."
    q(...)
    qq(...)
    qw(...)
    <... ...>
    <<... ...>>
    «...»

Quoting constructs are macros:

    macro quote:<qX> (*%adverbs) {...}

=head1 See also

S02

=cut

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

our %hash = (
    q(') => Pugs::Compiler::Rule->compile( q(
                [ . | \\ \' ]* 
                \'
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
    q(") => Pugs::Compiler::Rule->compile( q(
                [ . | \\ \" ]* 
                \"
            ), 
            grammar => 'Pugs::Grammar::Str',
        ),
);

*parse = Pugs::Compiler::Rule->compile( '
        %Pugs::Grammar::Str::hash
' )->code;

1;
