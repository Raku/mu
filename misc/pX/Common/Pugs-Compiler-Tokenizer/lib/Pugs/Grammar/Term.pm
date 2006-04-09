package Pugs::Grammar::Term;
use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;
use Pugs::Runtime::Match;

use Pugs::Grammar::Var;
use Pugs::Grammar::Num;
use Pugs::Grammar::Str;

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

our %hash = (
    %Pugs::Grammar::Str::hash,
    %Pugs::Grammar::Num::hash,
    %Pugs::Grammar::Var::hash,
);

*parse = Pugs::Compiler::Rule->compile( '
        %Pugs::Grammar::Term::hash
' )->code;

1;
