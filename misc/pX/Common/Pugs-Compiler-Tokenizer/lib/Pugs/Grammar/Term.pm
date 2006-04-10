package Pugs::Grammar::Term;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;

use Pugs::Grammar::Var;
use Pugs::Grammar::Num;
use Pugs::Grammar::Str;

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST
# TODO - term:<...>  - yada-yada-yada

our %hash;

BEGIN {
    %hash = (
        %Pugs::Grammar::Str::hash,
        %Pugs::Grammar::Num::hash,
        %Pugs::Grammar::Var::hash,
    );
    __PACKAGE__->recompile;
}

1;
