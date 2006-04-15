package Pugs::Grammar::Term;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Runtime::Match;

use Pugs::Grammar::Var;
use Pugs::Grammar::Num;
use Pugs::Grammar::Str;

# TODO - implement the "magic hash" dispatcher
# TODO - term:<...>  - yada-yada-yada
# moose=>1
# moose:<elk>
# moose:{antler()}

our %hash;

sub recompile {
    my $class = shift;
    %hash = (
        %Pugs::Grammar::Str::hash,
        %Pugs::Grammar::Num::hash,
        %Pugs::Grammar::Var::hash,
        '...' => Pugs::Compiler::Rule->compile( q(
            { 
                return { die => "not implemented" } 
            }
        ) ),
    );
    $class->SUPER::recompile;
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
