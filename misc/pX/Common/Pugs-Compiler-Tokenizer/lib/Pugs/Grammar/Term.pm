package Pugs::Grammar::Term;
use strict;
use warnings;
use Pugs::Compiler::Rule;
use base qw(Pugs::Grammar::Base);
#use Pugs::Runtime::Match;

use Pugs::Grammar::Var;
use Pugs::Grammar::Num;
use Pugs::Grammar::Str;

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST
# TODO - term:<...>  - yada-yada-yada

our %hash = (
    %Pugs::Grammar::Str::hash,
    %Pugs::Grammar::Num::hash,
    %Pugs::Grammar::Var::hash,
);

sub capture {
    # print Dumper ${$_[0]}->{match}[0]{match}[1]{capture}; 
    return ${$_[0]}->{match}[0]{match}[1]{capture};
}

*parse = Pugs::Compiler::Rule->compile( '
    %Pugs::Grammar::Term::hash
    { return Pugs::Grammar::Term::capture( $/ ) }
' )->code;

1;
