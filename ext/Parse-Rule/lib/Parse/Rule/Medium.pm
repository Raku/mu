role Parse::Rule::Medium;

use Parse::Rule::Strategy;
does Parse::Rule::Strategy;   # we use combinators from a strategy

our role Pos { }

has $.Pos;

submethod BUILD () {
    $.Pos = Pos;
}

# vim: ft=perl6 :
