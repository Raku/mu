role Parse::Rule::Medium;

use Parse::Rule::Strategy;
does Parse::Rule::Strategy;   # we use combinators from a strategy

role Parse::Rule::Medium::Pos { }

has $.Pos;

submethod BUILD () {
    $.Pos = Parse::Rule::Medium::Pos;
}

# vim: ft=perl6 :
