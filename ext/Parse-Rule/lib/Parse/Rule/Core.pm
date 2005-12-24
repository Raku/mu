module Parse::Rule::Core;

class Medium { }

class Match {
    has Medium $.start;
    has Medium $.end;
    
    has Match $.match_num;
    has Match $.match_name;

    has $.multidex;  # Match doesn't seem the right place to store this
                     # but neither does Result
                     # maybe there's a level of indirection in between

    submethod BUILD () {
        $.match_num //= [];
        $.match_name //= {};
        $.multidex //= [];
    }
}

class Result {
    has Code $.backtrack;
    has Code $.marks;
    has Medium $.pos;
    has Match $.match;

    submethod BUILD () {
        $.marks //= {};
    }
}

class Parser {
    has Code $.parse;
}

# vim: ft=perl6 :
