#!/usr/bin/pugs

use v6;

say "1..3";

sub foo () {
    say "ok";
    &bar.goto("param1", "param2");
}

sub bar ($param1, $param2) {
    if $param1 eq "param1" and $param2 eq "param2" {
        say "ok";
    } else {
        say "not ok";
    }
}

bar("param1", "param2");
foo();
