#!/usr/bin/pugs

use v6;
use Test;

plan 5;

sub f() { 
    my sub g(){"g"}; my sub h(){g()}; h();
};
is(f(),'g');


sub foo($x) { $x + 1 }

sub callit(&foo) {
    foo(1);
}

is(foo(1), 2);
is(callit({ $^x + 2 }), 3, "lexical subs get precedence over package subs");

sub infix:<@@> ($x, $y) { $x + $y }

sub foo2(&infix:<@@>) {
    2 @@ 3;
}

is(2 @@ 3, 5);
is(foo2({ $^x * $^y }), 6);

# vim: ft=perl6 :
