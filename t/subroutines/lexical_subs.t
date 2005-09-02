#!/usr/bin/pugs

use v6;
use Test;

plan 3;

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



# vim: ft=perl6 :
