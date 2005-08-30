#!/usr/bin/pugs
use v6;

use Test;

sub foo (?$foo = 5 is copy, ?$bar = 10 is copy) {
    $foo += $bar;
    $foo;
}

plan 7;

is(try { foo() }, 15, 'calling without arguments', :todo<bug>);

is(try { foo(10) }, 20, 'calling with one argument', :todo<bug>);
is(try { foo(10, 15) }, 25, 'calling with two arguments', :todo<bug>);

my ($baz, $quux) = (10, 15);

is(try { foo($baz) }, 20, 'calling with one argument', :todo<bug>);
is($baz, 10, 'variable was not affected');

is(try { foo($baz, $quux) }, 25, 'calling with two arguments', :todo<bug>);
is($baz, 10, 'variable was not affected');
