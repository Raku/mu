#!/usr/bin/pugs
use v6;

use Test;

sub foo (?$foo = 5 is copy, ?$bar = 10 is copy) {
    $foo += $bar;
    $foo;
}

plan 7;

is(eval('foo()'), 15, 'calling without arguments');

is(eval('foo(10)'), 20, 'calling with one argument');
is(eval('foo(10, 15)'), 25, 'calling with two arguments');

my ($baz, $quux) = (10, 15);

is(eval('foo($baz)'), 20, 'calling with one argument');
is($baz, 10, 'variable was not affected');

is(eval('foo($baz, $quux)'), 25, 'calling with two arguments');
is($baz, 10, 'variable was not affected');
