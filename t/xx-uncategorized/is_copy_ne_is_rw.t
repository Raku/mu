use v6-alpha;
use Test;

sub foo ($foo is copy = 5, $bar is copy = 10) {
    $foo += $bar;
    $foo;
}

plan 7;

is(try { foo() }, 15, 'calling without arguments');

is(try { foo(10) }, 20, 'calling with one argument');
is(try { foo(10, 15) }, 25, 'calling with two arguments');

my ($baz, $quux) = (10, 15);

is(try { foo($baz) }, 20, 'calling with one argument');
is($baz, 10, 'variable was not affected');

is(try { foo($baz, $quux) }, 25, 'calling with two arguments');
is($baz, 10, 'variable was not affected');
