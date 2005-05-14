#!/usr/bin/pugs

use v6;
use Test;

plan 13;

# type based dispatching

multi sub foo (Int $bar)   { "Int "  ~ $bar  }
multi sub foo (Str $bar)   { "Str "  ~ $bar  }
multi sub foo (Num $bar)   { "Num "  ~ $bar  }
multi sub foo (Rat $bar)   { "Rat "  ~ $bar  }
multi sub foo (Bool $bar)  { "Bool " ~ $bar  }
multi sub foo (Sub $bar)   { "Sub " ~ $bar() }
multi sub foo (Array @bar) { "Array " ~ join(', ', @bar) }
multi sub foo (Hash %bar)  { "Hash " ~ join(', ', %bar.keys) }
multi sub foo (IO $fh)     { "IO" }

is(foo('test'), 'Str test', 'dispatched to the Str sub');
is(foo(2), 'Int 2', 'dispatched to the Int sub');

my $num = '4';
is(foo(+$num), 'Num 4', 'dispatched to the Num sub');
is(foo(1.5), 'Rat 1.5', 'dispatched to the Rat sub');
is(foo(1 == 1), 'Bool 1', 'dispatched to the Bool sub');
is(foo(sub { 'baz' }), 'Sub baz', 'dispatched to the Sub sub', :todo<bug>);

my @array = ('foo', 'bar', 'baz');
is(foo(@array), 'Array foo, bar, baz', 'dispatched to the Array sub');

my %hash = ('foo' => 1, 'bar' => 2, 'baz' => 3);
is(foo(%hash), 'Hash foo, bar, baz', 'dispatched to the Hash sub', :todo<bug>);

is(foo($*ERR), 'IO', 'dispatched to the IO sub');

eval_ok('multi sub foo( (Int, Str) $tuple: ) '
	~ '{ "Tuple(2) " ~ $tuple.join(",") }',
	"declare sub with tuple argument", :todo<feature>);

eval_ok('multi sub foo( (Int, Str, Str) $tuple: ) '
	~ '{ "Tuple(3) " ~ $tuple.join(",") }',
	"declare multi sub with tuple argument", :todo<feature>);

is(foo([3, "Four"]), "Tuple(2) 3,Four", "call tuple multi sub", :todo<feature>);
is(foo([3, "Four", "Five"]), "Tuple(3) 3,Four,Five", "call tuple multi sub", :todo<feature>);
