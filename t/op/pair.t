#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Pair test

=cut

plan 43;

# basic Pair

my $pair = 'foo' => 'bar';
is(ref($pair), 'Pair', 'we got a Pair type');

# get key and value from the pair as many was as possible

is(key($pair), 'foo', 'got the right key($pair)');
is(value($pair), 'bar', 'got the right value($pair)');

is(key $pair, 'foo', 'got the right key $pair');
is(value $pair, 'bar', 'got the right value $pair');

is($pair.key(), 'foo', 'got the right $pair.key()');
is($pair.value(), 'bar', 'got the right $pair.value()');

is($pair.key, 'foo', 'got the right $pair.key');
is($pair.value, 'bar', 'got the right $pair.value');

# get both (kv) as many ways as possible

my @pair1a = kv($pair);
is(+@pair1a, 2, 'got the right number of elements in the list');
is(@pair1a[0], 'foo', 'got the right key');
is(@pair1a[1], 'bar', 'got the right value');

my @pair1b = kv $pair;
is(+@pair1b, 2, 'got the right number of elements in the list');
is(@pair1b[0], 'foo', 'got the right key');
is(@pair1b[1], 'bar', 'got the right value');

my @pair1c = $pair.kv;
is(+@pair1c, 2, 'got the right number of elements in the list');
is(@pair1c[0], 'foo', 'got the right key');
is(@pair1c[1], 'bar', 'got the right value');

my @pair1d = $pair.kv();
is(+@pair1d, 2, 'got the right number of elements in the list');
is(@pair1d[0], 'foo', 'got the right key');
is(@pair1d[1], 'bar', 'got the right value');

# Pair with a numeric value

my $pair2 = 'foo' => 2;
is(ref($pair2), 'Pair', 'we got a Pair type');

is($pair2.value, 2, 'got the right value');

# Pair with a Pair value

my $pair3 = "foo" => ("bar" => "baz");
is(ref($pair3), 'Pair', 'got a Pair type');

my $pair3a = $pair3.value;
is(ref($pair3a), 'Pair', 'got a Pair type');
is($pair3a.key, 'bar', 'got right nested pair key');
is($pair3a.value, 'baz', 'got right nested pair key');

is($pair3.value.key, 'bar', 'got right nested pair key (method chaining)');
is($pair3.value.value, 'baz', 'got right nested pair key (method chaining)');

# Pair with a Pair key

my $pair4 = ("foo" => "bar") => "baz";
is(ref($pair4), 'Pair', 'got a Pair type');

is($pair4.value, 'baz', 'got the right value');

is(ref($pair4.key), 'Pair', 'got a Pair type');
is($pair4.key.key, 'foo', 'got right nested key');
is($pair4.key.value, 'bar', 'got right nested value');

# Pair list a la http://www.nntp.perl.org/group/perl.perl6.language/19360

my $list = 1 => 2 => 3 => 4;
is(ref($list), 'Pair', 'got a Pair type');

my $key = $list.key;
is($key, 1, 'the key is 1');
is(ref($list.value), 'Pair', 'the value is a Pair type');
is($list.value.key, 2, 'the list.value.key is 2');
is(ref($list.value.value), 'Pair', 'the list.value.value is a Pair type');
is($list.value.value.key, 3, 'the list.value.value.key is 3');
is($list.value.value.value, 4, 'the list.value.value.value is 4');

# TODO tests

my $quux = eval '(quux => "xyzzy").key';
todo_is($quux, 'quux', "lhs quotes" );

# lvalue Pair assignments from S06 and thread starting with
# http://www.nntp.perl.org/group/perl.perl6.language/19425

my $val;
eval '("foo" => $val) = "baz"';
todo_ok($val eq "baz", "lvalue lists");


