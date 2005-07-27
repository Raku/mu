#!/usr/bin/pugs

use v6;
use Test;

plan 10;

class Array { method test_method  { 1 }; };
class Hash { method test_method  { 1 }; };

my @named_array;

ok @named_array.test_method, "Uninitialized array";

@named_array = (1,2,3);

ok @named_array.test_method, "Populated array";

ok try { [].test_method }, "Bare arrayref";


my $arrayref = [];

$arrayref = [];

ok $arrayref.test_method, "arrayref in a variable";

my %named_hash;

ok %named_hash.test_method, "Uninitialized hash";
%named_hash = (Foo => "bar");

ok %named_hash.test_method, "Populated hash";

ok try { {foo => "bar"}.test_method }, "Bare hashref";


my $hashref = {foo => "bar"};

ok $hashref.test_method, "Named hashref";

# Now for pairs.

is(try { (:key<value>).value; }, 'value', "method on a bare pair");

my $pair = :key<value>;

is $pair.value, 'value', "method on a named pair";
