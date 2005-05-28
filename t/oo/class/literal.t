#!/usr/bin/pugs
use v6;
use Test;

plan 3;

BEGIN { @*INC.unshift('t/oo/class/TestFiles'); }

# Testing class literals
require Foo;
my $test1;

lives_ok {
    $test1 = ::Foo;
}, "::Foo is a valid class literal";

isa_ok($test1, "Class", "It's a class");

lives_ok {
    my $x = Foo;
}, "Foo is now a valid class literal",  :todo<feature>;
