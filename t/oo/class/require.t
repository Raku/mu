#!/usr/bin/pugs
use v6;
use Test;

plan 3;

BEGIN { @*INC.unshift('t/oo/class/TestFiles'); }

lives_ok {
    require ::Bar; # Should be equivalent to "require Bar"
}, "We can generally require class literals";

my $test1;
lives_ok {
    $test1 = ::Foo;
}, "::Foo is a valid class literal";

lives_ok {
require $test1; # Should be equivalent to "require ::Foo"
}, "Can require a class literal in a variable";
