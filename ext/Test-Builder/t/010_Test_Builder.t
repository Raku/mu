#!/usr/bin/pugs

use v6;
use Test;

plan 3;

use_ok('Test::Builder');

my $Test = ::Test::Builder.new();
is($Test.ref, 'Test::Builder', '... we got a Test::Builder instance');

{
    my $Test2 = ::Test::Builder.new();
    ok($Test =:= $Test2, '... Test::Builder is a singleton', :todo<feature>);
}