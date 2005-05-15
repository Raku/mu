#!/usr/bin/pugs

use v6;
use Test;

plan 6;

use_ok('Test::Builder::Test');

my $fail_test = ::Test::Builder::Test::Fail.new(
        number      => 1,     
        passed      => 0,
        description => 'first test description'
        );
is($fail_test.ref, 'Test::Builder::Test::Fail', '... we got a Test::Builder::Test::Fail instance');

is($fail_test.number(), 1, '... got the right test number');
ok(!$fail_test.passed(), '... got the right passed value');
is($fail_test.description(), 'first test description', '... got the right test description');
is($fail_test.diagnostic(), '???', '... got the right test diagnostic', :todo<feature>);