#!/usr/bin/pugs

use v6;
use Test;

plan 6;

use_ok('Test::Builder::Test');

my $pass_test = ::Test::Builder::Test::Pass.new(
        number      => 1,     
        passed      => 1,
        description => 'first test description'
        );
is($pass_test.ref, 'Test::Builder::Test::Pass', '... we got a Test::Builder::Test::Pass instance');

is($pass_test.number(), 1, '... got the right test number');
ok($pass_test.passed(), '... got the right passed value');
is($pass_test.description(), 'first test description', '... got the right test description');
is($pass_test.diagnostic(), '???', '... got the right test diagnostic', :todo<feature>);