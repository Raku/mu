#!/usr/bin/pugs

use v6;
use Test;

plan 7;

use_ok('Test::Builder::Test');

my $skip_test = ::Test::Builder::Test::Skip.new(
        number      => 1,     
        passed      => 1,
        description => 'first test description',
        reason      => 'reason for skipping'
        );
is($skip_test.ref, 'Test::Builder::Test::Skip', '... we got a Test::Builder::Test::Skip instance');

is($skip_test.number(), 1, '... got the right test number');
ok($skip_test.passed(), '... got the right passed value');
is($skip_test.description(), 'first test description', '... got the right test description');
is($skip_test.diagnostic(), '???', '... got the right test diagnostic', :todo<feature>);
is($skip_test.reason(), 'reason for skipping', '... got the right test reason');