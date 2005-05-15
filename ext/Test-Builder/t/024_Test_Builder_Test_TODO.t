#!/usr/bin/pugs

use v6;
use Test;

plan 7;

use_ok('Test::Builder::Test');

my $todo_test = ::Test::Builder::Test::TODO.new(
        number      => 1,     
        passed      => 1,
        description => 'first test description',
        reason      => 'reason for TODO-ing'
        );
is($todo_test.ref, 'Test::Builder::Test::TODO', '... we got a Test::Builder::Test::TODO instance');

is($todo_test.number(), 1, '... got the right test number');
ok($todo_test.passed(), '... got the right passed value');
is($todo_test.description(), 'first test description', '... got the right test description');
is($todo_test.diagnostic(), '???', '... got the right test diagnostic', :todo<feature>);
is($todo_test.reason(), 'reason for TODO-ing', '... got the right test reason');