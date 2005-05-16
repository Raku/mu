#!/usr/bin/pugs

use v6;
use Test;

plan 13;

use Test::Builder::Test;

my $todo_test = Test::Builder::Test::TODO.new(
    number      => 1,     
    passed      => 1,
    description => 'first test description',
    reason      => 'reason for TODO-ing'
);

is( $todo_test.ref, 'Test::Builder::Test::TODO',
    'new() should return a Test::Builder::Test::TODO instance' );

is( $todo_test.number(), 1, 'number() should return the right test number' );
ok( $todo_test.passed(),    'passed() should always be true' );
is( $todo_test.description(), 'first test description',
    'description() should return the test description' );
is( $todo_test.diagnostic(), '???',
    'diagnostic() should return the default diagnostic if needed',
    :todo<feature>);

my $todo_diag = Test::Builder::Test::TODO.new(
    number      => 1,     
    passed      => 0,
    diagnostic  => 'some test diagnostic',
    description => 'first test description',
    reason      => 'reason for TODO-ing'
);

is( $todo_test.diagnostic(), 'some test diagnostic',
    '... or the provided diagnostic',
    :todo<feature> );

is( $todo_test.reason(), 'reason for TODO-ing',
    'reason() should return the test reason' );

my %status;
lives_ok { %status = $todo_diag.status() }, 'status() parses',
    :todo<feature>;

is( +( keys %status ),      5, 'status() should return a hash',
    :todo<feature> );

is( %status<passed>,        1, '... with a passed key set to true',
    :todo<feature> );

is( %status<TODO>,          1, '... a TODO key set to true',
    :todo<feature> );

is( %status<really_passed>, 0,
    '... the really_passed key set to the passed value',
    :todo<feature> );

is( %status<description>, 'first test description',
    '... and the correct test description',
    :todo<feature> );
