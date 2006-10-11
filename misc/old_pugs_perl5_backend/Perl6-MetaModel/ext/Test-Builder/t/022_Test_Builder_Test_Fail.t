#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;

require "lib/TestBuilder.pm";

my $fail_test = $::TestBuilder->FETCH('::Test')->FETCH('::Fail')->new(
        '$.number'      => 1,     
        '$.passed'      => 0,
        '$.description' => 'first test description'
        );

isa_ok($fail_test, 'TestBuilder::Test::Fail');

is( $fail_test->number(), 1, 'number() should return the provided test number' );
ok(!$fail_test->passed(),    'passed() should report the right passed value' );
is( $fail_test->description(), 'first test description',
    'description() should report the test description' );
is( $fail_test->diagnostic(), '???',
    'diagnostic() should report the default diagnostic if needed' );

my $fail_diag = $::TestBuilder->FETCH('::Test')->FETCH('::Fail')->new(
        '$.number'      => 1,     
        '$.passed'      => 0,
        '$.diagnostic'  => 'some reason this failed',
        '$.description' => 'first test description',
);
isa_ok($fail_diag, 'TestBuilder::Test::Fail');

is( $fail_diag->diagnostic(), 'some reason this failed',
    '... or the provided diagnostic' );

is( $fail_test->report(), 'not ok 1 - first test description',
    'report() should produce the correct TAP line' );

my %status = %{$fail_test->status()};
is(scalar keys %status, 2, 'status() should return a hash' );
is($status{passed}, 0, '... with a passed key set to false' );
is($status{description}, 'first test description',
    '... and the correct test description' );
