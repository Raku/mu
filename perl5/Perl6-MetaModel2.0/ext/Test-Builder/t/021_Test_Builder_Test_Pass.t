#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;

require "lib/TestBuilder.pm";

my $pass_test = $::TestBuilder->FETCH('::Test')->FETCH('::Pass')->new(
        '$.number'      => 1,     
        '$.passed'      => 1,
        '$.description' => 'first test description'
        );

isa_ok($pass_test, 'TestBuilder::Test::Pass');

is( $pass_test->number(), 1, 'number() should return the provided test number' );
ok( $pass_test->passed(),    'passed() should report the right passed value'   );
is( $pass_test->description(), 'first test description',
    'description() should report the test description' );
is( $pass_test->diagnostic(), '???',
    'diagnostic() should report the default diagnostic if needed' );

my $pass_diag = $::TestBuilder->FETCH('::Test')->FETCH('::Pass')->new(
    '$.number'      => 1,     
    '$.passed'      => 1,
    '$.diagnostic'  => 'some reason this passed',
    '$.description' => 'first test description',
);
isa_ok($pass_diag, 'TestBuilder::Test::Pass');

is($pass_diag->diagnostic(), 'some reason this passed',
    '... or the provided diagnostic' );

is($pass_test->report(), 'ok 1 - first test description',
    'report() should produce the correct TAP line' );

my %status = %{$pass_test->status()};
is(scalar keys %status, 2, 'status() should return a hash' );
is($status{passed},   1, '... with a passed key set to true' );
is($status{description}, 'first test description',
    '... and the correct test description' );
