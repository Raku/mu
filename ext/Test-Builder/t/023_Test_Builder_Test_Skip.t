#!/usr/bin/pugs

use v6;
use Test;

plan 15;

use_ok( 'Test::Builder::Test' );

my $skip_test = ::Test::Builder::Test::Skip.new(
    number      => 1,     
    passed      => 1,
    description => 'first test description',
    reason      => 'reason for skipping'
);

is( $skip_test.ref, 'Test::Builder::Test::Skip',
    'new() should return a Test::Builder::Test::Skip instance' );

is( $skip_test.number(), 1, 'number() should return the test number' );
ok( $skip_test.passed(), 'passed() should return the passed value' );
is( $skip_test.description(), 'first test description',
    'description() should return the test description' );
is( $skip_test.diagnostic(), '???',
    'diagnostic() should return the default diagnostic if needed',
    :todo<feature>);

my $skip_diag = ::Test::Builder::Test::Skip.new(
    number      => 1,     
    passed      => 1,
    reason      => 'reason for skipping',
    diagnostic  => 'some diagnostic message',
    description => 'first test description',
);

is( $skip_diag.diagnostic(), 'some diagnostic message',
    '... but should return diagnostic if set' );

is( $skip_test.reason(), 'reason for skipping',
    'reason() should return the test reason' );

is( $skip_test.report(), 'not ok 1 #skip reason for skipping',
    'report() should return a TAP-formatted skip message' );

my %status;
lives_ok { %status = $skip_test.status() }, 'status() parses', :todo<feature>;

is( +( keys %status ), 4, 'status() should return a hash',
    :todo<feature> );

is( %status<passed>,   1, '... with a passed key set to true',
    :todo<feature> );

is( %status<description>, 'first test description',
    '... the correct test description',
    :todo<feature> );

is( %status<skip>,     1, '... a skip key set to true',
    :todo<feature> );

is( %status<reason>, 'reason for skipping', '... and the skip reason',
    :todo<feature> );
