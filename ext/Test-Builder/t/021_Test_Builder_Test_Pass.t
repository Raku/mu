#!/usr/bin/pugs

use v6;
use Test;

plan 11;

use_ok( 'Test::Builder::Test' );

my $pass_test = ::Test::Builder::Test::Pass.new(
        number      => 1,     
        passed      => 1,
        description => 'first test description'
        );

is( $pass_test.ref, 'Test::Builder::Test::Pass',
    '... we got a Test::Builder::Test::Pass instance' );

is( $pass_test.number(), 1, 'number() should return the provided test number' );
ok( $pass_test.passed(),    'passed() should report the right passed value' );
is( $pass_test.description(), 'first test description',
    'description() should report test description' );
is( $pass_test.diagnostic(), '???',
    'diagnostic() should report the default diagnostic if needed',
    :todo<feature> );

my $pass_diag = ::Test::Builder::Test::Pass.new(
    number      => 1,     
    passed      => 1,
    diagnostic  => 'some reason this passed',
    description => 'first test description',
);

is( $pass_diag.diagnostic(), 'some reason this passed',
    '... or the provided diagnostic' );

is( $pass_test.report(), 'ok 1 - first test description',
    'report() should produce the correct TAP line' );

my %status = $pass_test.status();
is( +( keys %status ), 2, 'status() should return a hash' );
is( %status<passed>,   1, '... with a passed key set to true' );
is( %status<description>, 'first test description',
    '... and the correct test description' );
