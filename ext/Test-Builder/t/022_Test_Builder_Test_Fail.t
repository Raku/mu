#!/usr/bin/pugs

use v6;
use Test;

plan 11;

use_ok( 'Test::Builder::Test' );

my $fail_test = ::Test::Builder::Test::Fail.new(
        number      => 1,     
        passed      => 0,
        description => 'first test description'
        );

is( $fail_test.ref, 'Test::Builder::Test::Fail',
	'new() should return a Test::Builder::Test::Fail instance' );

is( $fail_test.number(), 1, 'number() should return the provided test number' );
ok(!$fail_test.passed(),    'passed() should report the right passed value' );
is( $fail_test.description(), 'first test description',
	'description() should report the test description' );
is( $fail_test.diagnostic(), '???',
	'diagnostic() should report the default diagnostic if needed',
	:todo<feature>);

my $fail_diag = ::Test::Builder::Test::Fail.new(
        number      => 1,     
        passed      => 0,
		diagnostic  => 'some reason this failed',
        description => 'first test description',
);

is( $fail_diag.diagnostic(), 'some reason this failed',
	'... or the provided doiagnostic' );

is( $fail_test.report(), 'not ok 1 - first test description',
	'report() should produce the correct TAP line' );

my %status = $fail_test.status();
is( +( keys %status ), 2, 'status() should return a hash' );
is( %status<passed>, 0, '... with a passed key set to false' );
is( %status<description>, 'first test description',
	'... and the correct test description' );
