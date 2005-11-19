#!/usr/bin/pugs

use v6;
use Test;

plan 9;

use Test::Builder::TestPlan;

my $test_plan = Test::Builder::TestPlan.new(:expect(10));

# NOTE: replace this with proper isa_ok() when that works
isa_ok( $test_plan, Test::Builder::TestPlan,
    'new() should return a Test::Builder::TestPlan instance' );

dies_ok { $test_plan.expect() },
    '$!expect is a private property, so expect() should die';

dies_ok { $test_plan.expect(100) }, '... as should expect( value )';

is( $test_plan.header(), '1..10', 'header() should return a valid header' );
is( $test_plan.footer( :run(10) ), '',
    'footer() should return nothing for running all expected tests' );
is( $test_plan.footer( :run(8) ), 'Expected 10 but ran 8',
    '... or a missing tests warning for running fewer' );

my $null_test_plan = Test::Builder::NullPlan.new();

isa_ok( $null_test_plan, 'Test::Builder::NullPlan',
    'new() should return a Test::Builder::NullPlan instance');

is( $null_test_plan.header(), '',
    'header() should return an empty string for a null plan' );

is( $null_test_plan.footer(:run(50)), '1..50',
    'footer() should return the test header for a null plan');
