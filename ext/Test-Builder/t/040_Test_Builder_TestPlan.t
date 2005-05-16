#!/usr/bin/pugs

use v6;
use Test;

plan 9;

use Test::Builder::TestPlan;

my $test_plan = Test::Builder::TestPlan.new(:expect(10));

# NOTE: replace this with proper isa_ok() when that works
is($test_plan.ref, 'Test::Builder::TestPlan', '... this is a Test::Builder::TestPlan instance');

dies_ok {
    $test_plan.expect();
}, '... this property is private, so this should die';

dies_ok {
    $test_plan.expect(100);
}, '... this property is private, so this should die';

is($test_plan.header(), '1..10', '... got the header we expected');
is($test_plan.footer(:run(10)), '', '... got the footer we expected :run(10)');
is($test_plan.footer(:run(8)), 'Expected 10 but ran 8', '... got the footer we expected :run(8)');

my $null_test_plan = Test::Builder::NullPlan.new();

is($null_test_plan.ref, 'Test::Builder::NullPlan', '... this is a Test::Builder::NullPlan instance');

is($null_test_plan.header(), '', '... null plans have no header');
is($null_test_plan.footer(:run(50)), '1..50', '... got the footer we expected :run(50)');
