#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 9;
use Test::Exception;

require "lib/TestBuilder.pm";

my $test_plan = $::TestBuilder->FETCH('::TestPlan')->new('$:expect' => 10);
isa_ok($test_plan, 'TestBuilder::TestPlan');

dies_ok { 
    $test_plan->expect() 
} '$:expect is a private property, so expect() should die';

dies_ok { 
    $test_plan->expect(100) 
} '... as should expect( value )';

is($test_plan->header(), '1..10', 'header() should return a valid header');
is($test_plan->footer(10), '', 'footer() should return nothing for running all expected tests' );
is($test_plan->footer(8), 'Expected 10 but ran 8', '... or a missing tests warning for running fewer' );

my $null_test_plan = $::TestBuilder->FETCH('::NullPlan')->new();
isa_ok($null_test_plan, 'TestBuilder::NullPlan');

is($null_test_plan->header(), '', 'header() should return an empty string for a null plan');
is($null_test_plan->footer(50), '1..50', 'footer() should return the test header for a null plan');

{
    # avoid those annoying "used only once" warnings
    my @blah = ($::TestBuilder);
}