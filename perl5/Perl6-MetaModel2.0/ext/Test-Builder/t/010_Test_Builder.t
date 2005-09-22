#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;

require "lib/TestBuilder.pm";

my $Test = $::TestBuilder->class::new();
isa_ok($Test, 'TestBuilder');

{
    my $Test2 = $::TestBuilder->class::new();
    isa_ok($Test2, 'TestBuilder');
    
    cmp_ok($Test, '==', $Test2, '... they are the same instance (singleton)');
}

my $custom_plan = $::TestBuilder->FETCH('::TestPlan')->new();
my $Test3       = $::TestBuilder->class::create('$.testplan' => $custom_plan);
cmp_ok($Test3, '!=', $Test, 'create() should return non-singleton object' );

cmp_ok($Test3->testplan, '==', $custom_plan, '... allowing plan setting' );

{
    # avoid those annoying "used only once" warnings
    my @blah = ($::TestBuilder);
}