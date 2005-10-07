#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;

require "lib/TestBuilder.pm";

isa_ok($::TestBuilder, 'TestBuilder');
isa_ok($::TestBuilder->FETCH('::Output'), 'TestBuilder::Output');
isa_ok($::TestBuilder->FETCH('::TestPlan'), 'TestBuilder::TestPlan');
isa_ok($::TestBuilder->FETCH('::Test'), 'TestBuilder::Test');