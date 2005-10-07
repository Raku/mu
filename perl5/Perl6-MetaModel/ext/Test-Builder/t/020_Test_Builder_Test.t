#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;

require "lib/TestBuilder.pm";

my $pass_test = $::TestBuilder->FETCH('::Test')->class::new(
        '$.number'      => 1,     
        '$.passed'      => 1,
        '$.description' => 'first test description'
        );
isa_ok($pass_test, 'TestBuilder::Test::Pass');

my $fail_test = $::TestBuilder->FETCH('::Test')->class::new(
        '$.number'      => 2,     
        '$.passed'      => 0,
        '$.description' => 'first test description'
        );
isa_ok($fail_test, 'TestBuilder::Test::Fail');

my $todo_test = $::TestBuilder->FETCH('::Test')->class::new(
        '$.number'      => 3,     
        '$.passed'      => 1,
        '$.description' => 'first test description',
        '$.todo'        => 1,
        '$.reason'      => 'this is TODO',         
        );
isa_ok($todo_test, 'TestBuilder::Test::TODO');

my $skip_test = $::TestBuilder->FETCH('::Test')->class::new(
        '$.number'      => 4,     
        '$.passed'      => 1,
        '$.description' => 'first test description',
        '$.skip'        => 1,
        '$.reason'      => 'this is TODO',         
        );
isa_ok($skip_test, 'TestBuilder::Test::Skip');
