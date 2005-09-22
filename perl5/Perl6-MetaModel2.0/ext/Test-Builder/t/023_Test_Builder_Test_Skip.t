#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 14;

require "lib/TestBuilder.pm";

my $skip_test = $::TestBuilder->FETCH('::Test')->FETCH('::Skip')->new(
    '$.number'      => 1,     
    '$.passed'      => 1,
    '$.description' => 'first test description',
    '$.reason'      => 'reason for skipping'
);

isa_ok($skip_test, 'TestBuilder::Test::Skip');

is($skip_test->number(), 1, 'number() should return the test number' );
ok($skip_test->passed(), 'passed() should return the passed value' );
is($skip_test->description(), 'first test description',
   'description() should return the test description' );
is($skip_test->diagnostic(), '???',
   'diagnostic() should return the default diagnostic if needed' );

my $skip_diag = $::TestBuilder->FETCH('::Test')->FETCH('::Skip')->new(
    '$.number'      => 1,     
    '$.passed'      => 1,
    '$.reason'      => 'reason for skipping',
    '$.diagnostic'  => 'some diagnostic message',
    '$.description' => 'first test description',
);
isa_ok($skip_diag, 'TestBuilder::Test::Skip');

is( $skip_diag->diagnostic(), 'some diagnostic message',
    '... but should return diagnostic if set' );

is( $skip_test->reason(), 'reason for skipping',
    'reason() should return the test reason' );

is( $skip_test->report(), 'not ok 1 #skip reason for skipping',
    'report() should return a TAP-formatted skip message' );

my %status = %{$skip_test->status()};

is(scalar keys %status, 4, 'status() should return a hash');
is($status{passed},   1, '... with a passed key set to true');
is($status{skip},     1, '... a skip key set to true'                 );
is($status{reason},      'reason for skipping', '... the skip reason' );
is($status{description}, 'first test description',
    '... and the correct test description');
