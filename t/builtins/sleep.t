#!/usr/bin/pugs
use v6;
require Test;

plan 4;

my $start = time();
my $sleep_says = sleep 3;
my $diff = time() - $start;

sub n_ge(Int $a, Int $b) { ?($a >= $b) };
sub n_le(Int $a, Int $b) { ?($a <= $b) };

cmp_ok( $sleep_says, \&n_ge, 2,  'Sleep says it slept at least 2 seconds' );
cmp_ok( $sleep_says, \&n_le, 10, '... and no more than 10' );

cmp_ok( $diff, \&n_ge, 2,  'Actual time diff is at least 2 seconds' );
cmp_ok( $diff, \&n_le, 10, '... and no more than 10' );