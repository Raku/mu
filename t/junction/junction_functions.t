#!/usr/bin/pugs

use v6;
require Test;

plan 23;

=pod

More Junction Tests

These tests are derived from the Perl6 and Parrot Essentials Chapter 4, page 42

=cut

my $j = (1 | 2 | 3);
is(ref($j), 'Junction', '$j is a Junc');

is($j.perl, '\(1 | 2 | 3)', 'got the right stringified junction');

my @values = $j.values;
is(+@values, 3, 'our junction has three values in it');
is(@values[0], 1, 'our junctions first value is 1');
is(@values[1], 2, 'our junctions second value is 2');
is(@values[2], 3, 'our junctions third value is 3');

my $sums = $j + 3;

is(ref($sums), 'Junction', '$sums is a Junc');

my @sums_values = sort $sums.values;
is(+@sums_values, 3, 'our junction has three values in it');
is(@sums_values[0], 4, 'our junctions first value is 4');
is(@sums_values[1], 5, 'our junctions second value is 5');
is(@sums_values[2], 6, 'our junctions third value is 6');

# loop enough to go through it twice
for (1 .. 6) {
    ok((1 ^ 2 ^ 3) == $j.pick, 'it is always at least one');
    ok((1 | 2 | 3) == $j.pick, 'it is always one of them');
}
