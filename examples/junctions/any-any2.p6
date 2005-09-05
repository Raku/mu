#!/usr/bin/pugs

use v6;

# Please remember to update t/examples/examples.t and rename
# examples/output/junctions/any-any2 if you rename/move this file.

# any compared with any - example 2

my @first_set = qw(1 1);
my @new_set = qw(1 1.4 1 1 1 1 1 1 8);

if ( (abs(any(@first_set) - any(@new_set))) > 0.5) {
  "a variation in the readings is too large".say;
}
