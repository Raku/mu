#!/usr/bin/pugs

use v6;

# Please remember to update t/examples/examples.t and rename
# examples/output/junctions/1 if you rename/move this file.

my @color = qw(red green blue);

my $x = any @color;

my $y = 'blue';

my $result = ($x eq $y) ?? "acceptable" !! 'unacceptable' ;

print "$result color\n";

  
