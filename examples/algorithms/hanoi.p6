#!/usr/bin/pugs

use v6;

# Towers of Hanoi

# Please remember to update t/examples/examples.t and rename
# examples/output/algorithms/hanoi if you rename/move this file.

# move from tower A to tower B using tower S as the spare
sub hanoi ($a,$b,$s,$d) {
   if $d > 0 {
      hanoi($a,$s,$b,$d-1); 
      say $a, $b;
      hanoi($s,$b,$a,$d-1);
   }
}


my $ndisks := @*ARGS[0] // 3;
say "ndisks = " ~ $ndisks;
hanoi('A','B','S',$ndisks);
