#!perl6

use v6;

# Towers of Hanoi
# 	move from tower A to tower B using tower S as the spare
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
