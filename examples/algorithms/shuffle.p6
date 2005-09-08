#!/usr/bin/pugs
use v6;

sub fisher_yates_shuffle (@copy is copy) returns Array {
   for  0..@copy-1  -> $i {
      my $j = rand($i+1).int;
      @copy[$i,$j] = @copy[$j,$i];
   }
   return @copy;
}

sub compare (@a,@b) returns Str{
    state $compared; 
    for 0..(@a >= @b ?? @a-1 !! @b-1) -> $i{
        $compared ~= "@a[$i] \t @b[$i]\n"; 
    }
    $compared;    
}

my @nums = 1..50;
my @shuf = fisher_yates_shuffle @nums ;
say compare @nums,@shuf;

