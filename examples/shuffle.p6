#!perl6
use v6;

sub fisher_yates_shuffle (@deck) {
   my @copy = @deck;
   my $i = +@copy;
   while ($i--) {
      my $j = int(rand($i+1));
      @copy[[$i,$j]] = @copy[[$j,$i]];
   }
   return @copy;
}

my @nums = 1..50;
my @shuf = fisher_yates_shuffle(@nums);

my $i;
loop ($i = 0; $i < +@nums; $i++) { say @nums[$i] ~ "\t" ~ @shuf[$i] }

