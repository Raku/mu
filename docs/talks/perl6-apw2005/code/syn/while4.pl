#=while4.pl - while-Schleife in Perl 6
use v6;
my $i = 1;
repeat { # dies ist eine echte Schleife, daher...
	say $i; $i++;
	# last if $i==3; # ... würde das gehen!
} while $i <= 10;
