#=while3.pl - while-Schleife in Perl 5
use strict;
my $i = 1;
do { # keine echte Schleife...
	print "$i\n"; $i++;
	# last if $i==3; # ... würde nicht gehen!
} while($i <= 10);
