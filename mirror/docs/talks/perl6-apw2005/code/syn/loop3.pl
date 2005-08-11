#=loop3.pl - Perl 5 Version
use strict;
my $i = 1;
for(;;) {
	print $i++, "\n";
	last if $i > 10;
}
