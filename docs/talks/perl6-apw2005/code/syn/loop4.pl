#=loop4.pl - Perl 6 Version
use v6;
my $i = 1;
loop {
	say $i++;
	last if $i > 10;
}
