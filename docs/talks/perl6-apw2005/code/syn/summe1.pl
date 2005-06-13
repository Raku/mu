#=summe1.pl - Unicode wird in Perl 5
use utf8;
sub Σ {
	my $i = 0;
	$i += $_ for @_;
	return $i;
}
print Σ(1..10), "\n"; # gibt 55 aus
