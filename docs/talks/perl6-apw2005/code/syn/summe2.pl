#=summe2.pl - Unicode in Perl 6
use v6;
sub Σ {
	my $i = 0;
	$i += $_ for @_;
	return $i;
}
say Σ 1..10; # gibt 55 aus
