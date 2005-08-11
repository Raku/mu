#=state1.pl - "state" in Perl 5
{
	my $var = new MeineKlasse;
	# $var wird nur einmal erzeugt
	sub singleton {
		return $var;
	}
}
