#=state2.pl - Perl 6: Anwendung von state
sub singleton {
	state $var = new MeineKlasse;
	# $var wird nur einmal erzeugt
	return $var;
}
