#=Code-Objekt
my $code = sub {say "code!"};
$code(); # gibt "code!" aus, Klammern sind nötig!
#=Anwendung von Code-Objekten: for-Schleifen
for 1..10 -> {
	say
}
