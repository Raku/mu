#=interp1.pl - Funktionsaufruf in Perl 5 (und 6?)
sub hallo {
	"Hallo, $_[0]!" # geht derzeit mit pugs
}

print "Otto sagt: ", hallo('Hund'), "\n";
