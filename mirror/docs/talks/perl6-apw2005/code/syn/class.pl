#=class1.pl - Klassen in Perl 6
use v6;

class MeineKlasse {
	method grüße { say "Hallo :-)" }
}

my $var = new MeineKlasse;
$var.grüße;
