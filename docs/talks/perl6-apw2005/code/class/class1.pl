#=eine einfache Klasse
class Welt {
	method hallo () { "Hallo, Welt!".say; }
}
#!\pause
#=Erzeugen eines Objektes (Instanzierung)
my $welt = "Welt"->new();	# geht in Perl 6 nicht mehr
my $welt = Welt.new();  	# (fast) wie in Perl 5
my $welt = new Welt();  	# wie in Perl 5
my Welt $welt = new Welt(); 	# neu: Typen-Deklaration
#!\pause
#=Aufruf einer einfachen Methode
$welt.hallo;	# Perl 5:  $welt->hello;
hallo $welt;	# wie in Perl 5
hallo($welt);	# wie in Perl 5
