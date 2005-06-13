#=otto2.pl - Perl 6
use v6;
my $wer = "Otto";
my $haustier = "Hund";
my @obst = qw(Äpfel Bananen);
my %lieblings = (
	Farbe => "blau",
	Drink => "Martini"
);

print "$wer hat einen $haustier\n";
print "$wer mag @obst[0] und @obst[1]\n";
print "{$wer}s Lieblingsfarbe ist %lieblings{'Farbe'}\n";
print "{$wer}s Lieblingsdrink ist %lieblings{'Drink'}\n";
