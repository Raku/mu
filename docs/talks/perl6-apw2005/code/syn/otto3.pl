#=otto3.pl - Perl 6
use v6;
my $wer = "Otto";
my $haustier = "Hund";
my @obst = qw(Äpfel Bananen);
my %lieblings = (
	Farbe => "blau",
	Drink => "Martini"
);

say "$wer hat einen $haustier";
say "$wer mag @obst[0] und @obst[1]";
say "{$wer}s Lieblingsfarbe ist %lieblings{'Farbe'}";
say "{$wer}s Lieblingsdrink ist %lieblings{'Drink'}";
