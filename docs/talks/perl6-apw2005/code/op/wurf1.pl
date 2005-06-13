#=Würfelspiele
sub wurf { 1 + int 6 * rand }
#!\pause
#=Perl 5: sechs mal würfeln
my @a; push @a, wurf() for 1..6;
#!\pause
#=Perl 6: sechs mal würfeln
my @a = wurf() xx 6; #!\pause
# Falsch: 6 mal die selbe Zahl
#!\pause\visible<5>{
my @a = {wurf()} xx 6; # Lösung: anonyme Subroutine
#!}