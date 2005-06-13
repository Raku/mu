#=boolean-Kontext: '?' liefert true oder false
my $var = 0;
say "ist 0" if $var == 0;
say "ist wahr" if ?$var;
say "'Hallo' ist true" if ?"Hallo";

#=String-Kontext: ~
say "4 ist " ~ 4;

#=Zahlen-Kontext: +
say 1 + +"3";
