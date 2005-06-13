#=In Perl 5 könnte das etwas so aussehen:
if (wurf() == 6 || wurf() == 6 || wurf() == 6) {
	...
}
#=In Perl 6 geht das eleganter:
if any(wurf(), wurf(), wurf()) == 6 {
	... # aber: wurf() wird immer genau drei mal aufgerufen
}
