#!perl6
use v6;

# vim:ft=perl:sw=4:
# Print the Mandlebrot set
#
# Translated from C code by Glenn Rhoads
# to perl6 by Leopold Toetsch <lt@toetsch.at>
#
# The C code is:
#
# main(){
#
#  int x, y, k;
#  char *b = " .:,;!/>)|&IH%*#";
#  float r, i, z, Z, t, c, C;
#  for (y=30; puts(""), C = y*0.1 - 1.5, y--;){
#     for (x=0; c = x*0.04 - 2, z=0, Z=0, x++ < 75;){
#        for (r=c, i=C, k=0; t = z*z - Z*Z + r, Z = 2*z*Z + i, z=t, k<112; k++)
#           if (z*z + Z*Z > 10) break;
#        printf (b[k%16]);
#        }
#     }
# }
#

my ($x, $y, $k);
# no substr now
my @b = (' ', '.', ':', ',', ';', '!', '/', '>', ')', '|', '&', 'I', 'H', '%', '*', '#');

my ($r, $i, $z, $Z, $t, $c, $C);
loop ($y=30; $C = $y*0.1 - 1.5;) {
    last() if $y-- < 0;
    loop ($x=0; $c = $x*0.04 - 2.0, $z=0.0, $Z=0.0;) {
        last() if $x++ > 75;
        loop ($r=$c, $i=$C, $k=0; $t = $z*$z - $Z*$Z + $r, $Z = 2.0*$z*$Z + $i, $z=$t; $k++) {
            last() if $k > 12 or $z*$z + $Z*$Z > 10.0;
        }
        print @b[ $k % 12 ];
    }
    print "\n";
}
