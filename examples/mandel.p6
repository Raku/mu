#!/usr/bin/pugs
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

### This is badly hacked to get Parrot compiler going ###
### Should be fixed in a couple days ###

my ($x, $y, $k);
my $b = ' .:,;!/>)|&IH%*#';

my ($P, $Q, $X, $L);
my ($r, $i, $z, $Z, $t, $c, $C);
loop ($L = 0;; $L++) {
    last() if $L > 0;
    loop ($y=30; $P = $y * 0.1, $C = $P - 1.5;) {
        last() if $y < 0;
        $y--;
        loop ($x=0; $P = $x * 0.04, $c = $P - 2, $z=0.0, $Z=0.0;) {
            last() if $x > 75;
            $x++;
            loop (
                $r=$c, $i=$C, $k=0;
                $P = $z*$z, $Q = $Z*$Z, $P = $P-$Q, $t = $P+$r,
                $P = $z*2.0,$P = $P*$Z, $Z = $P + $i,
                $z=$t; $k++
            ) {
                last() if $k > 12;
                $P = $z * $z;
                $Q = $Z * $Z;
                $P = $P + $Q;
                last() if $P > 10.0;
            }
            $P = $k % 12;
            $X = substr($b, $P, 1);
            print $X;
        }
        say '';
    }
}
