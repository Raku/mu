use v6;

use Test;

=begin pod

Basic test for the ** operator with complex numbers

=end pod

plan 3;

sub approx($a, $b) {
    my $EPSILON = 0.0001;
    ($EPSILON > abs($a - $b));
}

ok(approx(-1, (0 + 1i)**2), "i^2 == -1");
ok(approx(-1, (0.7071067811865476 + -0.7071067811865475i)**4), "sqrt(-i)**4 ==-1" );
ok(approx(0+1i, (-1+0i)**(1/2)) || approx(0-1i, (-1+0i)**(1/2)), "-1**(1/2) == i or -i");

# vim: ft=perl6
