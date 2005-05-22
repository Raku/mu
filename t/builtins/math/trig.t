#!/usr/bin/pugs

use v6;
use Test;

plan 45;
# force_todo 2 .. 8, 12 .. 20;

=head1 DESCRIPTION

Basic tests for trigonometric functions.

=cut

sub approx(Num $a, Num $b) {
  my $EPSILON = 0.0001;
  ($EPSILON > abs($a - $b));
}

# See also: L<http://theory.cs.iitm.ernet.in/~arvindn/pi/> :)
my $PI = 3.14159265358979323846264338327950288419716939937510;

# L<S29/"Math::Trig" /"pi"/> -- pi
ok(approx(pi, $PI), "pi()");
is eval("approx(pi + 3, $PI + 3)"), "'pi() + 3' may drop its parentheses before +3";

# L<S29/"Math::Trig" /"atan"/> -- atan
# The basic form of atan (one argument) returns a value in ]-pi, pi[.
# Quadrants I, III
ok(approx(atan(1)           / $PI * 180, 45));
ok(approx(atan(1/3*sqrt(3)) / $PI * 180, 30));
ok(approx(atan(sqrt(3))     / $PI * 180, 60));

# Quadrants II, IV
ok(approx(atan(-1)           / $PI * 180, -45));
ok(approx(atan(-1/3*sqrt(3)) / $PI * 180, -30));
ok(approx(atan(-sqrt(3))     / $PI * 180, -60));

# S29: This second form of C<atan> computes the arctangent of $y/$x, and
# **takes the quadrant into account**.
# Quadrant I
ok(approx(atan(1, 1)           / $PI * 180, 45));
ok(approx(atan(1, sqrt(3))     / $PI * 180, 30));
ok(approx(atan(1, 1/3*sqrt(3)) / $PI * 180, 60));

# Quadrant II
ok(approx(atan(1, -1)           / $PI * 180, 135));
ok(approx(atan(1, -1/3*sqrt(3)) / $PI * 180, 120));
ok(approx(atan(1, -sqrt(3))     / $PI * 180, 150));

# Quadrant III
ok(approx(atan(-1, -1)           / $PI * 180 + 360, 225));
ok(approx(atan(-1, -sqrt(3))     / $PI * 180 + 360, 210));
ok(approx(atan(-1, -1/3*sqrt(3)) / $PI * 180 + 360, 240));

# Quadrant IV
ok(approx(atan(-1, 1)           / $PI * 180 + 360, 315));
ok(approx(atan(-1, sqrt(3))     / $PI * 180 + 360, 330));
ok(approx(atan(-1, 1/3*sqrt(3)) / $PI * 180 + 360, 300));

# L<S29/"Math::Trig"> -- sin, cos, tan
# sin
ok(approx(sin(0/4*$PI), 0));
ok(approx(sin(1/4*$PI), 1/2*sqrt(2)));
ok(approx(sin(2/4*$PI), 1));
ok(approx(sin(3/4*$PI), 1/2*sqrt(2)));
ok(approx(sin(4/4*$PI), 0));
ok(approx(sin(5/4*$PI), -1/2*sqrt(2)));
ok(approx(sin(6/4*$PI), -1));
ok(approx(sin(7/4*$PI), -1/2*sqrt(2)));
ok(approx(sin(8/4*$PI), 0));

# cos
ok(approx(cos(0/4*$PI), 1));
ok(approx(cos(1/4*$PI), 1/2*sqrt(2)));
ok(approx(cos(2/4*$PI), 0));
ok(approx(cos(3/4*$PI), -1/2*sqrt(2)));
ok(approx(cos(4/4*$PI), -1));
ok(approx(cos(5/4*$PI), -1/2*sqrt(2)));
ok(approx(cos(6/4*$PI), 0));
ok(approx(cos(7/4*$PI), 1/2*sqrt(2)));
ok(approx(cos(8/4*$PI), 1));

# tan
ok(approx(tan(0/4*$PI), 0));
ok(approx(tan(1/4*$PI), 1));
ok(approx(tan(3/4*$PI), -1));
ok(approx(tan(4/4*$PI), 0));
ok(approx(tan(5/4*$PI), 1));
ok(approx(tan(7/4*$PI), -1));
ok(approx(tan(8/4*$PI), 0));
