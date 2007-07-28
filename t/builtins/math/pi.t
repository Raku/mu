use v6-alpha;
use Test;
plan 6;

# L<S29/"The :Trig tag">

=head1 DESCRIPTION

Basic tests for trigonometric functions.

=cut

sub approx(Num $a, Num $b) {
  my $EPSILON = 0.0001;
  ($EPSILON > abs($a - $b));
}

# See also: L<"http://theory.cs.iitm.ernet.in/~arvindn/pi/"> :)
my $PI = 3.14159265358979323846264338327950288419716939937510;

# -- pi
ok(approx(pi      , $PI),   "pi, as a bareword");
ok(approx(pi()    , $PI),   "pi, as a sub");
ok(approx(3 + pi(), $PI+3), "3+pi(), as a sub");
ok(approx(pi() + 3, $PI+3), "pi()+3, as a sub");
ok(approx(3 + pi,   $PI+3), "3+pi, as a barword");
ok(approx(pi + 3,   $PI+3), "pi+3, as a barword");
