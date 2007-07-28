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

# See also: L<"http://en.wikipedia.org/wiki/E_%28mathematical_constant%29"> :)
my $e = 2.71828182845904523536;

# -- pi
ok(approx(e      , $e),   "pi, as a bareword");
ok(approx(e()    , $e),   "pi, as a sub");
ok(approx(1 + e(), $e+1), "1+pi(), as a sub");
ok(approx(e() + 1, $e+1), "pi()+1, as a sub");
ok(approx(1 + e,   $e+1), "1+pi, as a bareword");
ok(approx(e + 1,   $e+1), "pi+1, as a bareword");
