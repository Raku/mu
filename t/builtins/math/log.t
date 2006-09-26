use v6-alpha;
use Test;
plan 4;

=pod

Basic tests for the log() and log10() builtins

=cut

sub is_approx (Num $is, Num $expected, Str $descr) {
  ok abs($is - $expected) <= 0.00001, $descr;
}

# L<S29/Num/"=item log">

is_approx(log(5), 1.6094379124341003, 'got the log of 5');
is_approx(log(0.1), -2.3025850929940455, 'got the log of 0.1');

# L<S29/Num/"=item log10">

is_approx(log10(5), 0.6989700043360187, 'got the log10 of 5');
is_approx(log10(0.1), -0.9999999999999998, 'got the log10 of 0.1');

# please add tests for complex numbers
