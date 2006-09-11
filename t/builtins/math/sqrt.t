use v6-alpha;
use Test;
plan 2;

# L<S29/"Math::Basic"/"=item sqrt">

=pod 

Basic tests for the sqrt() builtin

=cut

sub is_approx (Num $is, Num $expected, Str $descr) {
  ok abs($is - $expected) <= 0.00001, $descr;
}

is_approx(sqrt(2), 1.4142135623730951, 'got the square root of 2');
is_approx(sqrt(5), 2.23606797749979,   'got the square root of 5');
