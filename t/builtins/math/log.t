use v6-alpha;
use Test;
plan 11;

=pod

Basic tests for the log() and log10() builtins

=cut

sub is_approx (Num $is, Num $expected, Str $descr) {
  ok abs($is - $expected) <= 0.00001, $descr;
}

sub approx(Num $a, Num $b) {
  my $EPSILON = 0.0001;
  ($EPSILON > abs($a - $b));
}


# L<S29/Num/"=item log">

is_approx(log(5), 1.6094379124341003, 'got the log of 5');
is_approx(log(0.1), -2.3025850929940455, 'got the log of 0.1');

# L<S29/Num/"=item log10">

is_approx(log10(5), 0.6989700043360187, 'got the log10 of 5');
is_approx(log10(0.1), -0.9999999999999998, 'got the log10 of 0.1');

# please add tests for complex numbers
#
# The closest I could find to documentation is here: http://tinyurl.com/27pj7c
# I use 1i instead of i since I don't know if a bare i will be supported
 
# log(exp(i pi)) = i pi log(exp(1)) = i pi
ok(approx(log(-1,), 0 + 1i * pi), "got the log of -1", :todo<feature>);
ok(approx(log10(-1), 0 + 1i * pi), "got the log10 of -1", :todo<feature>);

# log(exp(1+i pi)) = 1 + i pi
ok(approx(log(-exp(1)), 1 + 1i * pi), "got the log of -e", :todo<feature>);
ok(approx(log10(-10), 1 + 1i * pi), "got the log10 of -10", :todo<feature>);

ok(approx(log((1+1i) / sqrt(2)), 1 + 1i * pi / 4), "got log of exp(i pi/4)", :todo<feature>);
ok(approx(log(1i), 1i * pi / 2), "got the log of i (complex unit)", :todo<feature>);
ok(approx(log(-1i), 1i * pi * 1.5), "got the log of -i (complex unit)", :todo<feature>);

# TODO: please add more testcases for log10 of complex numbers
