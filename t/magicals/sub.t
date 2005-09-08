#!/usr/bin/pugs

use v6;
use Test;

=pod

This tests the &?SUB magical value

L<S06/"The &?SUB routine">

=cut

plan 3;

# L<S06/"The &?SUB routine">
# L<S02/"Names" /Which sub am I in/>
sub factorial { @_[0] < 2 ?? 1 !! @_[0] * &?SUB(@_[0] - 1) }

my $result1 = factorial(3);
is($result1, 6, 'the &?SUB magical works correctly');

my $factorial = sub { @_[0] < 2 ?? 1 !! @_[0] * &?SUB(@_[0] - 1) };
my $result2 = $factorial(3);
is($result2, 6, 'the &?SUB magical works correctly in anon-subs');

my $baz = try { &?SUB };
ok(!defined($baz), '&?SUB not defined outside of a sub');
