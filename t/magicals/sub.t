#!/usr/bin/pugs

use v6;
require Test;

plan 2;

sub factorial { @_[0] < 2 ?? 1 :: @_[0] * &?SUB(@_[0] - 1) }

my $result1 = factorial(3);
is($result1, 6, 'the &?SUB magical works correctly');

my $factorial = sub { @_[0] < 2 ?? 1 :: @_[0] * &?SUB(@_[0] - 1) }

my $result2 = $factorial(3);
is($result2, 6, 'the &?SUB magical works correctly in anon-subs');
