#!/usr/bin/pugs
# This is a *evil* example of random operator overloading.

use v6;

my $op;
BEGIN {
  # At compilation time, we randomly pick a op to be overloaded.
  $op = pick <+ - * />;
}

sub infix:{$op} { 42 }

# Now one of +-*/ will return 42, the other ops will continue to work as
# normal.
say "1 + 1 = {1 + 1}";
say "1 * 1 = {1 * 1}";
say "1 / 1 = {1 / 1}";
say "1 - 1 = {1 - 1}";
