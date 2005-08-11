#!/usr/bin/pugs

use v6;
use Test;

plan 11;

{
  my $var;
  my $was_in_first = 0;
  my $sub = { FIRST { $was_in_first++; $var = rand } };

  $sub();
  is $was_in_first, 1, 'our FIRST {} block was invoked';
  my $orig_var = $var;

  $sub();
  is $was_in_first, 1, 'our FIRST {} block was invoked only once...';
  is $var, $orig_var, "...and our var wasn't changed";
}

{
  my $sub = {
    my $var = 42;
    FIRST { $var = 23 };
    $var;
  };

  is $sub(), 23, 'our FIRST {} block set our variable (1)';
  is $sub(), 42, 'our FIRST {} wasn\'t invoked again (1-1)';
  is $sub(), 42, 'our FIRST {} wasn\'t invoked again (1-2)';
}

{
  my $was_in_first;
  my $sub = {
    my $var = FIRST { $was_in_first++; 23 };
    $var //= 42;
    $var;
  };

  is $sub(), 23, 'our FIRST {} block set our variable (2)';
  is $sub(), 23, 'our FIRST {} wasn\'t invoked again (2-1)';
  is $sub(), 23, 'our FIRST {} wasn\'t invoked again (2-2)';
  is $was_in_first, 1, 'our FIRST {} block was invoked exactly once';
}

# Test that FIRST {} blocks are executed only once even if they return undef
# (the first implementation ran than twice instead).
{
  my $was_in_first;
  my $sub = { FIRST { $was_in_first++; undef } };

  $sub();
  $sub();
  $sub();
  is $was_in_first, 1, 'our FIRST { ...; undef } block was invoked exactly once';
}
