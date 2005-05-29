#!/usr/bin/pugs

use v6;
use Test;

plan 4;

# L<S04/"Exceptions" /The fail function/>

{
  # "use fatal" is not standard, so we don't have to disable it here
  my $was_after_fail  = 0;
  my $was_before_fail = 0;
  my $sub = sub { $was_before_fail++; my $exception = fail_ 42; $was_after_fail++ };

  my $unthrown_exception = $sub();
  # Note: We don't further access $unthrown_exception, so it doesn't get thrown
  is $was_before_fail, 1, "fail() doesn't cause our sub to not get executed";
  is $was_after_fail,  0, "fail() causes our sub to return (1)";
}

{
  # Explicitly "use fatal"
  # use fatal; -- Commented as there's no fatal.pm yet.
  # Instead, we set the magical variable $?FAIL_SHOULD_DIE to a true value.
  my $?FAIL_SHOULD_DIE = 1;
  my $was_after_fail = 0;
  my $was_after_sub  = 0;
  my $sub = sub { fail_ 42; $was_after_fail++ };

  try { $sub(); $was_after_sub++ };

  is $was_after_fail, 0, "fail() causes our sub to return (2)";
  is $was_after_sub,  0, "fail() causes our try{} to die";
}
