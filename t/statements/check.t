#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# CHECK {...} block in "void" context
{
  my $var;

  my $was_in_check;
  my $was_in_begin;
  my $was_in_check_at_begin_time;
  my $was_in_begin_at_check_time;
  my $var_as_of_check_time;

  $var = 19;

  CHECK {
    $var_as_of_check_time = $var;
    $was_in_check++;
    $was_in_begin_at_check_time = $was_in_begin;
  }
  BEGIN {
    $var = 42;
    $was_in_begin++;
    $was_in_check_at_begin_time = $was_in_check;
  }

  is $var_as_of_check_time, 42, 'our CHECK {...} block was executed after our BEGIN {...} block';
  is $was_in_check, 1, 'our CHECK {...} block was executed';
  is $was_in_begin_at_check_time, 1, 'our BEGIN {...} block was executed before our CHECK {...} block was (1)';
  is $was_in_check_at_begin_time // 0, 0, 'our BEGIN {...} block was executed before our CHECK {...} block was (2)';
}

# CHECK {...} blocks as rvalues
{
  my $var;
  my $was_in_check;

  BEGIN { $var = 42 }
  $var = 19;
  my $a = { CHECK { $was_in_check++; $var } };

  is $a(), 42, 'our CHECK {...} block returned the correct var (1)';
  is $a(), 42, 'our CHECK {...} block returned the correct var (2)';
  is $a(), 42, 'our CHECK {...} block returned the correct var (3)';
  is $was_in_check, 1, 'our rvalue CHECK {...} block was executed exactly once';
}
