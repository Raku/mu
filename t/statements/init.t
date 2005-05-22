#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# INIT {...} blocks in "void" context
{
  my $var;

  my $was_in_init;
  my $was_in_init_as_of_begin_time;
  my $var_as_of_init_time;

  BEGIN { $var = 42; $was_in_init_as_of_begin_time = $was_in_init }
  INIT  { $was_in_init++; $var_as_of_init_time = $var; $var = 13 }

  is $was_in_init,  1, 'our INIT {...} block was executed';
  is $var,         13, 'our INIT {...} block set our variable';
  is $was_in_init_as_of_begin_time // 0, 0, 'our INIT {...} block was not called before runtime';
  is $var_as_of_init_time, 42, 'our INIT {...} block was executed after our BEGIN {...} block';
}

# INIT {...} blocks as rvalues
{
  my $var;
  my $was_in_init;

  BEGIN { $var = 42 }
  my $sub = { my $z = INIT { $was_in_init++; $var }; $z + 1 };

  $var = 23;

  is $sub(), 43, 'our rvalue INIT {...} block returned the correct var (1)';
  is $sub(), 43, 'our rvalue INIT {...} block returned the correct var (2)';
  is $sub(), 43, 'our rvalue INIT {...} block returned the correct var (3)';
  is $was_in_init, 1, 'our rvalue INIT {...} block was executed exactly once';
}
