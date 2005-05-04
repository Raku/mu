#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# state() inside coderefs
{
  my $gen = {
    state $svar = 42;
    my $ret = { $svar++ };
  };

  my $a = $gen(); # $svar == 42
  $a(); $a();     # $svar == 44
  my $b = $gen(); # $svar == 44

  is $b(), 44, "state() works inside coderefs";
}

# state() inside subs
{
  sub gen {
    state $svar = 42;
    my $ret = { $svar++ };
  };

  my $a = gen(); # $svar == 42
  $a(); $a();    # $svar == 44
  my $b = gen(); # $svar == 44

  is $b(), 44, "state() works inside subs";
}

# state() inside for-loops
{
  for [1,2,3] -> $val {
    state $svar = 42;
    $svar++;

    # Only check on last run
    if($val == 3) {
      is $svar, 45, "state() works inside for-loops";
    }
  }
}
