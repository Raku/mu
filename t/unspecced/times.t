#!/usr/bin/pugs

use v6;
use Test;

plan 1;

lives_ok {
  my @ret = times();
}
