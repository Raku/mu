#!/usr/bin/perl

use Test;
use Commands::Guarded <step verbose>;

plan 1;

verbose = 0;

my $var = 1;

step
  name   => "nullOp",
  ensure => { $var == 1 },
  using  => {
    $var = 0 # shouldn't run this
  };

is $var, 1;
