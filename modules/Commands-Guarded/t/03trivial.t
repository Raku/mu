#!/usr/bin/perl

use Test;
use Commands::Guarded <step verbose>;

plan 1;

verbose = 0;

my $var = 0;

step
  name   => "trivial",
  ensure => { $var == 1 },
  using  => { $var  = 1 };

is $var, 1;
