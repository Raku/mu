#!/usr/bin/perl

use Test;
use Commands::Guarded <step verbose>;

plan 2;

verbose = 0;
my $var = 1;

step
  name     => "makeVarZero",
  ensure   => { $var == 0 },
  using    => { $var  = 0 },
  rollback => { $var  = 1 };

is $var, 0;

eval {
  step
    name   => "failForRollback",
    ensure => { 1 == 0 },
    using  => { 1 };
};

is $var, 1;
