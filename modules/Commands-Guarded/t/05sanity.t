#!/usr/bin/perl

use Test;
use Commands::Guarded <step verbose>;

plan 3;

verbose = 0;

my $var = 0;

eval {
  step
    name   => "shouldFail",
    ensure => { $var == 0 },
    using  => { $var  = 0 },
    sanity => { $var == 1 };
};

ok $@;

$var = 1;

eval {
  step
    name   => "shouldNotFail",
    ensure => { $var == 0 },
    using  => { $var  = 0 },
    sanity => { 1    == 1 };
};

ok not $@;

$var = 0;

eval {
  step
    name   => "shouldFailOnSecondTry",
    ensure => { $var == 1 },
    using  => { $var  = 1 },
    sanity => { $var == 0 };
};

ok $@;
