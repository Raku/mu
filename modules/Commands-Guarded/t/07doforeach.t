#!/usr/bin/perl

use Test;
use Commands::Guarded <step verbose>;

my @nums = (1..10);
plan +@nums;

verbose = 0;

my $step = step
  name   => "doForeachTest",
  ensure => { @nums[$^i] % 2 },
  using  => { @nums[$^i]++ };

$step.do_foreach(0..@nums - 1);

for @nums {
  ok $^num % 2;
}
