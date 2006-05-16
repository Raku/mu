#!/usr/bin/pugs

use v6;
use Test;

plan 2;

lives_ok {
  &New::Package::foo;
  # this is ok, as you don't have to predeclare globally qualified variables
}, "using an undeclared globabbly qualified code variable in void context is ok";

dies_ok {
  &New::Package::foo();
}, "...but invoking it should die";
