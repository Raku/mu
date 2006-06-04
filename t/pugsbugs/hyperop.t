#!/usr/bin/pugs

use v6;
use Test;
plan 1;

my @a = 1..5;
is(try { [+] (@a »++ ) }, 20, "»(whatever) unimplemented", :todo<bug>);
