#!/usr/bin/pug
use v6;
use Test;

my @a = 1..5;
is(try { [+] (@a »++ ) }, 20, "»(whatever) unimplemented", :todo<bug>);
