#!/usr/bin/pugs

use v6;
use Test;

=pod

We ought to be able to change a value when aliasing into it.

=cut

plan 1;
my %h = 1..4;
lives_ok {
    for %h.values -> $v is rw { $v += 1 }
}, "can't endlessly dereference", :todo<bug>;

# (currently this dies with "Can't modify constant item: VInt 2")

