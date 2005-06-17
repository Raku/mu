#!/usr/bin/pugs

use v6;
use Test;

=pod

We don't have references yet, but that doesn't mean dereferencing should
be a nop.

=cut

plan 1;
my $x = 42;
dies_ok { $$$$$$$$$$x }, "can't endlessly dereference"

# (currently this evaluates to 42)
