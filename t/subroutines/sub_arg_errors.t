#!/usr/bin/pugs

use v6;
reqiure Test;

plan 1;

=pod

These are misc. sub argument errors.

=cut

dies_ok { sub foo (*$x) { say $x } foo(reverse(1,2)) }, 'slurpy args are now bounded';