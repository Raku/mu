#!/usr/bin/pugs

use v6;
use Test;

=kwid

The sub doesn't return anything

=cut

plan 1;

sub foo { return (42, 1) }
my $bar = ~foo();
is($bar, '42 1', 'Should not return empty string');
