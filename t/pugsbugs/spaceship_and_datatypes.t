#!/usr/bin/pugs

use v6;
require Test;

plan 6;

=pod

The first 2 bugs die with syntax errors.
The third one dies with the following error:

    No compatible subroutine found: &h

=cut

my %h = ("a" => 1, "b" => 2);
eval_ok('%h{"a"} < %h{"b"}', 'comparing hash values');
eval_ok('%h{"a"} <= %h{"b"}', 'comparing hash values');
eval_is('%h{"a"} <=> %h{"b"}', 1, 'comparing hash values');

=pod

The 3 bugs die with syntax errors.

=cut

my @a = (1, 2);
eval_ok('@a[0] < @a[1]', 'comparing array values');
eval_ok('@a[0] <= @a[1]', 'comparing array values');
eval_is('@a[0] <=> @a[1]', 1, 'comparing array values');