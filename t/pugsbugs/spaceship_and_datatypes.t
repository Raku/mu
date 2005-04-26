#!/usr/bin/pugs

use v6;
use Test;

plan 6;

=pod

The first 2 bugs die with syntax errors.
The third one dies with the following error:

    No compatible subroutine found: &h

=cut

my %h = ("a" => 1, "b" => 2);
ok(%h{"a"} < %h{"b"}, 'comparing hash values');
ok(%h{"a"} <= %h{"b"}, 'comparing hash values');
is(%h{"a"} <=> %h{"b"}, -1, 'comparing hash values');

=pod

The 3 bugs die with syntax errors.

=cut

my @a = (1, 2);
ok(@a[0] < @a[1], 'comparing array values');
ok(@a[0] <= @a[1], 'comparing array values');
is(@a[0] <=> @a[1], -1, 'comparing array values');
