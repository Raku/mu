use v6;

use Test;

=begin pod

Parse/eval error :

Pugs enters an infinite loop on:

  say +(any(1).values)

This test must be disabled for releases, or
smoke testing.

=end pod

plan 1;

my $count = +(any(1).values);
is( $count, 1, "The test does not enter an infinite loop" );
