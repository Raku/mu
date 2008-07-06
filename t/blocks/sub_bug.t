use v6;

use Test;

=begin pod

In an ancient version of pugs the sub below didn't return anything

TODO: merge into one of the return.t files

=end pod

plan 1;

sub foo { return (42, 1) }
my $bar = ~foo();
is($bar, '42 1', 'Should not return empty string');

# vim: ft=perl6
