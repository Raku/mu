#!/usr/bin/pugs

use v6;
require Test;

=kwid

while statement tests

L<S04/"Loop statements">

=cut

plan 6;

my $i = 0;
while $i < 5 { $i++; };
is($i, 5, 'while $i < 5 {} works');

my $i = 0;
while 5 > $i { $i++; };
is($i, 5, 'while 5 > $i {} works');

# with parens

my $i = 0;
while ($i < 5) { $i++; };
is($i, 5, 'while ($i < 5) {} works');

my $i = 0;
while (5 > $i) { $i++; };
is($i, 5, 'while (5 > $i) {} works');

# single value
my $j = 0;
while 0 { $j++; };
is($j, 0, 'while 0 {...} works');

my $k = 0;
while $k { $k++; };
is($k, 0, 'while $var {...} works');

