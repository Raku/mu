#!/usr/bin/pugs

use v6;
require Test;

plan 4;

my $i = 0;
eval 'until $i >= 5 { $i++; }';
is($i, 5, 'until $i >= 5 {} works');

my $i = 0;
eval 'until 5 <= $i { $i++; }';
is($i, 5, 'until 5 <= $i {} works');

# with parens

my $i = 0;
eval 'until ($i >= 5) { $i++; }';
is($i, 5, 'until ($i >= 5) {} works');

my $i = 0;
eval 'until (5 <= $i) { $i++; }';
is($i, 5, 'until (5 <= $i) {} works');