#!/usr/bin/pugs

use v6;
require Test;

plan 4;

# Again, mostly stolen from Perl 5

my $a = 'ab' ~ 'c';
is($a, 'abc', '~ two literals correctly');

my $b = 'def';

my $c = $a ~ $b;
is($c, 'abcdef', '~ two variables correctly');

eval '$c ~= "xyz"';
todo_is($c, 'abcdefxyz', '~= a literal string correctly');

my $d = $a;
eval '$d ~= $b';
todo_is($d, 'abcdef', '~= variable correctly');
