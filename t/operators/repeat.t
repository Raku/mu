#!/usr/bin/pugs

use v6;
use Test;

=kwid

Repeat operators for strings and lists

=cut

plan 13;

is ('a' x 3, 'aaa', 'string repeat operator works on single character');
is ('ab' x 4, 'abababab', 'string repeat operator works on multiple character');
is (1 x 5, '11111', 'number repeat operator works on number and creates string');
is ('' x 6, '', 'repeating an empty string creates an empty string');

my @foo = 'x' xx 10;
is (@foo[0], 'x', 'list repeat operator created correct list');
is (@foo[9], 'x', 'list repeat operator created correct list');
is (+@foo, 10, 'list repeat operator created list of the right size');

# test x=
my $twin = 'Lintilla';
ok(eval '$twin x= 2;', 'operator x= for string works');
is ($twin, 'LintillaLintilla', 'operator x= for string repeats correct');

my @array = (4, 2);
ok(eval '@array xx= 2;', 'operator xx= for list works');
is (@array[0], 4, 'operator xx= for list repeats correct');
is (@array[3], 2, 'operator xx= for list repeats correct');
is (+@array, 4, 'operator xx= for list created the right size');
