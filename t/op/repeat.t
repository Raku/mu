#!/usr/bin/pugs

use v6;
require Test;

=kwid

Repeat operators for strings and lists

=cut

plan(7);

is ('a' x 3, 'aaa', 'string repeat operator works on single character');
is ('ab' x 4, 'abababab', 'string repeat operator works on multiple character');
is (1 x 5, '11111', 'number repeat operator works on number and creates string');
is ('' x 6, '', 'repeating an empty string creates an empty string');

my @foo = 'x' xx 10;
is (@foo[0], 'x', 'list repeat operator created correct list');
is (@foo[9], 'x', 'list repeat operator created correct list');
is (+@foo, 10, 'list repeat operator created list of the right size');
