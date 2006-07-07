use v6-alpha;

use Test;

plan 1;

is "abc{ BEGIN { 3 } }def", "abc3def", 'BEGIN {} works in double quoted strings';
