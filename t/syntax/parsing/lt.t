use v6-alpha;

use Test;

plan 4;

# infix less-than requires whitespace

eval_ok("1 <2", "S03: Changes to Perl 5 operators: infix less-than (<) requires whitespace before.");
eval_ok("1 < 2", "S03: Changes to Perl 5 operators: infix less-than (<) requires whitespace before.");
eval_dies_ok("1< 2", "S03: Changes to Perl 5 operators: infix less-than (<) requires whitespace before, so this is a parse error.");
eval_dies_ok("1<2", "S03: Changes to Perl 5 operators: infix less-than (<) requires whitespace before, so this is a parse error.");
