use v6-alpha;

use Test;

=pod

  Infix comparison operators L<S03/"Changes to Perl 5 operators"/"stealth postfix">

=cut


plan 4;

# infix less-than requires whitespace

ok(eval("1 <2"), "infix less-than (<) requires whitespace before.");
ok(eval("1 < 2"), "infix less-than (<) requires whitespace before.");
eval_dies_ok("1< 2", "infix less-than (<) requires whitespace before, so this is a parse error.", :todo<bug>);
eval_dies_ok("1<2", "infix less-than (<) requires whitespace before, so this is a parse error.", :todo<bug>);
