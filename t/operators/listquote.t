use v6-alpha;

use Test;

=kwid

= DESCRIPTION

This file tests a parse failure between expressions
and list quotes <>:

The C<< <3 >> is seen as the start of a list that
extends into the commented line. The expression
should be parsed as restricted to the one C<ok()>
line of course.

The following expressions also contend for the
same problem:

Two-way comparison:

  1 < EXPR > 2

Hash access:

  HASHEXPR<KEY>

= TODO

Add relevant Sxx and/or Axx references, that
describe the conflicting cases.

=cut

plan 13;

# L<S02/"Literals">
# L<S03/"Chained comparisons">

my $s = join 'a', <x y z>;
is($s, "xayaz", 'list context <list>');

my $s = join |<< <a x y z>;
is($s, "xayaz", 'listop |<< <list>', :todo<bug>);

my $x = [1,2,3].join<a b c>;
ok(!$!, '.join<abc> parses but semantic error');
is($x, [1,2,3].join()<a b c>, '.join()<a b c> not treated as argument');

my @y = try { {:a<1>, :b(2)}<a b c> };
is(@y, [1,2,undef], '{...}<a b c> is hash subscript');

eval '{:a<1>, :b(2)} <a b c>';
ok($!, '{...} <...> parsefail');

ok((1 | 3) < 3, '(...) < 3 no parsefail');

eval '(1 | 3)<3';
ok($!, '()<3 parsefail', :todo<bug>);

eval 'print < 3';
ok($!, 'print < 3 parsefail');

my $z = eval 'reverse<1 2 3>';
ok($!, 'reverse<1 2 3> parsefail', :todo<bug>);

eval ':foo <1 2 3>';
ok($!, ':foo <1 2 3> parsefail');

my $r = eval ':foo <3';
is($r, Bool::True, ':foo <3 is comparison');

my $p = eval ':foo<1 2 3>';
is($p, ~('foo' => (1,2,3)), ':foo<1 2 3> is pair of list');

=cut
