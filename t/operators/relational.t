use v6-alpha;

use Test;

=kwid
   Relational Operators
=cut

plan 39;

## For Numbers: <, <=, <=>, >=, >
### refactored to t/spec/S03-operators/relational.t

# less than

ok(1 < 2, '1 is less than 2');
ok(!(2 < 1), '2 is ~not~ less than 1');

# greater than

ok(2 > 1, '2 is greater than 1');
ok(!(1 > 2), '1 is ~not~ greater than 2');

# less than or equal to

ok(1 <= 2, '1 is less than or equal to 2');
ok(1 <= 1, '1 is less than or equal to 2');
ok(!(1 <= 0), '1 is ~not~ less than or equal to 0');

# greater than or eqaul to

ok(2 >= 1, '2 is greater than or equal to 1');
ok(2 >= 2, '2 is greater than or equal to 2');
ok(!(2 >= 3), '2 is ~not~ greater than or equal to 3');

# spaceship
###  spaceship refactored to t/spec/S03-operators/comparison.t
is(1 <=> 1, 0,  '1 is equal to 1');
is(1 <=> 2, -1, '1 is less than 2');
is(2 <=> 1, 1,  '2 is greater than 1');

## Multiway comparisons (RFC 025)

# L<S03/"Chained comparisons">

# this works ...
is(5 > 1 < 10, 5 > 1 && 1 < 10, 'multi-way comp 5 > 1 < 10 works');

# however this doesn't which makes 
# me think these are not implemented
is(5 < 1 < 10, 5 < 1 && 1 < 10, 'multi-way comp 5 < 1 < 10 works');

## NOTE: these tests moved here from t/03operator.t 
# L<S03/"Chained comparisons">

ok(5 > 4 > 3, "chained comparison");
ok(3 < 4 < 5, "chained comparison");
ok(5 == 5 > -5, "chained comparison with equality");
ok(!(3 > 4 < 5), "chained n > n < n comparison");
ok(5 <= 5 > -5, "chained comparison with <=");
ok(-5 < 5 >= 5, "chained comparison with >=");

## For Strings: <, <=, <=>, >=, >

# less than

ok('a' lt 'b', 'a is less than b');
ok(!('b' lt 'a'), 'b is ~not~ less than a');

# greater than

ok('b' gt 'a', 'b is greater than a');
ok(!('a' gt 'b'), 'a is ~not~ greater than b');

# less than or equal to

ok('a' le 'b', 'a is less than or equal to b');
ok('a' le 'a', 'a is less than or equal to a');
ok(!('b' le 'a'), 'b is ~not~ less than or equal to a');

# greater than or eqaul to

ok('b' ge 'a', 'b is greater than or equal to a');
ok('b' ge 'b', 'b is greater than or equal to b');
ok(!('b' ge 'c'), 'b is ~not~ greater than or equal to c');

## Multiway comparisons (RFC 025)
# L<S03/"Chained comparisons">

# this works ...
is('e' gt 'a' lt 'j', 'e' gt 'a' && 'a' lt 'j', 'multi-way comp e gt a lt j works');

# however this doesn't which makes 
# me think these are not implemented
is('e' lt 'a' lt 'j', 'e' lt 'a' && 'a' lt 'j', 'multi-way comp e lt a lt j works');

## NOTE: these tests moved here from t/03operator.t 
# L<S03/"Chained comparisons">

ok("5" gt "4" gt "3", "5 gt 4 gt 3 chained str comparison");
ok("3" lt "4" lt "5", "3 lt 4 gt 5 chained str comparison");
ok(!("3" gt "4" lt "5"), "!(3 gt 4 lt 5) chained str comparison");
ok("5" eq "5" gt "0", '"5" eq "5" gt "0" chained str comparison with equality');
ok("5" le "5" gt "0", "5 le 5 gt 0 chained str comparison with le");
ok("0" lt "5" ge "5", "0 lt 5 ge 5 chained comparison with ge");
