use v6-alpha;

use Test;

=kwid

There is helper code in Prelude.pm.

Moving primitives from Prim.hs to Prelude.pm has been a goal for some
time.  However, there are difficulties, some of which this file tests for.

 - our &*f ::= &g; doesn't actually create a global.
 - Global multis behave non-multi, disappearing upon another declaration.

=cut

plan 11;

eval_is 'prelude_test_1()', 'test 1', 'sub *f';
eval_is 'prelude_test_2()', 'test 2', 'our &*f ::= &f2';
eval_is 'prelude_test_3()', 'test 3', 'our &*f := &f2';

is prelude_test_4('ss'), 'test 4', 'multi f(Str $s) is builtin';
{
  eval_ok q<multi prelude_test_4(Int $n){'4 Int'};1>, '... added multi f(Int $n)';
  is prelude_test_4(3), '4 Int', '... new signature works';
  is prelude_test_4('ss'), 'test 4', '... old signature still works!';
}
is prelude_test_5('ss'), 'test 5', 'multi *f(Str $s)';
{
  eval_ok q<multi prelude_test_5(Int $n){'5 Int'};1>, '... added multi f(Int $n)';
  is prelude_test_5(3), '5 Int', '... new signature works';
  is prelude_test_5('ss'), 'test 5', '... old signature still works!';
}
