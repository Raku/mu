use v6-alpha;

use Test;

plan 8;

# L<S29/Container/"=item each">

=pod

Tests of

  our Lazy multi Container::each( Bool :$shortest, Bool :$finite, *@@list );

=cut

ok(each() eqv (), 'each null identity');

ok(each(1) eqv (1,), 'each scalar identity');

ok(each(1..3) eqv 1..3, 'each list identity');

ok(each([1..3]) eqv 1..3, 'each array identity');

# Next 2 work.  Just waiting on eqv.

ok(each({'a'=>1,'b'=>2,'c'=>3}) eqv ('a'=>1,'b'=>2,'c'=>3),
    'each hash identity', :todo<feature>, :depends<eqv>);

ok(each((); 1; 2..4; [5..7]; {'a'=>1,'b'=>2}) eqv
    ( undef,     1, 2, 5, 'a'=>1,
      undef, undef, 3, 6, 'b'=>2,
      undef, undef, 4, 7,  undef ), 'basic each', :todo<feature>,
      :depends<eqv>);

# :shortest

ok(each(:shortest, 1..9; [1..3]) eqv (1, 1, 2, 2, 3, 3),
    'each(:shortest, ...)', :todo<feature>);

# :finite

flunk('each(:finite, ...)', :todo<feature>, :depends<lazy each>);

=begin lazy_each

eval_ok('each(:finite, 1..Inf; [1..6])', 'parse of each(:finite, ...)',
   :todo<feature>);

=cut
