use v6-alpha;

use Test;

plan 1;

# L<S29/Container/"=item cat">

=pod

Tests of

  our Lazy multi Container::cat( *@@list );

=cut

ok(cat(1..3; [4,5,6]) eqv 1..6, 'basic cat() test');
