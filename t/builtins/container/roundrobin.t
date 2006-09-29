use v6-alpha;

use Test;

plan 4;

# L<S29/Container/"=item roundrobin">

=pod

Tests of

  our Lazy multi Container::roundrobin( Bool :$shortest,
      Bool :$finite, *@@list );

=cut

my @a = 1;
my @b = 1..2;
my @c = 1..3;

ok(roundrobin( 1; 1..2; 1..3 ) eqv (1, 1, 1, 2, 2, 3) , 'roundrobin lists');
ok(roundrobin( @a; @b; @c ) eqv (1, 1, 1, 2, 2, 3) , 'roundrobin arrays');

ok(roundrobin(:shortest, 1; 1..2; 1..3) eqv (1), 'roundrobin :shortest',
    :todo<feature>);

flunk('roundrobin :finite', :todo<feature>, :depends<lazy roundrobin>);

=begin lazy_roundrobin

ok(roundrobin(:finite, 1; 1..2; 1..3) eqv (1), 'roundrobin :shortest',
    :todo<feature>);

=cut
