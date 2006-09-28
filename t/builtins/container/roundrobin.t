use v6-alpha;

use Test;

plan 1;

# L<S29/Container/"=item roundrobin">

=pod

Tests of

  our Lazy multi Container::roundrobin( Bool :$shortest,
      Bool :$finite, *@@list );

=cut

my @a = 1;
my @b = 1..2;
my @c = 1..3;

eval_ok('roundrobin( @a; @b; @c )', 'parse of roundrobin', :todo<feature>);

=begin TODO

my @ans01 = gather {
    for roundrobin( @a; @b; @c ) -> $v {
        take($v);
    }
}

ok(@ans01.fmt('%s', ':') eq '1:1:1:2:2:3', 'basic roundrobin');

=end TODO

=cut
