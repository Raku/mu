use v6-alpha;

use Test;

plan 1;

# L<S29/Container/"=item cat">

=pod

Tests of

  our Lazy multi Container::cat( *@@list );

=cut

my @foo := [[1,2,3],[4,5,6]];

my $answer = list(1..6);

eval_ok('cat([;] @foo) eqv (@foo[0], @foo[1])', 'basic cat() test',
    :todo<feature>);
