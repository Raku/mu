#!/usr/bin/pugs

use v6;
use Test;

=kwid

loop statement tests

L<S04/"The general loop statement">

=cut

plan 8;
force_todo 8;

# basic loop

my $i = 0;
is($i, 0, 'verify our starting condition');
loop ($i = 0; $i < 10; $i++) {}
is($i, 10, 'verify our ending condition');

# loop with last()

my $i = 0;
is($i, 0, 'verify our starting condition');
loop ($i = 0; $i < 10; $i++) {
    if ($i == 5) { 
        last(); # should this really need the ()
    }
}
is($i, 5, 'verify our ending condition');

# infinite loop

my $i = 0;
is($i, 0, 'verify our starting condition');
loop (;;) { $i++; last(); }
is($i, 1, 'verify our ending condition');

# declare variable inside loop
my $count = 0;
is($count, 0, 'verify our starting condition');
eval 'loop (my $j = 0; $j < 10; $j++) { $count++; }';
is($count, 10, 'verify our ending condition');
