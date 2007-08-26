#!/usr/bin/env pugs

use v6;

sub fib($n) {
    my @fibs = (0, 1);

    for ^($n - 1) {
        @fibs = (@fibs[1], @fibs[0] + @fibs[1])
    }

    @fibs[1];
}

my $sum = 0;
my $fib;

for 2..* -> $i {
    $fib = fib($i);

    last if $fib >= 1_000_000;

    $sum += $fib if $fib % 2 == 0;
}

say $sum;
