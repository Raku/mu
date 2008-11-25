#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
=end Problem

use v6;
use Benchmark;

my $max = 10000;
my int @divsum = (1) xx $max;
# slight variation of the Sieve of Eratosthenes
for 2 .. $max / 2 -> $n {
    loop (my $m = $n + $n; $m < $max; $m += $n) {
        @divsum[$m] += $n;
    }
}

sub divsum($n) {
    my $sum = 0;
    for 2 .. ^$n -> $d {
        last if $d * $d >= $n;
        $sum += $d + ($n / $d) if $n % $d == 0;
    }
    return $sum;
}

sub is_amicable($a) {
    my $b = @divsum[$a];
    return Bool::False if $a == $b;

    # happens if the divsum of a is greater than max
    if (!@divsum[$b]) { @divsum[$b] = divsum($b) }

    return $a == @divsum[$b];
}

sub check {
    say is_amicable(220) && is_amicable(284) ?? "ok" !! "not ok";
}

sub main {
    say sum(grep { is_amicable($_) }, 1..^$max);
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";

