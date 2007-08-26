#!/usr/bin/env pugs

=begin Problem
=cut

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

