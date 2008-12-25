use v6;

# P35 (**) Determine the prime factors of a given positive integer.
#
# Construct a flat list containing the prime factors in ascending order.
# Example:
# * (prime-factors 315)
# (3 3 5 7)

sub prime_factors($n is copy) {
    my @factors;

    my $cand = 2;
    while ($n > 1) {
        if $n % $cand == 0 {
            @factors.push($cand);
            $n /= $cand;
        }
        else {
            $cand++;
        }
    }

    @factors
}

# XXX -- this breaks due to $_ being clobbered.
sub pf2($n) {
    for 2..floor sqrt $n {
        return ($_, pf2($n/$_)) if $n % $_ == 0;
    }
    return $n;
}

use Test;
plan 1;

is prime_factors(315), (3,3,5,7), 'prime factors of 315 are 3,3,5,7';
