use v6-alpha;

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

unless caller {
    use Test;
    plan 1;

    is prime_factors(315), (3,3,5,7), 'prime factors of 315 are 3,3,5,7';
}
