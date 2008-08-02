use v6;

# P31 (**) Determine whether a given integer number is prime.
# 
# Example:
# * (is-prime 7)
# T

# Very Naive implementation and 
# could probably use something like: 
#  subset Divisible::Int of Int where { $_ > 1 };
#  sub is_prime(Divisible::Int $num) {
# but "subset" is not working yet.

sub is_prime(Int $num) returns Bool {
    
    # 0 and 1 are not prime by definition
    return False if $num < 2;
    
    # 2 and 3 are
    return True  if $num < 4;

    # no even number is prime
    return False if $num % 2 == 0;

    # let's try what's left
    my $max=floor(sqrt($num));

    # we could use
    #  for ( 3..$max:by(2) ) {
    # but it's unimplemented yet 
    loop (my $i=3; $i <= $max ; $i+=2) {
        return False if $num % $i == 0;
    }
    return True;
}

unless caller {
    use Test;
    plan 10;
    ok !is_prime(0), "We should find that 0 is not prime";
    ok !is_prime(1), ".. and neither is 1";
    ok  is_prime(2), ".. 2 is prime";
    ok  is_prime(3), ".. 3 is prime";
    ok !is_prime(4), ".. 4 is not";
    ok  is_prime(5), ".. 5 is prime";
    ok !is_prime(6), ".. 6 is even, thus not prime";
    ok !is_prime(15), ".. 15 is product of two primes, but not prime";
    ok  is_prime(2531), ".. 2531 is a larger prime";
    ok !is_prime(2533), ".. 2533 is not";
}
