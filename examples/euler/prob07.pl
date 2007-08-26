
#!/usr/bin/env pugs

use v6;

sub is_prime($n) {
    my @primes = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
        47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109,
        113);

    return True  if $n == any(@primes);
    return False if $n % any(@primes) == 0;

    for (2..sqrt($n)) -> $i {
        return False if $n % $i == 0;
    }

    True;
}

my $prime;
my $count = 0;

for 2..* -> $i {
    $count++ if is_prime($i);
    if $count == 10001 {
        say $i;
        last;
    }
}
