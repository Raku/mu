#!perl6
use v6;

my $upper = 1000;
my @primes = (2);
my @sieve;
my $i;
loop ($i = 3; $i < $upper; $i += 2)
{
    unless (@sieve[$i])
    {
        push @primes, $i;
        print "$i ";
        my $j;
        loop ($j = $i * $i; $j < $upper; $j += 2 * $i)
        {
            @sieve[$j] = 1;
        }
    }
}

say;

