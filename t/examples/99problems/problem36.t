use v6;

# P36 (**) Determine the prime factors of a given positive integer (2).
# 
# Construct a list containing the prime factors and their multiplicity.
# Example:
# * (prime-factors-mult 315)
# ((3 2) (5 1) (7 1))
# 
# Hint: The problem is similar to problem P13.

sub prime_factors_mult(Int $n is copy){
  return () if $n == 1;
  my $count = 0;
  my $cond = 2;
  return gather {
    while $n > 1 {
      if $n % $cond == 0 {
	$count++;
	$n /= $cond;
      }
      else {
	if $count > 0 {
	  take [$cond,$count];
	  $count = 0;
	}
	$cond++;
      }
    }
    take [$cond,$count];
  }
}

unless caller() {
  use Test;
  plan 5;
  is prime_factors_mult(1),[], "We ignore 1";
  is prime_factors_mult(2),[[2,1]], "We get prime numbers prime";
  is prime_factors_mult(4),[[2,2]],  ".. and multiplicity right";
  is prime_factors_mult(12),[[2,2],[3,1]], ".. and products of primes";
  is prime_factors_mult(315),[[3,2],[5,1],[7,1]], ".. and ignore multiplicity 0";
}
