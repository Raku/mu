use v6;
use FindBin;

# P37 (**) Calculate Euler's totient function phi(m) (improved).
# 
# See problem P34 for the definition of Euler's totient function. If the list of
# the prime factors of a number m is known in the form of problem P36 then the
# function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2)
# (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given
# number m. Then phi(m) can be calculated with the following formula:
# 
# phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
# 
# Note that a ** b stands for the b'th power of a.

# This made me mad, the above formula is wrong
# where it says + it should be *
# based on the fact that
#  phi(prime**m)=prime**(m-1)*(prime-1)
# and
#  some_number=some_prime**n * some_other_prime**m * ....
 
@INC.push($FindBin::Bin);
require 'problem36.t';

sub phi($n) {
  my $result=1;
  
  # XXX - I think there is a way of doing the unpacking + assignment 
  # in one step but don't know how

  for prime_factors_mult($n) -> @a  {
    my ($p,$m) = @a;
    $result *= $p ** ($m - 1) * ($p - 1);
  }
  $result;
}

unless caller {
  use Test;
  plan 20;

  constant @phi = *,1,1,2,2,4,2,6,4,6,4,10,4,12,6,8,8,16,6,18,8;

  for 1..20 -> $n {
        is phi($n), @phi[$n], "totient of $n is @phi[$n]";
  }
}

