module Algorithm::NaiveBayes::Util-0.03;
use v6;

# XXX -- correct?
# reduce++! :)
sub sum(@array) is export { reduce { $^a + $^b } 0, @array               }
sub max(@array) is export { reduce { $^a > $^b ?? $^a :: $^b } 0, @array }

sub sum_hash(%hash) is export { sum values %hash }

sub variance(@array, Num ?$mean is copy) is export {
  return 0 unless @array > 1;

  $mean //= sum(@array) / @array;

  my $var = 0;
  $var   += ($_ - $mean)**2 for @array;
  return $var / (@array - 1);
}

sub add_hash(%first is rw, %second) is export {
  for keys %second {
    %first{$^k} += %second{$^k};
  }
}

sub rescale(%scores) {
  # Scale everything back to a reasonable area in logspace (near zero),
  # un-loggify, and normalize
  my $total = 0;
  my $max   = max values %scores;

  for values %scores -> {
    $_      = exp($_ - $max);
    $total += $_**2;
  }
  $total = sqrt $total;
  for values %scores -> {
    $_ /= $total;
  }
}

1;
