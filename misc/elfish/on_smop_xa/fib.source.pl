method fib(int $n) {
  if ($n == 0) {
    0
  } elsif ($n == 1) {
    1
  } else {
    self.fib($n - 1) + self.fib($n - 2)
  }
}
