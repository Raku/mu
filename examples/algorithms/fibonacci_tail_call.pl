use v6;

my $n = @*ARGS[0] // 42;
say fib($n);

sub fib (Int $n, Int $curr = 0, Int $next = 1) {
    return $curr if $n < 1;
    return fib($n-1, $next, $curr + $next);
}
