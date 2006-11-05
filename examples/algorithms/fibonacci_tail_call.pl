use v6;

my $n = @*ARGS[0] // 42;
say fib($n);

sub fib (UInt $n, Int $curr = 0, Int $next = 1) {
    return $curr unless $n;
    return fib($n-1, $next, $curr + $next);
}
