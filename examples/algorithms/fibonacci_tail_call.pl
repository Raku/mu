use v6;

my $n = @*ARGS[0] // 42;
say fib($n);

sub fib (UInt $n, UInt $curr = 0, UInt $next = 1) {
    return $curr unless $n;
    return fib($n-1, $next, $curr + $next);
}
