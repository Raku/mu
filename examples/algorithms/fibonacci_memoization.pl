use v6;

# See http://perlmonks.org/?node_id=582273 for a discussion on state variables for memoization

my $n = @*ARGS[0] // 42;
say fib($n);

sub fib (UInt $n) {
    (state @seen = 0,1,1)[$n] //= fib($n-1) + fib($n-2);
}

# Also works:
# sub fib (UInt $n) {
#     (state $seen = [0,1,1]).[$n] //= fib($n-1) + fib($n-2);
# }

