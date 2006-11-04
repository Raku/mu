use v6;

my $n = @*ARGS[0] // 42;
say fib($n);

sub fib (Int $n) {
    state %seen;
    return 1 if $n < 2;
    %seen{$n - $_} //= fib($n - $_) for 1 .. 2;
    return %seen{$n - 1} + %seen{$n - 2};
}