use v6;

say "1..6";

# Mostly copied from Perl 5.8.4 s t/op/recurse.t

sub gcd {
    return gcd(@_[0] - @_[1], @_[1]) if (@_[0] > @_[1]);
    return gcd(@_[0], @_[1] - @_[0]) if (@_[0] < @_[1]);
    @_[0];
}

sub factorial {
    @_[0] < 2 ?? 1 :: @_[0] * factorial(@_[0] - 1);
}

sub fibonacci {
    @_[0] < 2 ?? 1 :: fibonacci(@_[0] - 2) + fibonacci(@_[0] - 1);
}

# Highly recursive, highly aggressive.
# Kids, do not try this at home.
#
# For example ackermann(4,1) will take quite a long time.
# It will simply eat away your memory. Trust me.

sub ackermann {
    return @_[1] + 1               if (@_[0] == 0);
    return ackermann(@_[0] - 1, 1) if (@_[1] == 0);
    ackermann(@_[0] - 1, ackermann(@_[0], @_[1] - 1));
}

# Highly recursive, highly boring.

sub takeuchi {
    @_[1] < @_[0] ??
        takeuchi(takeuchi(@_[0] - 1, @_[1], @_[2]),
                 takeuchi(@_[1] - 1, @_[2], @_[0]),
                 takeuchi(@_[2] - 1, @_[0], @_[1]))
            :: @_[2];
}

if(gcd(1147, 1271) == 31) { say "ok 1" } else { say "not ok 1" }

if(gcd(1908, 2016) == 36) { say "ok 2" } else { say "not ok 2" }

if(factorial(10) == 3628800) { say "ok 3" } else { say "not ok 3" }

if(factorial(factorial(3)) == 720) { say "ok 4" } else { say "not ok 4" }

if(fibonacci(10) == 89) { say "ok 5" } else { say "not ok 5" }

# if(fibonacci(fibonacci(7)) == 17711) { say "ok 6" } else { say "not ok 6" }
# takes too long
say "ok 6 # skip Takes too long to wait for"

