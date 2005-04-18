#!/usr/bin/pugs

use v6;
require Test;

plan 5;

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
    @_[0] < 2 ?? 1 :: &?SUB(@_[0] - 2) + &?SUB(@_[0] - 1);
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
    # for the script failure here, see Parser.hs:589
    @_[1] < @_[0] ??
        takeuchi(takeuchi(@_[0] - 1, @_[1], @_[2]),
                 takeuchi(@_[1] - 1, @_[2], @_[0]),
                 takeuchi(@_[2] - 1, @_[0], @_[1]))
            :: @_[2];
}

ok(gcd(1147, 1271) == 31);
ok(gcd(1908, 2016) == 36);
ok(factorial(10) == 3628800);
ok(factorial(factorial(3)) == 720);
ok(fibonacci(10) == 89);

# ok(fibonacci(fibonacci(7)) == 17711);
# takes too long
# skip("Takes too long to wait for");


