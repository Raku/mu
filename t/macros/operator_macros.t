use v6-pugs;

use Test;

plan 1;

macro postfix:<!> (Int $n) {
    my $factorial = [*] 1..$n;
    return "$factorial + 0";
}

is 3!, 6, "macro postfix:<!> works";
