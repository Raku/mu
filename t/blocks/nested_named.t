use v6;

use Test;

=begin desc

Tests for nested subs in Apocalypse 6

=end desc 

plan 3;

sub factorial (Int $n) {
    my sub facti (Int $acc, Int $i) {
        return $acc if $i > $n;
        facti($acc * $i, $i + 1);
    }
    facti(1, 1);
} ;

is factorial(1), 1, "Checking semantics... 1";
is factorial(2), 2, "Checking semantics... 2";
is factorial(0), 1, "Checking semantics... 0";

# vim: ft=perl6
