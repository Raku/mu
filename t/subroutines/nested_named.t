#!/usr/bin/pugs

use v6;
use Test;

=kwid

Tests for nested subs in Apocalypse 6

=cut 

plan 3;

sub factorial (Int $n) {
    my sub facti (Int $acc, Int $i) {
        return $acc if $i > $n;
	facti($acc * $i, $i + 1);
    }
    facti(1, 1);
} ;

is factorial(1), 1, "Checking semantics... 1", :todo<bug>;
is factorial(2), 2, "Checking semantics... 2", :todo<bug>;
is factorial(0), 1, "Checking semantics... 0", :todo<bug>;

