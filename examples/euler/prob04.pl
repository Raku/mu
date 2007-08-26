#!/usr/bin/env pugs

use v6;

sub is_palindrome($s) {
    $s eq $s.reverse;
}

my @products;
my $largest_palindrome;
my $n;

for 100..999 -> $i {
    for 100..999 -> $j {
        $n = $i * $j;
        if is_palindrome($n) && $n > $largest_palindrome {
            $largest_palindrome = $n;
        }
    }
}

say $largest_palindrome;
