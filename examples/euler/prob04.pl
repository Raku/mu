#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit
numbers.
=end Problem

use v6;
use Benchmark;

sub is_palindrome($s) {
    $s eq $s.reverse;
}

sub main {
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
}


my @t = timeit(1, \&main);
say "execution time: @t[0]";
