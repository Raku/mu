#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.

Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
=end Problem

use v6;
use Benchmark;

#sub fib($n) {
#    my @fibs = (0, 1);
#
#    for ^($n - 1) {
#        @fibs = (@fibs[1], @fibs[0] + @fibs[1])
#    }
#
#    @fibs[1];
#}


sub main {
    my @fib_pair = (0, 1);
    for 2..* -> $i {
        if (@fib_pair[0] + @fib_pair[1]).bytes >= 1000 {
            say $i;
            last;
        }
        @fib_pair = (@fib_pair[1], (@fib_pair[0] + @fib_pair[1]));
    }
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
