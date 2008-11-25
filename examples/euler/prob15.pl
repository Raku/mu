#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.

How many routes are there through a 20x20 grid?
=end Problem

use v6;
use Benchmark;

proto postfix:<!>($n) {
    return 1 if $n < 2;
    return $n * ($n-1)!
}

sub routes($n) {
    # 40 choose 20
    return ((2*$n)!) / ($n! * $n!);
}

sub check {
    my $routes = routes(2);
    # can't use ?? !! here yet because the postfix:<!> screws it up
    say "ok" and return if $routes == 6;
    say "not ok: got $routes, expected 6";
}

sub main {
    say routes(20);
}

my @t = timeit(1, \&main);
say "execution time: @t[0]";
