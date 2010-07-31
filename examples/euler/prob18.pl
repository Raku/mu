#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

    3
   7 5
  2 4 6
 8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

[triangle omitted]

NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)
=end Problem

=begin Insight
This problem is an excellent example of dynamic programming. Instead of searching for the optimal route by starting from the top and going down each possible path, we use some cleverness. Let's instead look at the second to bottom row. 

    3
   7 5
  2 4 6   <--
 8 5 9 3

From the leftmost cell, 2, we can either go to 8 or 5. Obviously we want to go to the 8. From the next cell, 4, we can go to 5 or 9. We want to go to 9. For the rightmost cell, 6, we go to 9 instead of 3. So we know how to move from the second to bottom row to the bottom row. The insight is I<now we can ignore the bottom row>. We can do this by summing each cell in the second to bottom row with the cell we would want to go to. Doing that produces this:

      3
    7   5
  10  13  15   <--

We can do this repeatedly until we get to the topmost cell. That cell will end up with the optimal path. This method will quickly solve both this 15-row triangle and the 100-row triangle that the note in the problem statement warns us about.
=end Insight

use v6;
use Benchmark;# qw<timeit>;

my @triangle;
for [<75>],
    [<95 64>],
    [<17 47 82>],
    [<18 35 87 10>],
    [<20 04 82 47 65>],
    [<19 01 23 75 03 34>],
    [<88 02 77 73 07 63 67>],
    [<99 65 04 28 06 16 70 92>],
    [<41 41 26 56 83 40 80 70 33>],
    [<41 48 72 33 47 32 37 16 94 29>],
    [<53 71 44 65 25 43 91 52 97 51 14>],
    [<70 11 33 28 77 73 17 78 39 68 17 57>],
    [<91 71 52 38 17 14 91 43 58 50 27 29 48>],
    [<63 66 04 68 89 53 67 30 73 16 69 87 40 31>],
    [<04 62 98 27 23 09 70 98 73 93 38 53 60 04 23>]
        -> $row { push @triangle, $row }

my @check;
for [<3>],
    [<7 5>],
    [<2 4 6>],
    [<8 5 9 3>]
        -> $row { push @check, $row }

sub optimal_path(@triangle) {
    while (@triangle.elems > 1) {
        my @last = @(pop @triangle);
        # XXX: Pugs doesn't seem to have implemented @triangle[*-1] in favor
        #      of @triangle[*-1] yet
        for 0 .. @triangle[*-1].elems -> $c {
            @triangle[*-1][$c] += [@last[$c], @last[$c+1]].max;
        }
    }

    return @triangle[0][0];
}

sub check {
    my $path = optimal_path(@check);
    say $path == 23 ?? "ok" !! "not ok: got $path, expected 23";
}

sub main {
    say optimal_path(@triangle);
}

my @t = timeit(1, &main);
say "execution time: @t[0]"
