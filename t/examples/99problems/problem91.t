use v6-alpha;
use Test;
plan 1;

# P91 (**) Knight's tour
# 
# Another famous problem is this one: How can a knight jump on an NxN chessboard
# in such a way that it visits every square exactly once?
# 
# Hints: Represent the squares by pairs of their coordinates of the form X/Y,
# where both X and Y are integers between 1 and N. (Note that '/' is just a
# convenient functor, not division!) Define the relation jump(N,X/Y,U/V) to
# express the fact that a knight can jump from X/Y to U/V on a NxN chessboard.
# And finally, represent the solution of our problem as a list of N*N knight
# positions (the knight's tour).

if 1 {
    skip 1, "Test(s) not yet written: (**) Knight's tour";
}
else {
    ok 1, "(**) Knight's tour";
}
