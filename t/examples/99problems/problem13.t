use v6-alpha;
use Test;
plan 1;

# P13 (**) Run-length encoding of a list (direct solution).
# 
# Implement the so-called run-length encoding data compression method directly.
# I.e. don't explicitly create the sublists containing the duplicates, as in
# problem P09, but only count them. As in problem P11, simplify the result list
# by replacing the singleton lists (1 X) by X.
# 
# Example:
# * (encode-direct '(a a a a b c c a a d e e e e))
# ((4 A) B (2 C) (2 A) D (4 E))

if 1 {
    skip 1, "Test(s) not yet written: don't understand the problem";
}
else {
    ok 1, '(**) Run-length encoding of a list (direct solution).';
}
