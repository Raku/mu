use v6-alpha;
use Test;
plan 1;

# P47 (*) Truth tables for logical expressions (2).
# 
# Continue problem P46 by defining and/2, or/2, etc as being operators. This
# allows to write the logical expression in the more natural way, as in the
# example: A and (A or not B). Define operator precedence as usual; i.e. as in
# Java.
# 
# Example:
# * table(A,B, A and (A or not B)).
# true true true
# true fail true
# fail true fail
# fail fail fail

if 1 {
    skip 1, "Test(s) not yet written: (*) Truth tables for logical expressions (2).";
}
else {
    ok 1, '(*) Truth tables for logical expressions (2).';
}
