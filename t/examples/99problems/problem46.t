use v6-alpha;
use Test;
plan 1;

# P46 (**) Truth tables for logical expressions.
# 
# Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
# logical equivalence) which succeed or fail according to the result of their
# respective operations; e.g. and(A,B) will succeed, if and only if both A and B
# succeed. Note that A and B can be Prolog goals (not only the constants true and
# fail).
# 
# A logical expression in two variables can then be written in prefix notation,
# as in the following example: and(or(A,B),nand(A,B)).
# 
# Now, write a predicate table/3 which prints the truth table of a given logical
# expression in two variables.
# 
# Example:
# * table(A,B,and(A,or(A,B))).
# true true true
# true fail true
# fail true fail
# fail fail fail

if 1 {
    skip 1, "Test(s) not yet written: (**) Truth tables for logical expressions.";
}
else {
    ok 1, '(**) Truth tables for logical expressions.';
}
