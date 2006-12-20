use v6-alpha;
use Test;
plan 1;

# P26 (**) Generate the combinations of K distinct objects chosen from the N
# elements of a list
# 
# In how many ways can a committee of 3 be chosen from a group of 12 people? We
# all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
# well-known binomial coefficients). For pure mathematicians, this result may be
# great. But we want to really generate all the possibilities in a list.
# 
# Example:
# * (combination 3 '(a b c d e f))
# ((A B C) (A B D) (A B E) ... )

if 1 {
    skip 1, "Test(s) not yet written: (**) Generate the combinations of K distinct objects chosen from the N";
}
else {
    ok 1, '(**) Generate the combinations of K distinct objects chosen from the N';
}
