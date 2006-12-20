use v6-alpha;
use Test;
plan 1;

# P58 (**) Generate-and-test paradigm
# 
# Apply the generate-and-test paradigm to construct all symmetric, completely
# balanced binary trees with a given number of nodes. Example:
# 
# * sym-cbal-trees(5,Ts).
# 
# Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]
# 
# How many such trees are there with 57 nodes? Investigate about how many
# solutions there are for a given number of nodes? What if the number is even?
# Write an appropriate predicate.

if 1 {
    skip 1, "Test(s) not yet written: (**) Generate-and-test paradigm";
}
else {
    ok 1, '(**) Generate-and-test paradigm';
}
