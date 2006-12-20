use v6-alpha;
use Test;
plan 1;

# P62B (*) Collect the nodes at a given level in a list
# 
# A node of a binary tree is at level N if the path from the root to the node has
# length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect
# all nodes at a given level in a list.
# 
# % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
# 
# Using atlevel/3 it is easy to construct a predicate levelorder/2 which creates
# the level-order sequence of the nodes. However, there are more efficient ways
# to do that.

if 1 {
    skip 1, "Test(s) not yet written: B (*) Collect the nodes at a given level in a list";
}
else {
    ok 1, 'B (*) Collect the nodes at a given level in a list';
}
