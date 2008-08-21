use v6;
use Test;
plan 2;

# P61 (*) Count the leaves of a binary tree

# A leaf is a node with no successors. 
# Write a predicate count_leaves/2 to count them.
#
#  % count_leaves(T,N) :- the binary tree T has N leaves

# only 'C' and 'D' are leaves
my $tree = ['A', ['B', ['C', undef, undef], ['D', undef, undef]], undef];

sub count_leaves($tree){
    return 0 unless defined($tree);
    return 1 if (not defined($tree[1])) and (not defined($tree[2]));
    return count_leaves($tree[1]) + count_leaves($tree[2]);
}

is(count_leaves($tree), 2, "count_leaves works");

# P61A (*) Collect the leaves of a binary tree in a list
# 
# A leaf is a node with no successors. Write a predicate leaves/2 to collect them
# in a list.
# 
# % leaves(T,S) :- S is the list of all leaves of the binary tree T

# the spec does not specify if the tree should be flattened in pre/infix or
# postfix order, let's just assue prefix or infix

my @expected = ('C', 'D');

sub leaves($tree){
    return () unless defined($tree);
    return ($tree[0],) if (not defined($tree[1])) and (not defined($tree[2]));
    return leaves($tree[1]), leaves($tree[2]);
}

is(leaves($tree), @expected, "leaves() works");
