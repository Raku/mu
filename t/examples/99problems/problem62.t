use v6;
use Test;
plan 4;

#P62 (*) Collect the internal nodes of a binary tree in a list

# An internal node of a binary tree has either one or two non-empty 
# successors. Write a predicate internals/2 to collect them in a list.
#
#    % internals(T,S) :- S is the list of internal nodes of the binary tree T.
 
my $tree = ['A', ['B', ['C', undef, undef], ['D', undef, undef]], ['E', undef, undef]];

my @expected = ('A', 'B');

# assume preorder traversal

sub internals($tree){
    return () unless defined($tree);
    if defined($tree[1]) and defined($tree[2]) {
        return $tree[0], internals($tree[1]), internals($tree[2]);
    } else {
        return internals($tree[1]), internals($tree[2]);
    }
}

is(internals($tree), @expected, "internals() collects internal nodes");

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

sub atlevel($tree, $level) {
    return () unless defined($tree);
    return $tree[0] if $level == 1;
    return atlevel($tree[1], $level - 1), atlevel($tree[2], $level - 1);
}

my @e1 = 'A', ;
my @e2 = 'B', 'E';
my @e3 = 'C', 'D';
is(atlevel($tree, 1), @e1, "atlevel() works at level 1");
is(atlevel($tree, 2), @e2, "atlevel() works at level 2");
is(atlevel($tree, 3), @e3, "atlevel() works at level 3");
