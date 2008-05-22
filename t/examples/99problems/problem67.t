use v6;
use Test;
plan 2;

# P67 (**) A string representation of binary trees
# 
# 
# Somebody represents binary trees as strings of the following type (see example
# opposite):
# 
# a(b(d,e),c(,f(g,)))
# 
# a) Write a Prolog predicate which generates this string representation, if the
# tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which
# does this inverse; i.e. given the string representation, construct the tree in
# the usual form. Finally, combine the two predicates in a single predicate
# tree-string/2 which can be used in both directions.

my $tree = ['a', ['b', ['d'], ['e']], ['c', undef, ['f', ['g']]]]; 
my $expected = "a(b(d,e),c(,f(g,)))";

sub stringify($tree) {
    return '' unless defined($tree);
    return $tree[0] if not defined($tree[1]) and (not defined($tree[2]));
    return $tree[0] ~ '(', stringify($tree[1]) ~ ',' ~ stringify($tree[2]) ~ ')';
}

ok(stringify($tree), $expected, "string representation of binary tree");

# b) Write the same predicate tree-string/2 using difference lists and a single
# predicate tree-dlist/2 which does the conversion between a tree and a
# difference list in both directions.
# 
# For simplicity, suppose the information in the nodes is a single letter and
# there are no spaces in the string.

if 1 {
    skip 1, "Test(s) not yet written: (**) A string representation of binary trees";
}
else {
    ok 1, '(**) A string representation of binary trees';
}
