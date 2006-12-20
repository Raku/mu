use v6-alpha;
use Test;
plan 1;

# P69 (**) Dotstring representation of binary trees
# 
# We consider again binary trees with nodes that are identified by single
# lower-case letters, as in the example of problem P67. Such a tree can be
# represented by the preorder sequence of its nodes in which dots (.) are
# inserted where an empty subtree (nil) is encountered during the tree
# traversal. For example, the tree shown in problem P67 is represented as
# 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams)
# and then write a predicate tree-dotstring/2 which does the conversion in both
# directions. Use difference lists.
# 
# Multiway Trees
# 
# A multiway tree is composed of a root element and a (possibly empty) set of
# successors which are multiway trees themselves. A multiway tree is never
# empty. The set of successor trees is sometimes called a forest.
# 
# 
# In Prolog we represent a multiway tree by a term t(X,F), where X denotes the
# root node and F denotes the forest of successor trees (a Prolog list). The
# example tree depicted opposite is therefore represented by the following
# Prolog term:
# 
# T = t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])

if 1 {
    skip 1, "Test(s) not yet written: (**) Dotstring representation of binary trees";
}
else {
    ok 1, '(**) Dotstring representation of binary trees';
}
