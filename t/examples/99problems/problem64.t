use v6;
use Test;
plan 1;

# P64 (**) Layout a binary tree (1)
# 
# Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a
# preparation for drawing the tree, a layout algorithm is required to determine
# the position of each node in a rectangular grid. Several layout methods are
# conceivable, one of them is shown in the illustration below.
# 
# In this layout strategy, the position of a node v is obtained by the following
# two rules:
# 
# * x(v) is equal to the position of the node v in the inorder sequence
# * y(v) is equal to the depth of the node v in the tree
# 
# 
# 
# In order to store the position of the nodes, we extend the Prolog term
# representing a node (and its successors) as follows:
# 
# % nil represents the empty tree (as usual)
# % t(W,X,Y,L,R) represents a (non-empty) binary tree with root W "positioned" at (X,Y), and subtrees L and R
# 
# Write a predicate layout-binary-tree/2 with the following specification:
# 
# % layout-binary-tree(T,PT) :- PT is the "positioned" binary tree obtained from
# the binary tree T. (+,?)
# 
# Test your predicate in an appropriate way.
 
my $tree = ['n', ['k', ['c', ['a', undef, undef], ['h', ['g', ['e', undef, undef], undef], undef]], ['m', undef, undef]], ['u', ['p', undef, ['s', ['q', undef, undef]], undef], undef]];
  
my $expected = ['n', 8, 1, 
        ['k', 6, 2, 
            ['c', 2, 3, 
                ['a', 1, 4,  undef, undef], 
                ['h', 5, 4,  
                    ['g', 4, 5, 
                        ['e', 3, 6, undef, undef], undef], undef]], 
            ['m', 7, 3, undef, undef]], 
        ['u', 12, 2, 
            ['p', 9, 3, undef, 
                ['s', 11, 4,
                    ['q', 10, 5, undef, undef]], undef], undef]];

sub count($tree) {
    return 0 unless defined ($tree);
    return 1 + count($tree[1]) + count($tree[2]);
}

sub align($tree, $prev_x, $prev_y, $lr){
    return undef unless defined($tree);
    my $y = $prev_y + 1;
    my $x = 0;
    if $lr eq "l" {
        $x = $prev_x - 1 - count($tree[2]);
    } else {
        $x = $prev_x + 1 + count($tree[1]);
    }
    return [$tree[0], 
           $x, 
           $y, 
           align($tree[1], $x, $y, "l"),
           align($tree[2], $x, $y, "r")];
}
my $result = align($tree, 0, 0, "r");

is($result, $expected, "tree alignment works");
