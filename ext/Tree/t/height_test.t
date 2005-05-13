#!/usr/bin/pugs

use v6;
use Test;

plan 58;

use_ok('Tree');

{ # test height (with pictures)
    
    my $tree = Tree::new();
    
    my $D = Tree::new(node => 'D');
    
    $tree.add_child($D);
    
    #   |
    #  <D>
    
    is($D.height(), 1, '... D has a height of 1');
    
    my $E = Tree::new(node => 'E');
    
    $D.add_child($E);
    
    #   |
    #  <D>
    #    \
    #    <E>
    
    is($D.height(), 2, '... D has a height of 2');
    is($E.height(), 1, '... E has a height of 1');
    
    my $F = Tree::new(node => 'F');
    
    $E.add_child($F);
    
    #   |
    #  <D>
    #    \
    #    <E>
    #      \
    #      <F>
    
    is($D.height(), 3, '... D has a height of 3');
    is($E.height(), 2, '... E has a height of 2');
    is($F.height(), 1, '... F has a height of 1');
    
    my $C = Tree::new(node => 'C');
    
    $D.add_child($C);
    
    #    |
    #   <D>
    #   / \
    # <C> <E>
    #       \
    #       <F>
    
    is($D.height(), 3, '... D has a height of 3');
    is($E.height(), 2, '... E has a height of 2');
    is($F.height(), 1, '... F has a height of 1');
    is($C.height(), 1, '... C has a height of 1');
    
    my $B = Tree::new(node => 'B');
    
    $C.add_child($B);
    
    #      |
    #     <D>
    #     / \
    #   <C> <E>
    #   /     \
    # <B>     <F>
    
    
    is($D.height(), 3, '... D has a height of 3');
    is($E.height(), 2, '... E has a height of 2');
    is($F.height(), 1, '... F has a height of 1');
    is($C.height(), 2, '... C has a height of 2');
    is($B.height(), 1, '... B has a height of 1');
    
    my $A = Tree::new(node => 'A');
    
    $B.add_child($A);
    
    #        |
    #       <D>
    #       / \
    #     <C> <E>
    #     /     \
    #   <B>     <F>
    #   /         
    # <A>         
    
    is($D.height(), 4, '... D has a height of 4');
    is($E.height(), 2, '... E has a height of 2');
    is($F.height(), 1, '... F has a height of 1');
    is($C.height(), 3, '... C has a height of 3');
    is($B.height(), 2, '... B has a height of 2');
    is($A.height(), 1, '... A has a height of 1');
    
    my $G = Tree::new(node => 'G');
    
    $E.insert_child(0, $G);
    
    #        |
    #       <D>
    #       / \
    #     <C> <E>
    #     /   / \
    #   <B> <G> <F>
    #   /         
    # <A>         
    
    is($D.height(), 4, '... D has a height of 4');
    is($E.height(), 2, '... E has a height of 2');
    is($F.height(), 1, '... F has a height of 1');
    is($G.height(), 1, '... G has a height of 1');
    is($C.height(), 3, '... C has a height of 3');
    is($B.height(), 2, '... B has a height of 2');
    is($A.height(), 1, '... A has a height of 1');
    
    my $H = Tree::new(node => 'H');
    
    $G.add_child($H);
    
    #        |
    #       <D>
    #       / \
    #     <C> <E>
    #     /   / \
    #   <B> <G> <F>
    #   /     \    
    # <A>     <H>    
    
    is($D.height(), 4, '... D has a height of 4');
    is($E.height(), 3, '... E has a height of 3');
    is($F.height(), 1, '... F has a height of 1');
    is($G.height(), 2, '... G has a height of 2');
    is($H.height(), 1, '... H has a height of 1');
    is($C.height(), 3, '... C has a height of 3');
    is($B.height(), 2, '... B has a height of 2');
    is($A.height(), 1, '... A has a height of 1');

    ok($B.remove_child($A), '... removed A subtree from B tree');

    #        |
    #       <D>
    #       / \
    #     <C> <E>
    #     /   / \
    #   <B> <G> <F>
    #         \    
    #         <H> 

    is($D.height(), 4, '... D has a height of 4');
    is($E.height(), 3, '... E has a height of 3');
    is($F.height(), 1, '... F has a height of 1');
    is($G.height(), 2, '... G has a height of 2');
    is($H.height(), 1, '... H has a height of 1');
    is($C.height(), 2, '... C has a height of 2');
    is($B.height(), 1, '... B has a height of 1');
    
    # and the removed tree is ok
    is($A.height(), 1, '... A has a height of 1');
    
    ok($D.remove_child($E), '... removed E subtree from D tree');

    #        |
    #       <D>
    #       / 
    #     <C> 
    #     /     
    #   <B>

    is($D.height(), 3, '... D has a height of 3');
    is($C.height(), 2, '... C has a height of 2');
    is($B.height(), 1, '... B has a height of 1');
    
    # and the removed trees are ok
    is($E.height(), 3, '... E has a height of 3');
    is($F.height(), 1, '... F has a height of 1');
    is($G.height(), 2, '... G has a height of 2');
    is($H.height(), 1, '... H has a height of 1');    
    
    ok($D.remove_child($C), '... removed C subtree from D tree');

    #        |
    #       <D>

    is($D.height(), 1, '... D has a height of 1');
    
    # and the removed tree is ok
    is($C.height(), 2, '... C has a height of 2');
    is($B.height(), 1, '... B has a height of 1');      
}
