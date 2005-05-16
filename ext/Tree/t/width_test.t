#!/usr/bin/pugs

use v6;
use Test;
use Tree;

plan 66;

{ # test height (with pictures)
    
    my $tree = Tree.new();
    
    my $D = Tree.new(node => 'D');
    
    $tree.add_child($D);
    
    #   |
    #  <D>
    
    is($D.width(), 1, '... D has a width of 1');
    
    my $E = Tree.new(node => 'E');
    
    $D.add_child($E);
    
    #   |
    #  <D>
    #    \
    #    <E>
    
    is($D.width(), 1, '... D has a width of 1');
    is($E.width(), 1, '... E has a width of 1');
    
    my $F = Tree.new(node => 'F');
    
    $E.add_child($F);
    
    #   |
    #  <D>
    #    \
    #    <E>
    #      \
    #      <F>
    
    is($D.width(), 1, '... D has a width of 1');
    is($E.width(), 1, '... E has a width of 1');
    is($F.width(), 1, '... F has a width of 1');
    
    my $C = Tree.new(node => 'C');
    
    $D.add_child($C);
    
    #    |
    #   <D>
    #   / \
    # <C> <E>
    #       \
    #       <F>
    
    is($D.width(), 2, '... D has a width of 2');
    is($E.width(), 1, '... E has a width of 1');
    is($F.width(), 1, '... F has a width of 1');
    is($C.width(), 1, '... C has a width of 1');
    
    my $B = Tree.new(node => 'B');
    
    $D.add_child($B);
    
    #        |
    #       <D>
    #      / | \
    #   <B> <C> <E>
    #             \
    #             <F>
    
    
    is($D.width(), 3, '... D has a width of 3');
    is($E.width(), 1, '... E has a width of 1');
    is($F.width(), 1, '... F has a width of 1');
    is($C.width(), 1, '... C has a width of 1');
    is($B.width(), 1, '... B has a width of 1');
        
    
    my $A = Tree.new(node => 'A');
    
    $E.add_child($A);
    
    #        |
    #       <D>
    #      / | \
    #   <B> <C> <E>
    #           / \
    #         <A> <F>       
    
    is($D.width(), 4, '... D has a width of 4', :todo<bug>);
    is($E.width(), 2, '... E has a width of 2');
    is($F.width(), 1, '... F has a width of 1');
    is($C.width(), 1, '... C has a width of 1');
    is($B.width(), 1, '... B has a width of 1');
    is($A.width(), 1, '... A has a width of 1');
    
    my $G = Tree.new(node => 'G');
    
    $E.insert_child(1, $G);
    
    #        |
    #       <D>
    #      / | \
    #   <B> <C> <E>
    #          / | \
    #       <A> <G> <F>         
    
    is($D.width(), 5, '... D has a width of 5');
    is($E.width(), 3, '... E has a width of 3');
    is($F.width(), 1, '... F has a width of 1');
    is($G.width(), 1, '... G has a width of 1');
    is($C.width(), 1, '... C has a width of 1');
    is($B.width(), 1, '... B has a width of 1');
    is($A.width(), 1, '... A has a width of 1');
    
    my $H = Tree.new(node => 'H');
    
    $G.add_child($H);
    
    #        |
    #       <D>
    #      / | \
    #   <B> <C> <E>
    #          / | \
    #       <A> <G> <F> 
    #            |
    #           <H>    
    
    is($D.width(), 5, '... D has a width of 5');
    is($E.width(), 3, '... E has a width of 3');
    is($F.width(), 1, '... F has a width of 1');
    is($G.width(), 1, '... G has a width of 1');
    is($H.width(), 1, '... H has a width of 1');
    is($C.width(), 1, '... C has a width of 1');
    is($B.width(), 1, '... B has a width of 1');
    is($A.width(), 1, '... A has a width of 1');
    
    my $I = Tree.new(node => 'I');
    
    $G.add_child($I);
    
    #        |
    #       <D>
    #      / | \
    #   <B> <C> <E>
    #          / | \
    #       <A> <G> <F> 
    #            | \
    #           <H> <I>   
    
    is($D.width(), 6, '... D has a width of 6', :todo<bug>);
    is($E.width(), 4, '... E has a width of 4', :todo<bug>);
    is($F.width(), 1, '... F has a width of 1');
    is($G.width(), 2, '... G has a width of 2');
    is($H.width(), 1, '... H has a width of 1');
    is($I.width(), 1, '... I has a width of 1');    
    is($C.width(), 1, '... C has a width of 1');
    is($B.width(), 1, '... B has a width of 1');
    is($A.width(), 1, '... A has a width of 1');      

    ok($E.remove_child($A), '... removed A subtree from B tree');

    #        |
    #       <D>
    #      / | \
    #   <B> <C> <E>
    #            | \
    #           <G> <F> 
    #            | \
    #           <H> <I>  

    is($D.width(), 5, '... D has a width of 5');
    is($E.width(), 3, '... E has a width of 3');
    is($F.width(), 1, '... F has a width of 1');
    is($G.width(), 2, '... G has a width of 2');
    is($H.width(), 1, '... H has a width of 1');
    is($C.width(), 1, '... C has a width of 2');
    is($B.width(), 1, '... B has a width of 1');
    
    # and the removed tree is ok
    is($A.width(), 1, '... A has a width of 1');
    
    ok($D.remove_child($E), '... removed E subtree from D tree');

    #        |
    #       <D>
    #      / | 
    #   <B> <C>

    is($D.width(), 2, '... D has a width of 2');
    is($C.width(), 1, '... C has a width of 1');
    is($B.width(), 1, '... B has a width of 1');
    
    # and the removed trees are ok
    is($E.width(), 3, '... E has a width of 3');
    is($F.width(), 1, '... F has a width of 1');
    is($G.width(), 2, '... G has a width of 2');
    is($H.width(), 1, '... H has a width of 1');    
    
    ok($D.remove_child($C), '... removed C subtree from D tree');

    #        |
    #       <D>
    #      /  
    #   <B> 

    is($D.width(), 1, '... D has a width of 1');
    is($B.width(), 1, '... B has a width of 1');
    
    # and the removed tree is ok
    is($C.width(), 1, '... C has a width of 1');
      
}
