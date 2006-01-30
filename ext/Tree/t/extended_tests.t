#!/usr/bin/pugs

use v6;

use Test;
use Tree;

plan 213;

# make a root for our tree
my $tree = Tree.new(node => "root tree");
isa_ok($tree, 'Tree');

# verfiy that it is a root
ok($tree.is_root());

# and since it has no children
# it is also a leaf node
ok($tree.is_leaf());

# check the value of the node,
# it should be root
is($tree.node(), "root tree", '... this tree is a root');

# we have no children yet
is($tree.child_count(), 0, '... we have no children yet');

# check the depth
is($tree.depth(), -1, '... we have no depth yet');

# check the index
is($tree.get_index(), -1, '... root trees have no index'); 

## ----------------------------------------------------------------------------
## testing adding children
## ----------------------------------------------------------------------------

# create a child
my $sub_tree = Tree.new(node => "1.0");
isa_ok($sub_tree, 'Tree');

# check the node value
is($sub_tree.node(), "1.0", '... this tree is 1.0');

# since we have not assigned a parent it 
# will still be considered a root
ok($sub_tree.is_root());

# and since it has no children
# it is also a leaf node
ok($sub_tree.is_leaf());    

# now add the child to our root
$tree.add_child($sub_tree);

# tree is no longer a leaf node
# now that we have a child
ok(!$tree.is_leaf());

# now that we have assigned a parent it
# will no longer be considered a root
ok(!$sub_tree.is_root());

# check the depth of the sub_tree
is($sub_tree.depth(), 0, '... depth should be 0 now');

# check the index
is($sub_tree.get_index(), 0, '... index should be 0 now');

# check the child count, 
# it should be one now
is($tree.child_count(), 1, '... we should have 1 children now');

# get the child we inserted
# and compare it with sub_tree
# they should be the same
ok($tree.get_child(0) === $sub_tree, '... make sure our sub_tree is fetchable');

# get the parent of sub_tree
my $sub_tree_parent = $sub_tree.parent();

# now test that the parent of
# our sub_tree is the same as
# our root    
ok($tree === $sub_tree_parent, '... make sure our sub_tree parent is tree');

## ----------------------------------------------------------------------------
## testing adding siblings
## ----------------------------------------------------------------------------
    
# create another sub_tree
my $sub_tree_2 = Tree.new(node => "2.0");
isa_ok($sub_tree_2, 'Tree');

# check its node value
is($sub_tree_2.node(), "2.0", '... this tree is 2.0');

# since we have not assigned a parent to 
# the new sub_tree it will still be
# considered a root
ok($sub_tree_2.is_root());

# and since it has no children
# it is also a leaf node
ok($sub_tree_2.is_leaf());

# add our new subtree as a sibling
# of our first sub_tree
$sub_tree.add_sibling($sub_tree_2);

# now that we have assigned a parent to
# the new sub_tree, it will no longer be 
# considered a root
ok(!$sub_tree_2.is_root());

# check the depth of the sub_tree
is($sub_tree_2.depth(), 0, '... depth should be 0 now');

# check the index
is($sub_tree_2.get_index(), 1, '... index should be 1');

# make sure that we now have 2 children in our root    
is($tree.child_count(), 2, '... we should have 2 children now');    

# and verify that the child at index 1
# is actually our second sub_tree    
ok($tree.get_child(1) === $sub_tree_2, '... make sure our sub_tree is fetchable');    
    
# get the parent of our second sub_tree
my $sub_tree_2_parent = $sub_tree_2.parent();

# and make sure that it is the 
# same as our root
ok($tree === $sub_tree_2_parent, '... make sure our sub_tree_2 parent is tree');
    
## ----------------------------------------------------------------------------
## test adding child by giving parent as a constructor argument
## ----------------------------------------------------------------------------    

# we create our new sub_tree and attach it
# to our root through its constructor
my $sub_tree_4 = Tree.new(node => "4.0");     
$tree.add_child($sub_tree_4);

# check its node value
is($sub_tree_4.node(), "4.0", '... this tree is 4.0');

# since we have assigned a parent to
# the new sub_tree, it will no longer be 
# considered a root
ok(!$sub_tree_4.is_root());

# check the depth of the sub_tree
is($sub_tree_4.depth(), 0, '... depth should be 0 now');

# check the index
is($sub_tree_4.get_index(), 2, '... index should be 2 now');

# but since it has no children
# it is also a leaf node
ok($sub_tree_4.is_leaf());

# make sure that we now have 3 children in our root    
is($tree.child_count(), 3, '... we should have 3 children now');

# and verify that the child at index 2
# is actually our latest sub_tree    
ok($tree.get_child(2) === $sub_tree_4, '... make sure our sub_tree is fetchable');    

# and make sure that the new sub-trees
# parent is the same as our root
ok($tree === $sub_tree_4.parent(), '... make sure our sub_tree_4 parent is tree');

## ----------------------------------------------------------------------------
## test inserting child 
## ----------------------------------------------------------------------------    

# we create our new sub_tree 
my $sub_tree_3 = Tree.new(node => "3.0");     

# check its node value
is($sub_tree_3.node(), "3.0", '... this tree is 3.0');

# since we have not assigned a parent to 
# the new sub_tree it will still be
# considered a root
ok($sub_tree_3.is_root());

# but since it has no children
# it is also a leaf node
ok($sub_tree_3.is_leaf());

# now insert the child at index 2
$tree.insert_child(2, $sub_tree_3);

# since we now have assigned a parent to
# the new sub_tree, it will no longer be 
# considered a root
ok(!$sub_tree_3.is_root());

# check the depth of the sub_tree
is($sub_tree_3.depth(), 0, '... depth should be 0 now');

# check the index of 3
is($sub_tree_3.get_index(), 2, '... index should be 2 now');

# check the index of 4 now
is($sub_tree_4.get_index(), 3, '... index should be 3 now');

# make sure that we now have 3 children in our root    
is($tree.child_count(), 4, '... we should have 4 children now');

# and verify that the child at index 2
# is actually our latest sub_tree    
ok($tree.get_child(2) === $sub_tree_3, '... make sure our sub_tree is fetchable');    

# and verify that the child that was 
# at index 2 is actually now actually
# at index 3    
ok($tree.get_child(3) === $sub_tree_4, '... make sure our sub_tree is fetchable');    

# and make sure that the new sub-trees
# parent is the same as our root
ok($tree === $sub_tree_3.parent(), '... make sure our sub_tree_3 parent is tree');    

## ----------------------------------------------------------------------------
## test getting all children and siblings
## ----------------------------------------------------------------------------    
=pod
# get it in item context and
# check that our arrays are equal
my $children = $tree.get_all_children();
ok eq_array($children, [ $sub_tree, $sub_tree_2, $sub_tree_3, $sub_tree_4 ]);

# get it in array context and
# check that our arrays are equal
my @children = $tree.get_all_children();
ok eq_array(\@children, [ $sub_tree, $sub_tree_2, $sub_tree_3, $sub_tree_4 ]);

# check that the values from both
# contexts are equal to one another
ok eq_array($children, \@children);

# now check that the siblings of all the 
# sub_trees are the same as the children
foreach my $_sub_tree (@children) {
    # test siblings in item context
    my $siblings = $sub_tree.get_all_siblings();
    ok eq_array($children, $siblings);
    # and now in array context
    my @siblings = $sub_tree.get_all_siblings();
    ok eq_array($children, \@siblings);
}
=cut
## ----------------------------------------------------------------------------
## test addChildren
## ----------------------------------------------------------------------------    

my @sub_children = (
             Tree.new(node => "1.1"),
            Tree.new(node => "1.5"),
            Tree.new(node => "1.6")
            );

# now go through the children and test them
for (@sub_children) -> $sub_child {
    # they should think they are root
    ok($sub_child.is_root());

    # and they should all be leaves
    ok($sub_child.is_leaf());

    # and their node values
    like($sub_child.node(), rx:perl5/1\.[0-9]/, '... they at least have "1." followed by a digit');
    
    # and they should all have a depth of -1
    is($sub_child.depth(), -1, '... depth should be -1');    
}

# check to see if we can add children
$sub_tree.add_children(@sub_children);

# we are no longer a leaf node now
ok(!$sub_tree.is_leaf());

# make sure that we now have 3 children now    
is($sub_tree.child_count(), 3, '... we should have 3 children now');

=pod
# now check that sub_tree's children 
# are the same as our list
ok eq_array([ $sub_tree.get_all_children() ], \@sub_children);
=cut

# now go through the children again
# and test them
for (@sub_children) -> $sub_child {
    # they should no longer think
    # they are root
    ok(!$sub_child.is_root());
    
    # but they should still think they 
    # are leaves
    ok($sub_child.is_leaf());
    
    # now we test their parental relationship
    ok($sub_tree === $sub_child.parent(), '... their parent is the sub_tree');
    
    # and they should all have a depth of 1
    is($sub_child.depth(), 1, '... depth should be 1');
    
=pod
    # now check that its siblings are the same 
    # as the children of its parent            
    ok eq_array([ $sub_tree.get_all_children() ], [ $sub_child.get_all_siblings() ]);
=cut
}

## ----------------------------------------------------------------------------
## test insertingChildren
## ----------------------------------------------------------------------------    

my @more_sub_children = (
             Tree.new(node => "1.2"),
            Tree.new(node => "1.3"),
            Tree.new(node => "1.4")
            );

# now go through the children and test them
for (@more_sub_children) -> $sub_child {
    # they should think they are root
    ok($sub_child.is_root());

    # and they should all be leaves
    ok($sub_child.is_leaf());

    # and their node values
    like($sub_child.node(), rx:perl5/1\.[0-9]/, '... they at least have "1." followed by a digit');
    
    # and they should all have a depth of -1
    is($sub_child.depth(), -1, '... depth should be -1');    
}

# check to see if we can insert children
$sub_tree.insert_children(1, @more_sub_children);

# make sure that we now have 6 children now    
is($sub_tree.child_count(), 6, '... we should have 6 children now');

=pod
# now check that sub_tree's children 
# are the same as our list
ok eq_array([ $sub_tree.get_all_children() ], [ $sub_children[0], @more_sub_children, @sub_children[1 .. $#sub_children] ]);
=cut

# now go through the children again
# and test them
for (@more_sub_children) -> $sub_child {
    # they should no longer think
    # they are roots
    ok(!$sub_child.is_root());
    
    # but they should still think they 
    # are leaves
    ok($sub_child.is_leaf());
    
    # now we test their parental relationship
    ok($sub_tree === $sub_child.parent(), '... their parent is the sub_tree');
    
    # and they should all have a depth of 1
    is($sub_child.depth(), 1, '... depth should be 1');
    
=pod
    # now check that its siblings are the same 
    # as the children of its parent
    ok eq_array([ $sub_tree.get_all_children() ], [ $sub_child.get_all_siblings() ]);
=cut
}

## ----------------------------------------------------------------------------
## test addingSiblings
## ----------------------------------------------------------------------------    

my @more_children = (
             Tree.new(node => "5.0"),
            Tree.new(node => "9.0")
            );

# now go through the children and test them
for (@more_children) -> $sub_child {
    # they should think they are root
    ok($sub_child.is_root());

    # and they should all be leaves
    ok($sub_child.is_leaf());

    # and their node values
    like($sub_child.node(), rx:perl5/[0-9]\.0/, '... they at least have digit followed by ".0"');
    
    # and they should all have a depth of -1
    is($sub_child.depth(), -1, '... depth should be -1');    
}

# check to see if we can insert children
$sub_tree.add_siblings(@more_children);

# make sure that we now have 6 children now    
is($tree.child_count(), 6, '... we should have 6 children now');

# now check that tree's new children 
# are the same as our list
ok($tree.get_child(4) === @more_children[0], '... they are the same');
ok($tree.get_child(5) === @more_children[1], '... they are the same');

# now go through the children again
# and test them
for (@more_children) -> $sub_child {
    # they should no longer think
    # they are roots
    ok(!$sub_child.is_root());
    
    # but they should still think they 
    # are leaves
    ok($sub_child.is_leaf());
    
    # now we test their parental relationship
    ok($tree === $sub_child.parent(), '... their parent is the tree');
    
    # and they should all have a depth of 1
    is($sub_child.depth(), 0, '... depth should be 0');

=pod
    # now check that its siblings are the same 
    # as the children of its parent            
    ok eq_array([ $tree.get_all_children() ], [ $sub_child.get_all_siblings() ]);
=cut
}

## ----------------------------------------------------------------------------
## test insertSibling
## ----------------------------------------------------------------------------    

my $new_sibling = Tree.new(node => "8.0"); 

# they should think they are root
ok($new_sibling.is_root());

# and they should all be leaves
ok($new_sibling.is_leaf());

# and their node values
is($new_sibling.node(), "8.0", '... node value should be 6.0');

# and they should all have a depth of -1
is($new_sibling.depth(), -1, '... depth should be -1');    

# check to see if we can insert children
$sub_tree.insert_sibling(5, $new_sibling);

# make sure that we now have 6 children now    
is($tree.child_count(), 7, '... we should have 7 children now');

# now check that sub_tree's new sibling
# is in the right place and that it 
# should have displaced the old value at
# that index to index + 1 
ok($tree.get_child(4) === @more_children[0], '... they are the same');
ok($tree.get_child(5) === $new_sibling, '... they are the same');
ok($tree.get_child(6) === @more_children[1], '... they are the same');

# they should no longer think
# they are roots
ok(!$new_sibling.is_root());

# but they should still think they 
# are leaves
ok($new_sibling.is_leaf());

# now we test their parental relationship
ok($tree === $new_sibling.parent(), '... their parent is the tree');

# and they should all have a depth of 1
is($new_sibling.depth(), 0, '... depth should be 0');
    
=pod
# now check that its siblings are the same 
# as the children of its parent            
ok eq_array([ $tree.get_all_children() ], [ $new_sibling.get_all_siblings() ]);
=cut

## ----------------------------------------------------------------------------
## test inserting Siblings
## ----------------------------------------------------------------------------    

my @even_more_children = (
             Tree.new(node => "6.0"),
            Tree.new(node => "7.0")
            );

# now go through the children and test them
for (@even_more_children) -> $sub_child {
    # they should think they are root
    ok($sub_child.is_root());

    # and they should all be leaves
    ok($sub_child.is_leaf());

    # and their node values
    like($sub_child.node(), rx:perl5/[0-9]\.0/, '... they at least have digit followed by ".0"');
    
    # and they should all have a depth of -1
    is($sub_child.depth(), -1, '... depth should be -1');    
}

# check to see if we can insert children
$sub_tree.insert_siblings(5, @even_more_children);

# make sure that we now have 6 children now    
is($tree.child_count(), 9, '... we should have 6 children now');

# now check that tree's new children 
# are the same as our list
ok($tree.get_child(4) === @more_children[0], '... they are the same');
ok($tree.get_child(5) === @even_more_children[0], '... they are the same');
ok($tree.get_child(6) === @even_more_children[1], '... they are the same');
ok($tree.get_child(7) === $new_sibling, '... they are the same');
ok($tree.get_child(8) === @more_children[1], '... they are the same');

# now go through the children again
# and test them
for (@even_more_children) -> $sub_child {
    # they should no longer think
    # they are roots
    ok(!$sub_child.is_root());
    
    # but they should still think they 
    # are leaves
    ok($sub_child.is_leaf());
    
    # now we test their parental relationship
    ok($tree === $sub_child.parent(), '... their parent is the tree');
    
    # and they should all have a depth of 1
    is($sub_child.depth(), 0, '... depth should be 0');

=pod    
    # now check that its siblings are the same 
    # as the children of its parent            
    ok eq_array([ $tree.get_all_children() ], [ $sub_child.get_all_siblings() ]);
=cut
}

## ----------------------------------------------------------------------------
## test getChild and getSibling
## ----------------------------------------------------------------------------    

# make sure that getChild returns the
# same as getSibling
for (0 .. $tree.child_count()) -> $i {
    is($tree.get_child($i), $sub_tree.get_sibling($i), '... siblings are the same as children');
}

## ----------------------------------------------------------------------------
## test self referential returns
## ----------------------------------------------------------------------------    

# addChildren's return value is actually $self
# so that method calls can be chained
my $self_ref_tree_test = Tree.new(node => "3.1")
                                .add_children(
                                    Tree.new(node => "3.1.1"),
                                    Tree.new(node => "3.1.2")
                                );
$sub_tree_3.add_child($self_ref_tree_test);

# make sure that it true
isa_ok($self_ref_tree_test, 'Tree');

# it shouldnt be a root
ok(!$self_ref_tree_test.is_root());

# and it shouldnt be a leaf
ok(!$self_ref_tree_test.is_leaf());

# make sure that the parent in the constructor worked
ok($sub_tree_3 === $self_ref_tree_test.parent(), '... should be the same');

# and the parents count should be 1
is($sub_tree_3.child_count(), 1, '... we should have 1 child here');

# make sure they show up in the count test
is($self_ref_tree_test.child_count(), 2, '... we should have 2 children here');

for ($self_ref_tree_test.get_all_children()) ->  $sub_child {
    # they should not think
    # they are roots
    ok(!$sub_child.is_root());
    
    # but they should think they 
    # are leaves
    ok($sub_child.is_leaf());
    
    # now we test their parental relationship
    ok($self_ref_tree_test === $sub_child.parent(), '... their parent is the tree');
    
    # and they should all have a depth of 1
    is($sub_child.depth(), 2, '... depth should be 0');
=pod    
    # now check that its siblings are the same 
    # as the children of its parent            
    ok eq_array([ $self_ref_tree_test.get_all_children() ], [ $sub_child.get_all_siblings() ]);
=cut
}

## ----------------------------------------------------------------------------    
## Test self-referential version of addChild
## ----------------------------------------------------------------------------

# addChild's return value is actually $self
# so that method calls can be chained
my $self_ref_tree_test_2 = Tree.new(node => "2.1")
                                .add_child(
                                    Tree.new(node => "2.1.1")
                                );
$sub_tree_2.add_child($self_ref_tree_test_2);
                                
# make sure that it true
isa_ok($self_ref_tree_test_2, 'Tree');

# it shouldnt be a root
ok(!$self_ref_tree_test_2.is_root());

# and it shouldnt be a leaf
ok(!$self_ref_tree_test_2.is_leaf());

# make sure that the parent in the constructor worked
ok($sub_tree_2 === $self_ref_tree_test_2.parent(), '... should be the same');

# and the parents count should be 1
is($sub_tree_2.child_count(), 1, '... we should have 1 child here');

# make sure they show up in the count test
is($self_ref_tree_test_2.child_count(), 1, '... we should have 1 child here');

my $sub_child = $self_ref_tree_test_2.get_child(0);

# they should not think
# they are roots
ok(!$sub_child.is_root());

# but they should think they 
# are leaves
ok($sub_child.is_leaf());

# now we test their parental relationship
ok($self_ref_tree_test_2 === $sub_child.parent(), '... their parent is the tree');

# and they should all have a depth of 1
is($sub_child.depth(), 2, '... depth should be 0');

=pod
# now check that its siblings are the same 
# as the children of its parent        
ok eq_array([ $self_ref_tree_test_2.get_all_children() ], [ $sub_child.get_all_siblings() ]);
=cut

## ----------------------------------------------------------------------------
## test removeChildAt
## ----------------------------------------------------------------------------    

my $sub_tree_of_tree_to_remove = Tree.new(node => "1.1.a.1");
# make a node to remove
my $tree_to_remove = Tree.new(node => "1.1.a").add_child($sub_tree_of_tree_to_remove);

# test that its a root
ok($tree_to_remove.is_root());

# and that its depth is -1
is($tree_to_remove.depth(), -1, '... the depth should be -1'); 
# and the sub-trees depth is 0
is($sub_tree_of_tree_to_remove.depth(), 0, '... the depth should be 0'); 

# insert it into the sub_tree
$sub_tree.insert_child(1, $tree_to_remove);

# test that it no longer thinks its a root
ok(!$tree_to_remove.is_root());

# check thats its depth is now 1
is($tree_to_remove.depth(), 1, '... the depth should be 1'); 
# and the sub-trees depth is 2
is($sub_tree_of_tree_to_remove.depth(), 2, '... the depth should be 2'); 

# make sure it is there
ok($sub_tree.get_child(1) === $tree_to_remove, '... these tree should be equal');        

# remove the subtree (it will be returned)
my $removed_tree = $sub_tree.remove_child_at(1);

# now check that the one removed it the one 
# we inserted origianlly
ok($removed_tree === $tree_to_remove, '... these tree should be equal');

# it should think its a root again
ok($tree_to_remove.is_root(), :todo<bug>);
# and its depth should be back to -1
is($tree_to_remove.depth(), -1, '... the depth should be -1', :todo<bug>); 
# and the sub-trees depth is 0
is($sub_tree_of_tree_to_remove.depth(), 0, '... the depth should be 0', :todo<bug>);     

## ----------------------------------------------------------------------------
## test removeChild
## ----------------------------------------------------------------------------    

my $sub_tree_of_tree_to_remove2 = Tree.new(node => "1.1.a.1");
# make a node to remove
my $tree_to_remove2 = Tree.new(node => "1.1.a").add_child($sub_tree_of_tree_to_remove2);

# test that its a root
ok($tree_to_remove2.is_root());

# and that its depth is -1
is($tree_to_remove2.depth(), -1, '... the depth should be -1'); 
# and the sub-trees depth is 0
is($sub_tree_of_tree_to_remove2.depth(), 0, '... the depth should be 0'); 

# insert it into the sub_tree
$sub_tree.insert_child(1, $tree_to_remove2);

# test that it no longer thinks its a root
ok(!$tree_to_remove2.is_root());

# check thats its depth is now 1
is($tree_to_remove2.depth(), 1, '... the depth should be 1'); 
# and the sub-trees depth is 2
is($sub_tree_of_tree_to_remove2.depth(), 2, '... the depth should be 2'); 

# make sure it is there
ok($sub_tree.get_child(1) === $tree_to_remove2, '... these tree should be equal');        

# remove the subtree (it will be returned)
my $removed_tree2 = $sub_tree.remove_child($tree_to_remove2);

# now check that the one removed it the one 
# we inserted origianlly
ok($removed_tree2 === $tree_to_remove2, '... these tree should be equal');

# it should think its a root again
ok($tree_to_remove2.is_root(), :todo<bug>);
# and its depth should be back to -1
is($tree_to_remove2.depth(), -1, '... the depth should be -1', :todo<bug>); 
# and the sub-trees depth is 0
is($sub_tree_of_tree_to_remove2.depth(), 0, '... the depth should be 0', :todo<bug>);     

## ----------------------------------------------
## now test the edge cases
## ----------------------------------------------

# trees at the end

# make a node to remove
my $tree_to_remove_2 = Tree.new(node => "1.7");

# add it into the sub_tree
$sub_tree.add_child($tree_to_remove_2);

# make sure it is there
ok($sub_tree.get_child($sub_tree.child_count() - 1) === $tree_to_remove_2, '... these tree should be equal');        

# remove the subtree (it will be returned)
my $removed_tree_2 = $sub_tree.remove_child_at($sub_tree.child_count() - 1);

# now check that the one removed it the one 
# we inserted origianlly
ok($removed_tree_2 === $tree_to_remove_2, '... these tree should be equal');

# trees at the beginging

# make a node to remove
my $tree_to_remove_3 = Tree.new(node => "1.1.-1");

# add it into the sub_tree
$sub_tree.insert_child(0, $tree_to_remove_3);

# make sure it is there
ok($sub_tree.get_child(0) === $tree_to_remove_3, '... these tree should be equal');        

# remove the subtree (it will be returned)
my $removed_tree_3 = $sub_tree.remove_child_at(0);

# now check that the one removed it the one 
# we inserted origianlly
ok($removed_tree_3 === $tree_to_remove_3, '... these tree should be equal');        

## ----------------------------------------------------------------------------
## test traverse
## ----------------------------------------------------------------------------    

# make a control set of 
# all the nodes we have
my @_all_node_values = (
    '1.0', 
        '1.1',
        '1.2',
        '1.3',
        '1.4',
        '1.5',
        '1.6',
    '2.0',
        '2.1',
            '2.1.1',
    '3.0',
        '3.1',
            '3.1.1',
            '3.1.2',
    '4.0',
    '5.0',
    '6.0',
    '7.0',
    '8.0',
    '9.0'
);

my @all_node_values;
# now collect the nodes in the actual tree
$tree.traverse(-> $t {
    return unless $t;
    @all_node_values.push($t.node());
});

# and compare the two
is(~@_all_node_values, ~@all_node_values, '... our nodes match our control nodes');


## ----------------------------------------------------------------------------
## test size
## ----------------------------------------------------------------------------    

is($tree.size(), (@_all_node_values + 1), '... our size is as we expect it to be', :todo<bug>);

# NOTE:
# it is (item(@_all_node_values) + 1) so that 
# we account for the root node which is not in 
# the list.
