#!/usr/bin/pugs

use v6;
use Test;
use Tree;

plan 28;

my $tree = Tree.new(node => 'my tree');

# check our default values/state
is($tree.node(), 'my tree', '... got the right node value');
is($tree.depth(), -1, '... got the right depth value');
is($tree.parent(), undef, '... no parent value');
is($tree.child_count(), 0, '... this tree has no children');

# check some predicates
ok($tree.is_root(), '... this tree is a root');
ok($tree.is_leaf(), '... this tree is a leaf');

# make some changes
$tree.node = 'my tree again';
is($tree.node(), 'my tree again', '... got the right changed node value');

my $tree2 = Tree.new(node => 'my other tree');
is($tree.node(), 'my tree again', '... the new instance did not spoil my old one');

# check our initial state again
is($tree2.node(), 'my other tree', '... got my $tree2 node');
is($tree2.depth(), -1, '... got the right depth value for $tree2');
is($tree2.parent(), undef, '... $tree2 no parent value');
is($tree2.child_count(), 0, '... $tree2 has no children');

# check some predicates
ok($tree2.is_root(), '... $tree2 is a root');
ok($tree2.is_leaf(), '... $tree2 is a leaf');

$tree.add_child($tree2);
is($tree2.parent(), $tree, '... $tree is now $tree2 parent');
ok(!($tree2.is_root()), '... this tree2 is no longer a root');
is($tree2.depth(), 0, '... $tree2 has a depth of 1');

ok($tree.is_root(), '... $tree is still a root');
ok(!($tree.is_leaf()), '... but $tree is no longer a leaf');
is($tree.child_count(), 1, '... $tree has a child count of 1');

my $tree3 = $tree.get_child(0);
is($tree3, $tree2, '... tree3 is the same as tree2');

is($tree.get_child(0).node(), 'my other tree', '... method chaining even works :)');

$tree.add_children(
    Tree.new(node => 'my tree 3'),
    Tree.new(node => 'my tree 4')
);

is($tree.get_child(1).node(), 'my tree 3', '... add_children worked');
is($tree.get_child(2).node(), 'my tree 4', '... add_children worked');

is($tree.child_count(), 3, '... $tree has a child count of 1');

$tree.get_child(1).add_child(Tree.new(node => 'tree 1.1'));
my $tree4 = $tree.get_child(1).get_child(0);
is($tree4.node(), 'tree 1.1', '... chained add_child worked');
is($tree4.depth(), 1, '... $tree4 has a depth of 2');

my $output;
$tree.traverse(-> $t {
    $output ~= ('--' x $t.depth()) ~ " " ~ $t.node() ~ "\n";
});
is($output, 
' my other tree
 my tree 3
-- tree 1.1
 my tree 4
', '... got the right output');


# test iterator version
eval {
my $iter = $tree.traverse_iter;

$output = "";

my $t;

while ($t = $iter()) {
    $output ~= ('--' x $t.depth()) ~ " " ~ $t.node() ~ "\n";
};

is($output, 
' my other tree
 my tree 3
-- tree 1.1
 my tree 4
', '... got the right output (iterator)');
};

fail "iterator/coroutine test", :todo<feature> if $!;
