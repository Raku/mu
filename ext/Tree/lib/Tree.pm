
use v6;

class Tree-0.0.2;

## ----------------------------------------------------------------------------
## attributes

has $.node is rw;

has $.depth;
has $.height;
has $.width;

has $:parent;
has @:children;

## ----------------------------------------------------------------------------
## constructors

submethod BUILD (?$node) {
    $.depth  = -1;
    $.height = 1;
    $.width  = 1;
    $.node   = $node if $node.defined;   
}

## ----------------------------------------------------------------------------
## accessors and mutators

method parent ($self:) returns Tree { 
    return $:parent;
}

method :_set_parent ($self: Tree $parent) returns Void {
    $:parent = $parent;
    $.depth  = $parent.depth() + 1;
}

## ----------------------------------------------------------------------------
## informational methods

method is_root returns Bool { $:parent.defined ?? 0 :: 1 }
method is_leaf returns Bool { +@:children == 0 }

method child_count returns Int { +@:children }

method size ($self:) returns Int {
    my $size = 1;
    for @:children -> $child {
        $size += $child.size();    
    }
    return $size;
}

## ----------------------------------------------------------------------------
## adding children 

method add_child ($self: Tree $child) returns Tree {
    $child.:_set_parent($self);   
    $self.:_set_height($child);
    $self.:_set_width($child);    
    $child.fix_depth() unless $child.is_leaf(); 
    @:children.push($child);
    $self;
}

method add_children ($self: *@children) returns Tree {
    for @children -> $child {
        $self.add_child($child);
    }
    $self;
}

## ----------------------------------------------------------------------------
## getting children

method get_child ($self: Int $index) returns Tree { @:children[$index] }
method get_all_children returns Array of Tree { @:children }

## ----------------------------------------------------------------------------
## inserting children

method insert_children ($self: Int $index, *@trees) returns Void {
    # check the bounds of our children 
    # against the index given
    ($index <= $self.child_count()) 
        || die "Index Out of Bounds : got ($index) expected no more than (" ~ $self.child_count() ~ ")";
    (@trees) 
        || die "Insufficient Arguments : no tree(s) to insert";    
    for @trees -> $tree {    
        $tree.:_set_parent($self);
        $self.:_set_height($tree);   
        $self.:_set_width($tree);                         
        $tree.fix_depth() unless $tree.is_leaf();
    }
    # if index is zero, use this optimization
    if ($index == 0) {
        @:children.unshift(@trees);
    }
    # otherwise do some heavy lifting here
    else {
        @:children = (
            @:children[0 .. ($index - 1)],
            @trees,
            @:children[$index .. (@:children - 1)],
        );
    }
}

# insert_child is really the same as
# insert_children, you are just inserting
# and array of one tree
our &Tree::insert_child ::= &Tree::insert_children;

## ----------------------------------------------------------------------------
## removing children

method remove_child_at ($self: Int $index) returns Tree {
    ($self.child_count() != 0) 
        || die "Illegal Operation : There are no children to remove";        
    # check the bounds of our children 
    # against the index given        
    ($index < $self.child_count()) 
        || die "Index Out of Bounds : got ($index) expected no more than (" ~ $self.child_count() ~ ")";        
    my $removed_child;
    # if index is zero, use this optimization    
    if ($index == 0) {
        $removed_child = @:children.shift;
    }
    # if index is equal to the number of children
    # then use this optimization    
    elsif ($index == +@:children) {
        $removed_child = @:children.pop();    
    }
    # otherwise do some heavy lifting here    
    else {
        $removed_child = @:children[$index];
        @:children = (
            @:children[0 .. ($index - 1)],
            @:children[($index + 1) .. (@:children - 1)],
        );
    }
    # make sure we fix the height
    $self.fix_height();
    $self.fix_width();    
    # make sure that the removed child
    # is no longer connected to the parent
    # so we change its parent to ROOT
    $removed_child.:_remove_parent();
    # and now we make sure that the depth 
    # of the removed child is aligned correctly
    $removed_child.fix_depth() unless $removed_child.is_leaf();    
    # return ths removed child
    # it is the responsibility 
    # of the user of this module
    # to properly dispose of this
    # child (and all its sub-children)
    return $removed_child;
}

method remove_child ($self: Tree $child_to_remove) returns Tree {
    my $index = 0;
    for @:children -> $child {
        ($child =:= $child_to_remove) && return $self.remove_child_at($index);
        $index++;
    }
    die "Child Not Found : cannot find object ($child_to_remove) in self";
}

## ----------------------------------------------------------------------------
## sibling methods

method get_sibling ($self: Int $index) returns Tree {
    (!$self.is_root()) 
        || die "Insufficient Arguments : cannot get siblings to a ROOT tree";
    $self.parent().get_child($index);
}

method  get_all_siblings ($self:) returns Array {
    (!$self.is_root()) 
        || die "Insufficient Arguments : cannot get siblings to a ROOT tree";
    $self.parent().get_all_children();
}

method add_sibling ($self: Tree $sibling) returns Tree {
    (!$self.is_root()) 
        || die "Insufficient Arguments : cannot add a sibling to a ROOT tree";
    $self.parent().add_child($sibling);
}

method add_siblings ($self: *@siblings) returns Tree {
    (!$self.is_root()) 
        || die "Insufficient Arguments : cannot add siblings to a ROOT tree";
    $self.parent().add_children(@siblings);
}

method insert_siblings ($self: Int $index, *@siblings) returns Tree {
    (!$self.is_root()) 
        || die "Insufficient Arguments : cannot insert siblings to a ROOT tree";
    $self.parent().insert_children($index, @siblings);
}

# insertSibling is really the same as
# insertSiblings, you are just inserting
# and array of one tree
our &Tree::insert_sibling ::= &Tree::insert_siblings;

# I am not permitting the removal of siblings 
# as I think in general it is a bad idea

## ----------------------------------------------------------------------------
## traversal

method traverse ($self: Code $func, Str ?$traversal_order) returns Void {
    if !$traversal_order.defined || $traversal_order.lc() eq 'pre_order' {
        $self.pre_order_traverse($func)
    }
    else {
        $self.post_order_traverse($func)
    }
}

method pre_order_traverse ($self: Code $func) returns Void {
    for @:children -> $child is rw {
        $func($child);
        $child.traverse($func);
    }
}

method post_order_traverse ($self: Code $func) returns Void {
    for @:children -> $child is rw {
        $child.traverse($func);
        $func($child);
    }
}

method traverse_iter($self: Str ?$traversal_order) returns Code {
    return coro {
        $self.traverse(sub { yield $^node }, $traversal_order);
    };
}

## ----------------------------------------------------------------------------
## utility methods

# NOTE:
# Occasionally one wants to have the 
# depth available for various reasons
# of convience. Sometimes that depth 
# field is not always correct.
# If you create your tree in a top-down
# manner, this is usually not an issue
# since each time you either add a child
# or create a tree you are doing it with 
# a single tree and not a hierarchy.
# If however you are creating your tree
# bottom-up, then you might find that 
# when adding hierarchies of trees, your
# depth fields are all out of whack.
# This is where this method comes into play
# it will recurse down the tree and fix the
# depth fields appropriately.
# This method is called automatically when 
# a subtree is added to a child array
method fix_depth ($self:) returns Void {
    # make sure the tree's depth 
    # is up to date all the way down
    $self.traverse(-> $t {
        $t.:_set_depth($t.parent().depth() + 1);
    });
}

# NOTE:
# This method is used to fix any height 
# discrepencies which might arise when 
# you remove a sub-tree
method fix_height ($self:) returns Void {
    # we must find the tallest sub-tree
    # and use that to define the height
    my $max_height = 0;
    unless ($self.is_leaf()) {
        for @:children -> $child is rw {
            my $child_height = $child.height();
            $max_height = $child_height if $max_height < $child_height;
        }
    }
    # if there is no change, then we 
    # need not bubble up through the
    # parents
    return if $.height == ($max_height + 1);
    # otherwise ...
    $.height = $max_height + 1;
    # now we need to bubble up through the parents 
    # in order to rectify any issues with height
    $self.parent().fix_height() unless $self.is_root();
}

method fix_width ($self:) {
    my $fixed_width = 0;
    for @:children -> $child is rw {
        $fixed_width += $child.width();
    }
    $.width = $fixed_width;
    $self.parent().fix_width() unless $self.is_root();
}

method get_index ($self:) returns Int {
    return -1 if $self.is_root();
    my $index = 0;
    for  $self.parent().get_all_children() -> $sibling {
        ($sibling =:= $self) && return $index;
        $index++;
    }
}

## ----------------------------------------------------------------------------
## private methods

method :_set_height ($self: Tree $child) returns Void {
    my $child_height = $child.height();
    return if $.height >= $child_height + 1;
    $.height = $child_height + 1;
    # and now bubble up to the parent (unless we are the root)
    $self.parent().:_set_height($self) unless $self.is_root();
}

method :_set_width ($self: Tree $child) returns Void {
    return if $.width > $self.child_count();    
    my $child_width = $child.width();
    $.width += $child_width;
    # and now bubble up to the parent (unless we are the root)
    $self.parent().:_set_width($self) unless $self.is_root();            
}

method :_set_depth ($self: Int $depth) returns Void { $.depth = $depth }

method :_remove_parent returns Void { $:parent = undef }

=pod

=head1 NAME

Tree - A basic I<n>-ary tree

=head1 SYNOPSIS

  use Tree;
  
  my $root = Tree.new(:node<0>).add_children(
      Tree.new(:node<1>).add_children(
          Tree.new(:node<1.1>),
          Tree.new(:node<1.2>),
          Tree.new(:node<1.3>),                
      ),
      Tree.new(:node<2>).add_children(
          Tree.new(:node<2.1>).add_children(
              Tree.new(:node<2.1.1>).add_children(
                  Tree.new(:node<2.1.1.1)
              ),
              Tree.new(:node<2.1.2>),             
          )
      )        
  );
  
  $root.traverse(-> $t {
      say(('    ' x $t.depth()) ~ $t.node())
  });

=head1 DESCRIPTION

=head1 METHODS

=over 4

=item B<new (?$node) returns Tree>

=item B<node ($self: ?$node)>

=item B<depth returns Int>

=item B<parent ($self: Tree ?$parent) returns Tree>

=item B<is_root returns Bool>

=item B<is_leaf returns Bool>

=item B<child_count returns Int>

=item B<add_child ($self: Tree $child) returns Tree>

=item B<add_children ($self: *@children) returns Tree>

=item B<get_child ($self: Int $index) returns Tree>

=item B<get_all_children returns Array>

=item B<traverse ($self: Code $func) returns Void>

=back

=head1 TODO

=over 4

=item Write more docs

=item Convert more tests

A good amount of Tree::Simple's test will not be relevant though.

=item Create a Visitor class

This would also be a good use for Roles.

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
