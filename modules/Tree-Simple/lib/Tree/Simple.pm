#!pugs
use v6;

class Tree::Simple-0.0.1;

has Any $:node;
has Int $:depth;
has Int $:height = 1;
has Int $:width  = 1;

has Tree::Simple $:parent;

has Array of Tree::Simple @:children;

## ----------------------------------------------------------------------------
## Exceptions

class InsufficientArguments is Exception {};
class IllegalOperation      is Exception {};
class IndexOutOfBounds      is Exception {};
class ChildNotFound         is Exception {};

## ----------------------------------------------------------------------------
## constructor

method new ($class: ?$node, Tree::Simple ?$parent) returns Tree::Simple {
    my $self = $class.bless();
    $self._init($node, $parent);
    return $self;
}

## ----------------------------------------------------------------------------
## private methods (??)

method _init (Any ?$node, Tree::Simple ?$parent) {
	$:node = $node if $node.defined;
	if ($parent.defined) {
        $parent.addChild($self);
	}
	else {
		$:depth = -1;
	}
}

method _setParent (Tree::Simple ?$parent) {
	unless ($parent.defined) {
		$:depth = -1;
	}
	else {
        $:parent = $parent;    
		$:depth  = $parent.getDepth() + 1;
	}
}

method _detachParent { $.parent = undef }

method _setHeight (Tree::Simple $child) {
    my $child_height = $child.getHeight();
    return if $:height >= ($child_height + 1);
    $:height = $child_height + 1;
    # and now bubble up to the parent (unless we are the root)
    .getParent()._setHeight($_) unless .isRoot();
}

method _setWidth ($child_width) {
    if (ref($child_width)) {
        return if $:width > .getChildCount();    
        $child_width = $child_width.getWidth();
    }
    $:width += $child_width;
    # and now bubble up to the parent (unless we are the root)
    .getParent()._setWidth($child_width) unless .isRoot();            
}

method _setDepth (Int $depth) { $:depth = $depth }

## ----------------------------------------------------------------------------
## mutators

method setNodeValue ($node_value) { $:node = $node_value }

## ----------------------------------------------------------------------------
## accessors

method getParent    returns Tree::Simple { $:parent }
method getNodeValue returns Any          { $:node   }
method getDepth     returns Int          { $:depth  }
method getHeight    returns Int          { $:height }
method getWidth     returns Int          { $:width  }

method getChildCount returns Int { +@:children }

method getChild (Int $index) returns Tree::Simple { @:children[$index] }

method getAllChildren returns Array of Tree::Simple { @:children }

method getSibling (Int $index) returns Tree::Simple {
	(!.isRoot()) || throw InsufficientArguments "cannot get siblings from a ROOT tree";
	.getParent().getChild($index);
}

method getAllSiblings returns Array of Tree::Simple {
	(!.isRoot()) || throw InsufficientArguments "cannot get siblings from a ROOT tree";	
	.getParent().getAllChildren();
}

## ----------------------------------------------------------------------------
## informational

method isLeaf returns Bool { ?(+@:children == 0) }
method isRoot returns Bool { !$.parent.defined   }

method size returns Int {
    my $size = 1;
    for .getAllChildren() -> $child {
        $size += $child.size();    
    }
    return $size;
}

## ----------------------------------------------
## child methods

method addChild (Tree::Simple $tree) returns Tree::Simple {
	$tree._setParent($_);
    ._setHeight($tree);
    ._setWidth($tree);    
	$tree.fixDepth() unless $tree.isLeaf();
	@:children.push($tree);	
	$self;
}

method addChildren (Array of Tree::Simple @trees) returns Tree::Simple {
	for @trees -> $tree { .addChild($tree) };
	$self;
}

method insertChildren (Int $index, Array of Tree::Simple @trees) {
	($index <= .getChildCount()) 
		|| throw IndexOutOfBounds "got ($index) expected no more than (" ~ .getChildCount() ~ ")";
	for @trees -> $tree {
		$tree._setParent($self);
        ._setHeight($tree);   
        ._setWidth($tree);                         
		$tree.fixDepth() unless $tree.isLeaf();
	}
	# if index is zero, use this optimization
	if ($index == 0) {
		@:children.unshift(@trees);
	}
	# otherwise do some heavy lifting here
	else {
		@:children = [
			@:children[0 .. ($index - 1)],
			@trees,
			@:children[$index .. +@:children],
			];
	}
}

method insertChild (Int $index, Tree::Simple $tree) { .insertChildren($index, ($tree)) }

method removeChildAt (Int $index) returns Tree::Simple {
	(.getChildCount() != 0) 
		|| throw IllegalOperation "There are no children to remove";		
	# check the bounds of our children 
	# against the index given		
	($index < .getChildCount()) 
		|| throw IndexOutOfBounds "got ($index) expected no more than (" ~ .getChildCount() ~ ")";		
	my $removed_child;
	# if index is zero, use this optimization	
	if ($index == 0) {
		$removed_child = @:children.shift;
	}
	# if index is equal to the number of children
	# then use this optimization	
	elsif ($index == +@:children) {
		$removed_child = @:children.pop;	
	}
	# otherwise do some heavy lifting here	
	else {
		$removed_child = @:children[$index];
		@:children = [
			@:children[0 .. ($index - 1)],
			@:children[($index + 1) .. $#{$self->{_children}}],
			];
	}
    # make sure we fix the height
    .fixHeight();
    .fixWidth();    
	# make sure that the removed child
	# is no longer connected to the parent
	# so we change its parent to ROOT
	$removed_child._detachParent();
	# and now we make sure that the depth 
	# of the removed child is aligned correctly
	$removed_child.fixDepth() unless $removed_child.isLeaf();	
	# return ths removed child
	# it is the responsibility 
	# of the user of this module
	# to properly dispose of this
	# child (and all its sub-children)
	return $removed_child;
}

method removeChild (Tree::Simple $child_to_remove) return Tree::Simple {
    my $index = 0;
    for .getAllChildren() -> $child {
        ($child =:= $child_to_remove) && return .removeChildAt($index);
        $index++;
    }
    throw ChildNotFound "cannot find object ($child_to_remove) in self";
}

method getIndex returns Int {
    return -1 unless $.parent.defined;
    my $index = 0;
    for $parent.getAllChildren() -> $sibling {
        ($sibling =:= $_) && return $index;
        $index++;
    }
    return -1;
}

## ----------------------------------------------
## Sibling methods

# these addSibling and addSiblings functions 
# just pass along their arguments to the addChild
# and addChildren method respectively, this 
# eliminates the need to overload these method
# in things like the Keyable Tree object

method addSibling (Tree::Simple $tree) {
	(!.isRoot()) || throw InsufficientArguments "cannot add a sibling to a ROOT tree";
	$:parent.addChild($tree);
}

method addSiblings (Array of Tree::Simple @trees) {
	(!.isRoot()) || throw InsufficientArguments "cannot add siblings to a ROOT tree";
	$:parent.addChildren(@trees);
}

method insertSiblings (Int $index, Array of Tree::Simple @trees) {
	(!.isRoot()) || throw InsufficientArguments "cannot insert sibling(s) to a ROOT tree";
	$:parent.insertChildren($index, @trees);
}

method insertSibling (Int $index, Tree::Simple $tree) {
	(!.isRoot()) || throw InsufficientArguments "cannot insert sibling(s) to a ROOT tree";
	$:parent.insertChild($index, $tree);    
}

# I am not permitting the removal of siblings 
# as I think in general it is a bad idea

## ----------------------------------------------------------------------------
## misc

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
method fixDepth {
	# make sure the tree's depth 
	# is up to date all the way down
	.traverse(sub ($tree) {
            return if $tree.isRoot();
			$tree._setDepth($tree.getParent().getDepth() + 1);
		}
	);
}

# NOTE:
# This method is used to fix any height 
# discrepencies which might arise when 
# you remove a sub-tree
method fixHeight {
    # we must find the tallest sub-tree
    # and use that to define the height
    my $max_height = 0;
    unless (.isLeaf()) {
        for .getAllChildren() -> $child {
            my $child_height = $child.getHeight();
            $max_height = $child_height if ($max_height < $child_height);
        }
    }
    # if there is no change, then we 
    # need not bubble up through the
    # parents
    return if ($:height == ($max_height + 1));
    # otherwise ...
    $:height = $max_height + 1;
    # now we need to bubble up through the parents 
    # in order to rectify any issues with height
    .getParent().fixHeight() unless .isRoot();
}

method fixWidth {
    my $fixed_width = 0;
    for .getAllChildren() -> $child { 
        $fixed_width += $child.getWidth() 
    }
    $:width = $fixed_width;
    .getParent().fixWidth() unless .isRoot();
}

method traverse (Code $func) {
	for .getAllChildren() -> $child { 
		$func($child);
		$child.traverse($func);
	}
}

method accept (Tree::Simple::Visitor $visitor) { $visitor.visit($_) }

## ----------------------------------------------------------------------------
## cloning 

method clone returns Tree::Simple {
    # first clone the value in the node
    my $cloned_node = _cloneNode(.getNodeValue());
    # create a new Tree::Simple object 
    # here with the cloned node, however
    # we do not assign the parent node
    # since it really does not make a lot
    # of sense. To properly clone it would
    # be to clone back up the tree as well,
    # which IMO is not intuitive. So in essence
    # when you clone a tree, you detach it from
    # any parentage it might have
    my $clone = .new($cloned_node);
    # however, because it is a recursive thing
    # when you clone all the children, and then
    # add them to the clone, you end up setting
    # the parent of the children to be that of
    # the clone (which is correct)
    $clone.addChildren(
                .getAllChildren().map:{ $_.clone() } 
                ) unless .isLeaf();
    # return the clone            
    return $clone;
}
    
# this allows cloning of single nodes while 
# retaining connections to a tree, this is sloppy
method cloneShallow returns Tree::Simple { .new(._cloneNode(.getNodeValue())) }

# this is a helper function which 
# recursively clones the node
method _cloneNode {
    die "TODO: I am not sure how this kind of method would work in Perl6";
}


## ----------------------------------------------------------------------------
## Desctructor

method DESTROY {
    # we want to detach all our children from 
    # ourselves, this will break most of the 
    # connections and allow for things to get
    # reaped properly
	unless (!@:children && +@:children == 0) {
		for @:children -> $child { 
            $child.defined && $child._detachParent();
		}
	}
    # we do not need to remove or undef the _children
    # of the _parent fields, this will cause some 
    # unwanted releasing of connections. 
}

## ----------------------------------------------------------------------------
## end Tree::Simple
## ----------------------------------------------------------------------------

1;

__END__

=head1 NAME

Tree::Simple - A simple tree object

=head1 SYNOPSIS

  use Tree::Simple;
  
  # make a tree root
  my $tree = Tree::Simple->new("0", Tree::Simple->ROOT);
  
  # explicity add a child to it
  $tree->addChild(Tree::Simple->new("1"));
  
  # specify the parent when creating
  # an instance and it adds the child implicity
  my $sub_tree = Tree::Simple->new("2", $tree);
  
  # chain method calls
  $tree->getChild(0)->addChild(Tree::Simple->new("1.1"));
  
  # add more than one child at a time
  $sub_tree->addChildren(
            Tree::Simple->new("2.1"),
            Tree::Simple->new("2.2")
            );

  # add siblings
  $sub_tree->addSibling(Tree::Simple->new("3"));
  
  # insert children a specified index
  $sub_tree->insertChild(1, Tree::Simple->new("2.1a"));
  
  # clean up circular references
  $tree->DESTROY();

=head1 DESCRIPTION

This module in an fully object-oriented implementation of a simple n-ary tree. It is built upon the concept of parent-child relationships, so therefore every B<Tree::Simple> object has both a parent and a set of children (who themselves may have children, and so on). Every B<Tree::Simple> object also has siblings, as they are just the children of their immediate parent. 

It is can be used to model hierarchal information such as a file-system, the organizational structure of a company, an object inheritance hierarchy, versioned files from a version control system or even an abstract syntax tree for use in a parser. It makes no assumptions as to your intended usage, but instead simply provides the structure and means of accessing and traversing said structure. 

This module uses exceptions and a minimal Design By Contract style. All method arguments are required unless specified in the documentation, if a required argument is not defined an exception will usually be thrown. Many arguments are also required to be of a specific type, for instance the C<$parent> argument to the constructor B<must> be a B<Tree::Simple> object or an object derived from B<Tree::Simple>, otherwise an exception is thrown. This may seems harsh to some, but this allows me to have the confidence that my code works as I intend, and for you to enjoy the same level of confidence when using this module. Note however that this module does not use any Exception or Error module, the exceptions are just strings thrown with C<die>. 

I consider this module to be production stable, it is based on a module which has been in use on a few production systems for approx. 2 years now with no issue. The only difference is that the code has been cleaned up a bit, comments added and the thorough tests written for its public release. I am confident it behaves as I would expect it to, and is (as far as I know) bug-free. I have not stress-tested it under extreme duress, but I don't so much intend for it to be used in that type of situation. If this module cannot keep up with your Tree needs, i suggest switching to one of the modules listed in the L<OTHER TREE MODULES> section below.

=head1 CONSTANTS

=over 4

=item B<ROOT>

This class constant serves as a placeholder for the root of our tree. If a tree does not have a parent, then it is considered a root. 

=back

=head1 METHODS

=head2 Constructor

=over 4

=item B<new ($node, $parent)>

The constructor accepts two arguments a C<$node> value and an optional C<$parent>. The C<$node> value can be any scalar value (which includes references and objects). The optional C<$parent> value must be a B<Tree::Simple> object, or an object derived from B<Tree::Simple>. Setting this value implies that your new tree is a child of the parent tree, and therefore adds it to the parent's children. If the C<$parent> is not specified then its value defaults to ROOT.

=back

=head2 Mutator Methods

=over 4

=item B<setNodeValue ($node_value)>

This sets the node value to the scalar C<$node_value>, an exception is thrown if C<$node_value> is not defined.

=item B<setUID ($uid)>

This allows you to set your own unique ID for this specific Tree::Simple object. A default value derived from the object's hex address is provided for you, so use of this method is entirely optional. It is the responsibility of the user to ensure the value's uniqueness, all that is tested by this method is that C<$uid> is a true value (evaluates to true in a boolean context). For even more information about the Tree::Simple UID see the C<getUID> method.

=item B<addChild ($tree)>

This method accepts only B<Tree::Simple> objects or objects derived from B<Tree::Simple>, an exception is thrown otherwise. This method will append the given C<$tree> to the end of it's children list, and set up the correct parent-child relationships. This method is set up to return its invocant so that method call chaining can be possible. Such as:

  my $tree = Tree::Simple->new("root")->addChild(Tree::Simple->new("child one"));

Or the more complex:

  my $tree = Tree::Simple->new("root")->addChild(
                         Tree::Simple->new("1.0")->addChild(
                                     Tree::Simple->new("1.0.1")     
                                     )
                         );

=item B<addChildren (@trees)>

This method accepts an array of B<Tree::Simple> objects, and adds them to it's children list. Like C<addChild> this method will return its invocant to allow for method call chaining.

=item B<insertChild ($index, $tree)>

This method accepts a numeric C<$index> and a B<Tree::Simple> object (C<$tree>), and inserts the C<$tree> into the children list at the specified C<$index>. This results in the shifting down of all children after the C<$index>. The C<$index> is checked to be sure it is the bounds of the child list, if it out of bounds an exception is thrown. The C<$tree> argument's type is verified to be a B<Tree::Simple> or B<Tree::Simple> derived object, if this condition fails, an exception is thrown. 

=item B<insertChildren ($index, @trees)>

This method functions much as insertChild does, but instead of inserting a single B<Tree::Simple>, it inserts an array of B<Tree::Simple> objects. It too bounds checks the value of C<$index> and type checks the objects in C<@trees> just as C<insertChild> does.

=item B<removeChild> ($child | $index)>

Accepts two different arguemnts. If given a B<Tree::Simple> object (C<$child>), this method finds that specific C<$child> by comparing it with all the other children until it finds a match. At which point the C<$child> is removed. If no match is found, and exception is thrown. If a non-B<Tree::Simple> object is given as the C<$child> argument, an exception is thrown. 

This method also accepts a numeric C<$index> and removes the child found at that index from it's list of children. The C<$index> is bounds checked, if this condition fail, an exception is thrown.

When a child is removed, it results in the shifting up of all children after it, and the removed child is returned. The removed child is properly disconnected from the tree and all its references to its old parent are removed. However, in order to properly clean up and circular references the removed child might have, it is advised to call it's C<DESTROY> method. See the L<CIRCULAR REFERENCES> section for more information.

=item B<addSibling ($tree)>

=item B<addSiblings (@trees)>

=item B<insertSibling ($index, $tree)>

=item B<insertSiblings ($index, @trees)>

The C<addSibling>, C<addSiblings>, C<insertSibling> and C<insertSiblings> methods pass along their arguments to the C<addChild>, C<addChildren>, C<insertChild> and C<insertChildren> methods of their parent object respectively. This eliminates the need to overload these methods in subclasses which may have specialized versions of the *Child(ren) methods. The one exceptions is that if an attempt it made to add or insert siblings to the B<ROOT> of the tree then an exception is thrown.

=back

B<NOTE:>
There is no C<removeSibling> method as I felt it was probably a bad idea. The same effect can be achieved by manual upwards traversal. 

=head2 Accessor Methods

=over 4

=item B<getNodeValue>

This returns the value stored in the object's node field.

=item B<getUID>

This returns the unique ID associated with this particular tree. This can be custom set using the C<setUID> method, or you can just use the default. The default is the hex-address extracted from the stringified Tree::Simple object. This may not be a I<universally> unique identifier, but it should be adequate for at least the current instance of your perl interpreter. If you need a UUID, one can be generated with an outside module (there are many to choose from on CPAN) and the C<setUID> method (see above).

=item B<getChild ($index)>

This returns the child (a B<Tree::Simple> object) found at the specified C<$index>. Note that we do use standard zero-based array indexing.

=item B<getAllChildren>

This returns an array of all the children (all B<Tree::Simple> objects). It will return an array reference in scalar context. 

=item B<getSibling ($index)>

=item B<getAllSiblings>

Much like C<addSibling> and C<addSiblings>, these two methods simply call C<getChild> and C<getAllChildren> on the invocant's parent.

=item B<getDepth>

Returns a number representing the invocant's depth within the hierarchy of B<Tree::Simple> objects. 

B<NOTE:> A C<ROOT> tree has the depth of -1. This be because Tree::Simple assumes that a tree's root will usually not contain data, but just be an anchor for the data-containing branches. This may not be intuitive in all cases, so I mention it here.

=item B<getParent>

Returns the invocant's parent, which could be either B<ROOT> or a B<Tree::Simple> object.

=item B<getHeight>

Returns a number representing the length of the longest path from the current tree to the furthest leaf node.

=item B<getWidth>

Returns the a number representing the breadth of the current tree, basically it is a count of all the leaf nodes.

=item B<getChildCount>

Returns the number of children the invocant contains.

=item B<getIndex>

Returns the index of this tree within its parent's child list. Returns -1 if the tree is the root.

=back

=head2 Predicate Methods

=over 4

=item B<isLeaf>

Returns true (1) if the invocant does not have any children, false (0) otherwise.

=item B<isRoot>

Returns true (1) if the invocant's "parent" field is B<ROOT>, returns false (0) otherwise.

=back

=head2 Recursive Methods

=over 4

=item B<traverse ($func)>

This method takes a single argument of a subroutine reference C<$func>. If the argument is not defined and is not in fact a CODE reference then an exception is thrown. The function is then applied recursively to all the children of the invocant. Here is an example of a traversal function that will print out the hierarchy as a tabbed in list.

  $tree->traverse(sub {
        my ($_tree) = @_;
        print (("\t" x $_tree->getDepth()), $_tree->getNodeValue(), "\n");
        });
        
=item B<size>

Returns the total number of nodes in the current tree and all its sub-trees.

=item B<height>

This method has also been B<deprecated> in favor of the C<getHeight> method above, it remains as an alias to C<getHeight> for backwards compatability. 

B<NOTE:> This is also no longer a recursive method which get's it's value on demand, but a value stored in the Tree::Simple object itself, hopefully making it much more efficient and usable.

=back

=head2 Visitor Methods

=over 4     

=item B<accept ($visitor)>

It accepts either a B<Tree::Simple::Visitor> object (which includes classes derived from B<Tree::Simple::Visitor>), or an object who has the C<visit> method available (tested with C<$visitor-E<gt>can('visit')>). If these qualifications are not met, and exception will be thrown. We then run the Visitor's C<visit> method giving the current tree as its argument. 

I have also created a number of Visitor objects and packaged them into the B<Tree::Simple::VisitorFactory>. 

=back

=head2 Cloning Methods

Cloning a tree can be an extremly expensive operation for large trees, so we provide two options for cloning, a deep clone and a shallow clone.

When a Tree::Simple object is cloned, the node is deep-copied in the following manner. If we find a normal scalar value (non-reference), we simply copy it. If we find an object, we attempt to call C<clone> on it, otherwise we just copy the reference (since we assume the object does not want to be cloned). If we find a SCALAR, REF reference we copy the value contained within it. If we find a HASH or ARRAY reference we copy the reference and recursively copy all the elements within it (following these exact guidelines). We also do our best to assure that circular references are cloned only once and connections restored correctly. This cloning will not be able to copy CODE, RegExp and GLOB references, as they are pretty much impossible to clone. We also do not handle C<tied> objects, and they will simply be copied as plain references, and not re-C<tied>. 

=over 4

=item B<clone>

The clone method does a full deep-copy clone of the object, calling C<clone> recursively on all its children. This does not call C<clone> on the parent tree however. Doing this would result in a slowly degenerating spiral of recursive death, so it is not recommended and therefore not implemented. What happens is that the tree instance that C<clone> is actually called upon is detached from the tree, and becomes a root node, all if the cloned children are then attached as children of that tree. I personally think this is more intuitive then to have the cloning crawl back I<up> the tree is not what I think most people would expect. 

=item B<cloneShallow>

This method is an alternate option to the plain C<clone> method. This method allows the cloning of single B<Tree::Simple> object while retaining connections to the rest of the tree/hierarchy.

=back

=head2 Misc. Methods

=over 4

=item B<DESTROY>

To avoid memory leaks through uncleaned-up circular references, we implement the C<DESTROY> method. This method will attempt to call C<DESTROY> on each of its children (if it has any). This will result in a cascade of calls to C<DESTROY> on down the tree. It also cleans up it's parental relations as well. 

Because of perl's reference counting scheme and how that interacts with circular references, if you want an object to be properly reaped you should manually call C<DESTROY>. This is especially nessecary if your object has any children. See the section on L<CIRCULAR REFERENCES> for more information.

=item B<fixDepth>

Tree::Simple will manage your tree's depth field for you using this method. You should never need to call it on your own, however if you ever did need to, here is it. Running this method will traverse your all the invocant's sub-trees correcting the depth as it goes.

=item B<fixHeight>

Tree::Simple will manage your tree's height field for you using this method. You should never need to call it on your own, however if you ever did need to, here is it. Running this method will correct the heights of the current tree and all it's ancestors.

=item B<fixWidth>

Tree::Simple will manage your tree's width field for you using this method. You should never need to call it on your own, however if you ever did need to, here is it. Running this method will correct the widths of the current tree and all it's ancestors.

=back

=head2 Private Methods

I would not normally document private methods, but in case you need to subclass Tree::Simple, here they are.

=over 4

=item B<_init ($node, $parent, $children)>

This method is here largely to facilitate subclassing. This method is called by new to initialize the object, where new's primary responsibility is creating the instance.

=item B<_setParent ($parent)>

This method sets up the parental relationship. It is for internal use only.

=item B<_setHeight ($child)>

This method will set the height field based upon the height of the given C<$child>.

=back

=head1 CIRCULAR REFERENCES

I have revised the model by which Tree::Simple deals with ciruclar references. In the past all circular references had to be manually destroyed by calling DESTROY. The call to DESTROY would then call DESTROY on all the children, and therefore cascade down the tree. This however was not always what was needed, nor what made sense, so I have now revised the model to handle things in what I feel is a more consistent and sane way. 

Circular references are now managed with the simple idea that the parent makes the descisions for the child. This means that child-to-parent references are weak, while parent-to-child references are strong. So if a parent is destroyed it will force all it's children to detach from it, however, if a child is destroyed it will not be detached from it's parent.

=head2 Optional Weak References

By default, you are still required to call DESTROY in order for things to happen. However I have now added the option to use weak references, which alleviates the need for the manual call to DESTROY and allows Tree::Simple to manage this automatically. This is accomplished with a compile time setting like this:

  use Tree::Simple 'use_weak_refs';
  
And from that point on Tree::Simple will use weak references to allow for perl's reference counting to clean things up properly.

For those who are unfamilar with weak references, and how they affect the reference counts, here is a simple illustration. First is the normal model that Tree::Simple uses:
 
 +---------------+
 | Tree::Simple1 |<---------------------+
 +---------------+                      |
 | parent        |                      |
 | children      |-+                    |
 +---------------+ |                    |
                   |                    |
                   |  +---------------+ |
                   +->| Tree::Simple2 | |
                      +---------------+ |
                      | parent        |-+
                      | children      |
                      +---------------+
                      
Here, Tree::Simple1 has a reference count of 2 (one for the original variable it is assigned to, and one for the parent reference in Tree::Simple2), and Tree::Simple2 has a reference count of 1 (for the child reference in Tree::Simple2).                       
                     
Now, with weak references:
                     
 +---------------+
 | Tree::Simple1 |.......................
 +---------------+                      :
 | parent        |                      :
 | children      |-+                    : <--[ weak reference ]
 +---------------+ |                    :
                   |                    :
                   |  +---------------+ :
                   +->| Tree::Simple2 | :
                      +---------------+ :
                      | parent        |..
                      | children      |
                      +---------------+   
                      
Now Tree::Simple1 has a reference count of 1 (for the variable it is assigned to) and 1 weakened reference (for the parent reference in Tree::Simple2). And Tree::Simple2 has a reference count of 1, just as before.                                                            

=head1 BUGS

None that I am aware of. The code is pretty thoroughly tested (see L<CODE COVERAGE> below) and is based on an (non-publicly released) module which I had used in production systems for about 2 years without incident. Of course, if you find a bug, let me know, and I will be sure to fix it. 

=head1 CODE COVERAGE

I use L<Devel::Cover> to test the code coverage of my tests, below is the L<Devel::Cover> report on this module's test suite.
 
 ---------------------------- ------ ------ ------ ------ ------ ------ ------
 File                           stmt branch   cond    sub    pod   time  total
 ---------------------------- ------ ------ ------ ------ ------ ------ ------
 Tree/Simple.pm                 99.6   96.0   92.3  100.0   97.0   95.5   98.0
 Tree/Simple/Visitor.pm        100.0   96.2   88.2  100.0  100.0    4.5   97.7
 ---------------------------- ------ ------ ------ ------ ------ ------ ------
 Total                          99.7   96.1   91.1  100.0   97.6  100.0   97.9
 ---------------------------- ------ ------ ------ ------ ------ ------ ------

=head1 SEE ALSO

I have written a number of other modules which use or augment this module, they are describes below and available on CPAN.

=over 4

=item L<Tree::Parser> - A module for parsing formatted files into Tree::Simple hierarchies.

=item L<Tree::Simple::View> - A set of classes for viewing Tree::Simple hierarchies in various output formats.

=item L<Tree::Simple::VisitorFactory> - A set of several useful Visitor objects for Tree::Simple objects.

=item L<Tree::Binary> - If you are looking for a binary tree, this you might want to check this one out.

=back

Also, the author of L<Data::TreeDumper> and I have worked together to make sure that B<Tree::Simple> and his module work well together. If you need a quick and handy way to dump out a Tree::Simple heirarchy, this module does an excellent job (and plenty more as well).

I have also recently stumbled upon some packaged distributions of Tree::Simple for the various Unix flavors. Here  are some links:

=over 4

=item FreeBSD Port - L<http://www.freshports.org/devel/p5-Tree-Simple/>

=item Debian Package - L<http://packages.debian.org/unstable/perl/libtree-simple-perl>

=item Linux RPM - L<http://rpmpan.sourceforge.net/Tree.html>

=back

=head1 OTHER TREE MODULES

There are a few other Tree modules out there, here is a quick comparison between B<Tree::Simple> and them. Obviously I am biased, so take what I say with a grain of salt, and keep in mind, I wrote B<Tree::Simple> because I could not find a Tree module that suited my needs. If B<Tree::Simple> does not fit your needs, I recommend looking at these modules. Please note that I am only listing Tree::* modules I am familiar with here, if you think I have missed a module, please let me know. I have also seen a few tree-ish modules outside of the Tree::* namespace, but most of them are part of another distribution (B<HTML::Tree>, B<Pod::Tree>, etc) and are likely specialized in purpose. 

=over 4

=item L<Tree::DAG_Node>

This module seems pretty stable and very robust with a lot of functionality. However, B<Tree::DAG_Node> does not come with any automated tests. It's I<test.pl> file simply checks the module loads and nothing else. While I am sure the author tested his code, I would feel better if I was able to see that. The module is approx. 3000 lines with POD, and 1,500 without the POD. The shear depth and detail of the documentation and the ratio of code to documentation is impressive, and not to be taken lightly. But given that it is a well known fact that the likeliness of bugs increases along side the size of the code, I do not feel comfortable with large modules like this which have no tests.

All this said, I am not a huge fan of the API either, I prefer the gender neutral approach in B<Tree::Simple> to the mother/daughter style of B<Tree::DAG_Node>. I also feel very strongly that B<Tree::DAG_Node> is trying to do much more than makes sense in a single module, and is offering too many ways to do the same or similar things. 

However, of all the Tree::* modules out there, B<Tree::DAG_Node> seems to be one of the favorites, so it may be worth investigating.

=item L<Tree::MultiNode>

I am not very familiar with this module, however, I have heard some good reviews of it, so I thought it deserved mention here. I believe it is based upon C++ code found in the book I<Algorithms in C++> by Robert Sedgwick. It uses a number of interesting ideas, such as a ::Handle object to traverse the tree with (similar to Visitors, but also seem to be to be kind of like a cursor). However, like B<Tree::DAG_Node>, it is somewhat lacking in tests and has only 6 tests in its suite. It also has one glaring bug, which is that there is currently no way to remove a child node.

=item L<Tree::Nary>

It is a (somewhat) direct translation of the N-ary tree from the GLIB library, and the API is based on that. GLIB is a C library, which means this is a very C-ish API. That doesn't appeal to me, it might to you, to each their own.

This module is similar in intent to B<Tree::Simple>. It implements a tree with I<n> branches and has polymorphic node containers. It implements much of the same methods as B<Tree::Simple> and a few others on top of that, but being based on a C library, is not very OO. In most of the method calls the C<$self> argument is not used and the second argument C<$node> is. B<Tree::Simple> is a much more OO module than B<Tree::Nary>, so while they are similar in functionality they greatly differ in implementation style.

=item L<Tree>

This module is pretty old, it has not been updated since Oct. 31, 1999 and is still on version 0.01. It also seems to be (from the limited documentation) a binary and a balanced binary tree, B<Tree::Simple> is an I<n>-ary tree, and makes no attempt to balance anything.

=item L<Tree::Ternary>

This module is older than B<Tree>, last update was Sept. 24th, 1999. It seems to be a special purpose tree, for storing and accessing strings, not general purpose like B<Tree::Simple>. 

=item L<Tree::Ternary_XS>

This module is an XS implementation of the above tree type. 

=item L<Tree::Trie>

This too is a specialized tree type, it sounds similar to the B<Tree::Ternary>, but it much newer (latest release in 2003). It seems specialized for the lookup and retrieval of information like a hash.

=item L<Tree::M>

Is a wrapper for a C++ library, whereas B<Tree::Simple> is pure-perl. It also seems to be a more specialized implementation of a tree, therefore not really the same as B<Tree::Simple>. 

=item L<Tree::Fat>

Is a wrapper around a C library, again B<Tree::Simple> is pure-perl. The author describes FAT-trees as a combination of a Tree and an array. It looks like a pretty mean and lean module, and good if you need speed and are implementing a custom data-store of some kind. The author points out too that the module is designed for embedding and there is not default embedding, so you can't really use it "out of the box".

=back

=head1 ACKNOWLEDGEMENTS

=over 4

=item Thanks to Nadim Ibn Hamouda El Khemir for making L<Data::TreeDumper> work with B<Tree::Simple>.

=item Thanks to Brett Nuske for his idea for the C<getUID> and C<setUID> methods.

=item Thanks to whomever submitted the memory leak bug to RT (#7512). 

=item Thanks to Mark Thomas for his insight into how to best handle the I<height> and I<width> properties without unessecary recursion.

=back

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2004 by Infinity Interactive, Inc.

L<http://www.iinteractive.com>

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
