
use v6;

class Tree;

sub tree (+$node) returns Tree is export { Tree.new(node => $node) }

has $:node;
has $:parent;
has $:depth;
has @:children;

method node ($self: ?$node) returns Any {
    $:node = $node if $node.defined;
    return $:node;
}

method depth returns Int { $.depth //= 0 }

method parent ($self: ?$parent) returns Tree { 
    if $parent.defined {
        $:parent = $parent;
        $:depth  = $parent.depth() + 1;
    }
    return $:parent;
}

method is_root returns Bool { ($.parent.defined ?? 0 :: 1) }
method is_leaf returns Bool { +@:children == 0 }

method child_count returns Int { +@:children }

method add_child ($self: $child) returns Void {
    $child.parent($self);    
    @:children.push($child);
}

method add_children ($self: *@children) returns Void {
    for @children -> $child {
        $self.add_child($child);
    }
}

method get_child ($self: $index) returns Tree { @:children[$index] }
method get_all_children ($self:) returns Array { @:children }

method traverse ($self: Code $func) returns Void {
    for @:children -> $child {
        $func($child);
        $child.traverse($func);
    }
}

