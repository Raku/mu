
use v6;
use Test;
plan 7;

use Tree;

class Birch is Tree {
    has $.bark;
    
    submethod BUILD (Str $.bark) {}
}

my $birch = Birch.new( :bark("thick"), :node("test") );

isa_ok($birch, "Birch", "New birch");
is($birch.bark, "thick", "looks like a birch!");
is($birch.node, "test", "looks like a tree!");

ok($birch.isa(Tree), "New birch is a tree");

$birch.add_child(Birch.new( :bark("thin"), :node("foo")));

my @found;

my $child = $birch.get_child(0);

is($child.bark, "thin", "looks like a tree!");
is($child.node, "foo", "looks like a tree!");

# bug - can't match return context to method
is($birch.get_child(0).node, "foo", "looks like a tree!");

# doesn't work - see t/oo/inheritance.t
#is($birch.get_child(0).bark, "thin", "looks like a Birch!");
