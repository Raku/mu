#!/usr/bin/pugs

use v6;
use Test;
plan 1;

use Tree;

flunk("This test is totally foobarred (something about submethod BUILD MMD i think)", :todo<bug>);

=pod

class Birch is Tree {
    has $.bark;
    
    submethod BUILD (Str $bark) {
        $.bark = $bark;
    }
}

my $birch;
lives_ok {
    $birch = Birch.new( :bark("thick"), :node("test") );
} '... created the Tree subclass correctly';

isa_ok($birch, "Birch", "New birch");
is($birch.bark, "thick", "looks like a birch!");
is($birch.node, "test", "looks like a tree!");

ok($birch.isa(Tree), "New birch is a tree");

lives_ok {
    $birch.add_child(Birch.new( :bark("thin"), :node("foo")));
} '... added the child subclass okay';

my @found;

my $child;
lives_ok {
    $child = $birch.get_child(0);
} '... fetching the child';

is($child.bark, "thin", "looks like a tree!");
is($child.node, "foo", "looks like a tree!");

# bug - can't match return context to method
#is($birch.get_child(0).node, "foo", "looks like a tree!");

# doesn't work - see t/oo/inheritance.t
#is($birch.get_child(0).bark, "thin", "looks like a Birch!");

=cut
