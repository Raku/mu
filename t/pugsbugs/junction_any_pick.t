#!/usr/bin/pugs

use v6;
require Test;

plan 2;

=pod

This code shows the bug:
    pugs -e 'my @a = ("one", "two", "three"); sub foo (@a) { my $t = any(@a).pick(); say ref($t); say $t }; foo(@a)'

prints out:
    Array
    one two three

But this code:
    pugs -e 'my @a = ("one", "two", "three"); my $t = any(@a).pick(); say ref($t); say $t'

prints out:
    Str
    ("one" "two" or "three")
    
And this code:
    pugs -e 'sub foo2 { any(1 .. 10).pick() }; say ref(foo2());'

prints out:
    Int
    
=cut

my @test_string = ("one", "two", "three"); 
my @test_int = 1 .. 10;

sub foo (@a) { any(@b).pick() }

isa_ok(foo(@test_string), 'Str');
isa_ok(foo(@test_int), 'Int');