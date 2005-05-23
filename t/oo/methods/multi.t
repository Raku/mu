#!/usr/bin/pugs

use v6;
use Test;

plan 10;

class Foo {

    multi method bar($self:) {
        return "Foo.bar() called with no args";
    }

    multi method bar($self: Int $int) {
        return "Foo.bar() called with Int : $int";
    }
    
    multi method bar($self: Num $num) {
        return "Foo.bar() called with Num : $num";
    }    
    
    multi method bar($self: Rat $rat) {
        return "Foo.bar() called with Rat : $rat";
    }    
    
    multi method bar($self: Str $str) {
        return "Foo.bar() called with Str : $str";
    } 
       
    multi method bar($self: Bool $bool) {
        return "Foo.bar() called with Bool : {$bool.perl}";
    } 

    multi method bar($self: Sub $sub) {
        return "Foo.bar() called with Sub : {$sub()}";
    } 

    multi method bar($self: Array @array) {
        return "Foo.bar() called with Array : { join(', ', @array) }";
    } 

    multi method bar($self: Hash %hash) {
        return "Foo.bar() called with Hash : { join(', ', %hash.keys) }";
    } 
    
    multi method bar($self: IO $fh) {
        return "Foo.bar() called with IO";
    }     

}


my $foo = Foo.new();
is($foo.bar(), 'Foo.bar() called with no args', '... multi-method dispatched on no args');

is($foo.bar(5), 'Foo.bar() called with Int : 5', '... multi-method dispatched on Int');
my $num = '4';
is($foo.bar(+$num), 'Foo.bar() called with Num : 4', '... multi-method dispatched on Num');
is($foo.bar(1.5), 'Foo.bar() called with Rat : 1.5', '... multi-method dispatched on Rat');

is($foo.bar("Hello"), 'Foo.bar() called with Str : Hello', '... multi-method dispatched on Str');
is($foo.bar(1 == 1), 'Foo.bar() called with Bool : \bool::true', '... multi-method dispatched on Bool');

is($foo.bar(sub { "my sub" }), 'Foo.bar() called with Sub : my sub', '... multi-method dispatched on Sub', :todo<bug>);

my @array = ('foo', 'bar', 'baz');
is($foo.bar(@array), 'Foo.bar() called with Array : foo, bar, baz', '... multi-method dispatched on Array');

my %hash = ('foo' => 1, 'bar' => 2, 'baz' => 3);
is($foo.bar(%hash), 'Foo.bar() called with Hash : foo, bar, baz', '... multi-method dispatched on Array', :todo<bug>);

is($foo.bar($*ERR), 'Foo.bar() called with IO', '... multi-method dispatched on IO');
