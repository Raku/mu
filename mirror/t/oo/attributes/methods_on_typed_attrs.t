#!/usr/bin/pugs

use v6;
use Test;

plan 5;

class Foo {
    has Bar $.bar;
    
    method call_bar returns Void {
        return $.bar.baz();
    }
}

class Bar {
    method baz returns Str { 'Baz' }
}

my $bar = Bar.new();
isa_ok($bar, 'Bar');

my $foo = Foo.new(:bar($bar));
isa_ok($foo, 'Foo');

# sanity test
is($bar.baz(), 'Baz', '... sanity test, this works as we expect');

my $val;
lives_ok {
    $val = $foo.call_bar();
}, '... this should work';
is($val, 'Baz', '... this should be "Baz"');
