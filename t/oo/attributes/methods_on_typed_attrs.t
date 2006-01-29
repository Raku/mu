#!/usr/bin/pugs

use v6;
use Test;

plan 7;

class Foo {
    has Bar $.bar;
    
    method call_bar returns Void {
        return $.bar.baz();
    }

    method call_bar_indirectly returns Void {
        my $bar = $.bar;
        return $bar.baz();
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
lives_ok { $val = $foo.call_bar() }, '... this should work', :todo<bug>;
is $val, 'Baz', '... this should be "Baz"', :todo<bug>;

my $val2;
lives_ok { $val2 = $foo.call_bar_indirectly() }, '... this should work';
is($val2, 'Baz', '... this should be "Baz"');
