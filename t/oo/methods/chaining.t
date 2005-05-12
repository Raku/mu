#!/usr/bin/pugs

use v6;
use Test;

plan 12;

class Foo {
    has $.num;
    
    method bar ($self: $num) returns Foo {
        $.num = $num; 
        return $self;
    }
    
    method baz ($self: $num) returns Foo {
        $.num += $num;
        return $self;
    }
}

my $foo = Foo.new(:num<10>);
isa_ok($foo, 'Foo');

# do some sanity checking to make sure it does 
# all that we expect it too first.

is($foo.num(), 10, '... got the right num value');

my $_foo1 = $foo.bar(20);
isa_ok($_foo1, 'Foo');
ok($_foo1 =:= $foo, '... $_foo1 and $foo are the same instances');

is($foo.num(), 20, '... got the right num value');

my $_foo2 = $foo.baz(20);
isa_ok($_foo2, 'Foo');
ok($foo =:= $_foo2 =:= $_foo1, '... $_foo1, $_foo2 and $foo are the same instances', :todo<feature>);

is($foo.num(), 40, '... got the right num value');

# now lets try it with chained methods ...

my $_foo3;
lives_ok {
    $_foo3 = $foo.bar(10).baz(5);
}, '... method chainging works';

isa_ok($_foo3, 'Foo');
ok($_foo3 =:= $_foo2 =:= $_foo1 =:= $foo, '... $_foo3, $_foo1, $_foo2 and $foo are the same instances', :todo<feature>);

is($foo.num(), 15, '... got the right num value');
