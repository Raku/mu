#!/usr/bin/pugs

use v6;
use Test;

plan 6;

# L<A12/"Object Deconstruction">

my $in_destructor = 0;
my @destructor_order;

class Foo
{
    submethod DESTROY { $in_destructor++ }
}

class Parent
{
    submethod DESTROY { push @in_destructor, 'Parent' }
}

class Child is Parent
{
    submethod DESTROY { push @in_destructor, 'Child' }
}

my $foo = Foo.new();
is( $foo.ref,      'Foo', 'basic instantiation of declared class' );
ok( ! $in_destructor,    'destructor should not fire while object is active' );

my $child = Child.new();
undef $child;

# no guaranteed timely destruction, so replace $a and try to force some GC here
for 1 .. 100
{
    $foo = Foo.new();
}

ok( $in_destructor, '... only when object goes away everywhere' );
is(  @destructor_order[0], 'Child',  'Child DESTROY should fire first', :todo<feature>  );
is(  @destructor_order[1], 'Parent', '... then parent', :todo<feature> );
is( +@destructor_order, 2, '... only as many as available DESTROY submethods');
