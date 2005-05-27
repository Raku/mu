#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# L<A12/"Object Deconstruction">

my $in_destructor = 0;

class Foo
{
    submethod DESTROY { $in_destructor++ }
}

my $a = Foo.new();
is( $a.ref,        'Foo', 'basic instantiation of declared class' );
ok( ! $in_destructor,     'destructor should not fire while object is active' );

# no guaranteed timely destruction, so replace $a and try to force some GC here
for 1 .. 100
{
    $a = Foo.new();
}

ok( $in_destructor, '... only when object goes away everywhere' );
