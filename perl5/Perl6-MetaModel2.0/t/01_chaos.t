#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

do 'lib/chaos.pl';

## test the opaque instance containers

my $i;
$i = ::create_opaque_instance(\$i, (foo => 1));
ok(ref($i), 'Dispatchable');

is(::opaque_instance_id($i), 1, '... got the right ID');
is(::opaque_instance_class($i), $i, '... got the right class');
is_deeply(
    ::opaque_instance_attrs($i), 
    { foo => 1 }, 
    '... got the right attrs');
    
my $j = ::create_opaque_instance(\$i, (bar => 1));
ok(ref($j), 'Dispatchable');

is(::opaque_instance_id($j), 2, '... got the right ID');    
is(::opaque_instance_class($j), $i, '... got the right class');
is_deeply(
    ::opaque_instance_attrs($j), 
    { bar => 1 }, 
    '... got the right attrs');
    
## test the method constructors

{
    my $m = ::make_method(sub { return 'Foo' }, $i);
    ok(ref($m), 'Perl6::Method');

    is($m->(), 'Foo', '... got the right return value');

    my $m2 = ::make_method(sub { return ::SELF() }, $i);
    ok(ref($m2), 'Perl6::Method');

    is($m2->('Bar'), 'Bar', '... got the right return value');
    
    my $m3 = ::make_method(sub { return ::CLASS() }, $i);
    ok(ref($m3), 'Perl6::Method');

    is($m3->(), $i, '... got the right return value');    
}

{
    my $m = ::make_classmethod(sub { return 'Foo::Bar' }, $i);
    ok(ref($m), 'Perl6::ClassMethod');

    is($m->(), 'Foo::Bar', '... got the right return value');
    
    my $m2 = ::make_classmethod(sub { return ::SELF() }, $i);
    ok(ref($m2), 'Perl6::ClassMethod');

    is($m2->('Bar::Baz'), 'Bar::Baz', '... got the right return value');    
    
    my $m3 = ::make_classmethod(sub { return ::CLASS() }, $i);
    ok(ref($m3), 'Perl6::Method');

    is($m3->(), $i, '... got the right return value');       
}

{
    my $m = ::make_submethod(sub { return 'Baz' }, $i);
    ok(ref($m), 'Perl6::Submethod');
    is($m->($i), 'Baz', '... got the right return value');
    
    my $m2 = ::make_submethod(sub { return 'Baz' }, $i);
    ok(ref($m2), 'Perl6::Submethod');
    is($m2->($j), 'Baz', '... got the right return value');    

    my $m3 = ::make_submethod(sub { return ::SELF() }, $i);
    ok(ref($m3), 'Perl6::Submethod');
    is($m3->($j), $j, '... got the right return value');    
    
    my $m4 = ::make_submethod(sub { return ::CLASS() }, $i);
    ok(ref($m4), 'Perl6::Submethod');
    is($m4->($j), $i, '... got the right return value');        
    
    {
        no strict 'refs';
        no warnings 'redefine';
        
        *{'::next_METHOD'} = sub { "fake next_METHOD" };
    
        my $m = ::make_submethod(sub { return 'Baz' }, $j);
        ok(ref($m), 'Perl6::Submethod');
        is($m->($i), 'fake next_METHOD', '... got the right return value');    
    }
    
    {
        my $m = ::make_submethod(sub { return 'Baz' }, $j);
        ok(ref($m), 'Perl6::Submethod');
        ok($Perl6::Submethod::FORCE, '... $Perl6::Submethod::FORCE is defined');
        is($m->($Perl6::Submethod::FORCE, $i), 'Baz', '... got the right return value (with force call)');          
    }
}

