#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::MetaModel;

use_ok('Perl6::MetaModel::Parser');

{ # basic method
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Foo {
        method bar { 'Foo::bar' }
    } 
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Foo');    

    is($Foo->name, 'Foo', '... got the right name for class');    
    is_deeply(
        $Foo->superclasses, 
        [ $::Object ], 
        '... got the right superclass');   
        
    ok($Foo->has_method('bar'), '... Foo has the &bar method');
    is($Foo->get_method('bar')->(), 'Foo::bar', '... Foo has the &bar method');    
}

{ # basic method
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Bar {
        method baz {
            my ($self) = @_;
            my $acc = 0;
            foreach my $num (1 .. 10) {
                $acc += $num;
            }
            return $acc;
        }
    } 
    |;

    $p->parse($source);

    my $Bar = $::{'*'}->FETCH('Bar');
    isa_ok($Bar, 'Class');
    isa_ok($Bar, 'Bar');    

    is($Bar->name, 'Bar', '... got the right name for class');    
    is_deeply(
        $Bar->superclasses, 
        [ $::Object ], 
        '... got the right superclass');   

    ok($Bar->has_method('baz'), '... Bar has the &baz method');
    is($Bar->get_method('baz')->(), 55, '... Bar has the &baz method');    
}

{ # basic method
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class FooBar {
        method bar { 'FooBar::bar' }        
        method baz {
            my ($self) = @_;
            my $acc = 0;
            foreach my $num (1 .. 10) {
                $acc += $num;
            }
            return $acc;
        }
    } 
    |;

    $p->parse($source);

    my $FooBar = $::{'*'}->FETCH('FooBar');
    isa_ok($FooBar, 'Class');
    isa_ok($FooBar, 'FooBar');    

    is($FooBar->name, 'FooBar', '... got the right name for class');    
    is_deeply(
        $FooBar->superclasses, 
        [ $::Object ], 
        '... got the right superclass');   
        
    ok($FooBar->has_method('bar'), '... FooBar has the &bar method');
    is($FooBar->get_method('bar')->(), 'FooBar::bar', '... FooBar has the &bar method');  

    ok($FooBar->has_method('baz'), '... FooBar has the &baz method');
    is($FooBar->get_method('baz')->(), 55, '... FooBar has the &baz method');    
}
