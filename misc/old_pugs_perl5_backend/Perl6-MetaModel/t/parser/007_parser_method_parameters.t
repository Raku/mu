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
        method bar (Foo $self:) { 
            $self.class.name ~ '::bar' 
        }
        method baz (Class $c:) { ($c.name ~ '::baz') }
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

    my $foo = $Foo->new();
    isa_ok($foo, 'Foo');
    
    is($foo->bar, 'Foo::bar', '... got the right value from the method');

    is($Foo->baz, 'Foo::baz', '... got right value form the class method');
}

{ # basic method
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Bar {
        method baz (Bar $self: @args) { 
            $self.class.name ~ '::baz(' ~ (join ", " => @args) ~ ')' 
        }
    } 
    |;

    $p->parse($source);

    my $Bar = $::{'*'}->FETCH('Bar');
    isa_ok($Bar, 'Class');
    isa_ok($Bar, 'Bar');    

    ok($Bar->has_method('baz'), '... Bar has the &baz method');

    my $bar = $Bar->new();
    isa_ok($bar, 'Bar');

    is($bar->baz(1, 2, 3), 'Bar::baz(1, 2, 3)', '... got the right value from the method');
    is($bar->baz('hello', 'world'), 'Bar::baz(hello, world)', '... got the right value from the method');    

}

