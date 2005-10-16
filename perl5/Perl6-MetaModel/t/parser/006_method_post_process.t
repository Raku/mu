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
        has $.bar;
        method bar {
            shift;
            $.bar = shift if @_;
            $.bar;
        }
    } 
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Foo');    

    is($Foo->name, 'Foo', '... got the right name for class');    
    ok($Foo->has_method('bar'), '... Foo has the &bar method');
    
    my $foo = $Foo->new();
    isa_ok($foo, 'Foo');
    
    $foo->bar(42);
    is($foo->bar, 42, '... our &bar method worked correctly');
}

{ # basic method
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Foo2 {
        has @.bar;
        method bar {
            shift;
            push @.bar => @_;
            @.bar;
        }
        method baz {
            my ($self, $index) = @_;
            @.bar[$index];
        }        
    } 
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo2');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Foo2');    

    is($Foo->name, 'Foo2', '... got the right name for class');    
    ok($Foo->has_method('bar'), '... Foo has the &bar method');
    ok($Foo->has_method('baz'), '... Foo has the &baz method');    

    my $foo = $Foo->new();
    isa_ok($foo, 'Foo2');

    $foo->bar(1, 2, 3, 4, 5);
    is_deeply(
        [ $foo->bar ], 
        [ 1, 2, 3, 4, 5 ], 
        '... our &bar method worked correctly');
        
    is($foo->baz(0), 1, '... got the right value with &baz');
    is($foo->baz(4), 5, '... got the right value with &baz');
    is($foo->baz(1), 2, '... got the right value with &baz');
    is($foo->baz(3), 4, '... got the right value with &baz');            
}

{ # basic method
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Foo3 {
        has %.bar;
        method bar {
            my ($self, $key, $value) = @_;
            %.bar{$key} = $value;
        }
        method baz {
            my ($self, $key) = @_;
            %.bar{$key};
        }  
        method foo {
            %.bar;
        }                
    } 
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo3');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Foo3');    

    is($Foo->name, 'Foo3', '... got the right name for class');    
    ok($Foo->has_method('bar'), '... Foo has the &bar method');
    ok($Foo->has_method('baz'), '... Foo has the &baz method');    

    my $foo = $Foo->new();
    isa_ok($foo, 'Foo3');

    $foo->bar(test => 42);
    is($foo->baz('test'), 42, '... our &bar and &baz methods worked correctly');

    $foo->bar(test2 => 43);
    is_deeply(
        { $foo->foo },
        { test => 42, test2 => 43 },
        '... got the right return value here');
    
}
