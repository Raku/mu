#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::MetaModel;

use_ok('Perl6::MetaModel::Parser');

{ # basic superclasses
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Foo {} 
    class Bar is Foo {}
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Object');      
    isa_ok($Foo, 'Foo');    

    is($Foo->name, 'Foo', '... got the right name for class'); 
    is_deeply(
        $Foo->superclasses, 
        [ $::Object ], 
        '... got the right superclass');             
    
    my $Bar = $::{'*'}->FETCH('Bar');
    isa_ok($Bar, 'Class');
    isa_ok($Bar, 'Object');      
    isa_ok($Bar, 'Bar');    
    isa_ok($Bar, 'Foo');        

    is($Bar->name, 'Bar', '... got the right name for class');        
    is_deeply(
        $Bar->superclasses, 
        [ $Foo ], 
        '... got the right superclass');            
}

{ # basic superclasses
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Foo2 {} 
    class Bar2 {}
    class FooBar is Foo2 is Bar2 {}
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo2');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Foo2');    
    is($Foo->name, 'Foo2', '... got the right name for class');    
    is_deeply(
        $Foo->superclasses, 
        [ $::Object ], 
        '... got the right superclass');           

    my $Bar = $::{'*'}->FETCH('Bar2');
    isa_ok($Bar, 'Class');
    isa_ok($Bar, 'Bar2');           
    is($Bar->name, 'Bar2', '... got the right name for class');   
    is_deeply(
        $Bar->superclasses, 
        [ $::Object ], 
        '... got the right superclass');           
    
    my $FooBar = $::{'*'}->FETCH('FooBar');
    isa_ok($FooBar, 'Class');
    isa_ok($FooBar, 'FooBar');           
    isa_ok($FooBar, 'Foo2');               
    isa_ok($FooBar, 'Bar2');               
    is($FooBar->name, 'FooBar', '... got the right name for class');            
    
    is_deeply(
        $FooBar->superclasses, 
        [ $Foo, $Bar ], 
        '... got the right superclass');            
}

{ # basic superclasses
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Foo3 {} 
    class Bar3 {}
    class FooBar2 { 
        is Foo3; 
        is Bar3;
    }
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo3');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Foo3');    
    is($Foo->name, 'Foo3', '... got the right name for class'); 
    is_deeply(
        $Foo->superclasses, 
        [ $::Object ], 
        '... got the right superclass');                  

    my $Bar = $::{'*'}->FETCH('Bar3');
    isa_ok($Bar, 'Class');
    isa_ok($Bar, 'Bar3');           
    is($Bar->name, 'Bar3', '... got the right name for class');        
    is_deeply(
        $Bar->superclasses, 
        [ $::Object ], 
        '... got the right superclass');               

    my $FooBar = $::{'*'}->FETCH('FooBar2');
    isa_ok($FooBar, 'Class');
    isa_ok($FooBar, 'FooBar2');           
    isa_ok($FooBar, 'Foo3');               
    isa_ok($FooBar, 'Bar3');               
    is($FooBar->name, 'FooBar2', '... got the right name for class');            

    is_deeply(
        $FooBar->superclasses, 
        [ $Foo, $Bar ], 
        '... got the right superclass');            
}

{ # basic superclasses
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Foo4 {} 
    class Bar4 {}
    class FooBar3 is Foo4 { 
        is Bar4;
    }
    |;

    $p->parse($source);

    my $Foo = $::{'*'}->FETCH('Foo4');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Foo4');    
    is($Foo->name, 'Foo4', '... got the right name for class');  
    is_deeply(
        $Foo->superclasses, 
        [ $::Object ], 
        '... got the right superclass');                 

    my $Bar = $::{'*'}->FETCH('Bar4');
    isa_ok($Bar, 'Class');
    isa_ok($Bar, 'Bar4');           
    is($Bar->name, 'Bar4', '... got the right name for class');        
    is_deeply(
        $Bar->superclasses, 
        [ $::Object ], 
        '... got the right superclass');               

    my $FooBar = $::{'*'}->FETCH('FooBar3');
    isa_ok($FooBar, 'Class');
    isa_ok($FooBar, 'FooBar3');           
    isa_ok($FooBar, 'Foo4');               
    isa_ok($FooBar, 'Bar4');               
    is($FooBar->name, 'FooBar3', '... got the right name for class');            

    is_deeply(
        $FooBar->superclasses, 
        [ $Foo, $Bar ], 
        '... got the right superclass');            
}
