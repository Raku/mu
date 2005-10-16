#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::MetaModel;

use_ok('Perl6::MetaModel::Parser');

{ # bare class
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');
    
    my $source = "class Foo {}";

    $p->parse($source);
    
    my $Foo = $::{'*'}->FETCH('Foo');
    isa_ok($Foo, 'Class');
    isa_ok($Foo, 'Object');   
    isa_ok($Foo, 'Foo');    
    
    is($Foo->name, 'Foo', '... got the right name for class');    
}

{ # class with one attribute
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = 'class Bar { has $.baz; }';

    $p->parse($source);

    my $Bar = $::{'*'}->FETCH('Bar');
    isa_ok($Bar, 'Class');
    isa_ok($Bar, 'Object');  
    isa_ok($Bar, 'Bar');    

    is($Bar->name, 'Bar', '... got the right name for class');    
    ok($Bar->has_attribute('$.baz'), '... got the $.baz attribute for class');        
}

{ # class with multiple attributes
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Baz { 
        has $.baz; 
        has @.bar;         
        has %.foo;                 
    }
    |;

    $p->parse($source);

    my $Baz = $::{'*'}->FETCH('Baz');
    isa_ok($Baz, 'Class');
    isa_ok($Baz, 'Object');  
    isa_ok($Baz, 'Baz');    

    is($Baz->name, 'Baz', '... got the right name for class');    
    ok($Baz->has_attribute('$.baz'), '... got the $.baz attribute for class'); 
    ok($Baz->has_attribute('@.bar'), '... got the @.bar attribute for class'); 
    ok($Baz->has_attribute('%.foo'), '... got the %.foo attribute for class'); 
                   
}

{ # two classes defined
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class One {}
    class Two {}    
    |;

    $p->parse($source);

    my $One = $::{'*'}->FETCH('One');
    isa_ok($One, 'Object');      
    isa_ok($One, 'One');
    is($One->name, 'One', '... got the right name for class');    
    
    my $Two = $::{'*'}->FETCH('Two');
    isa_ok($Two, 'Object');      
    isa_ok($Two, 'Two');
    is($Two->name, 'Two', '... got the right name for class');    

}

{ # two classes defined w/ attributes
    my $p = Perl6::MetaModel::Parser->new();
    isa_ok($p, 'Perl6::MetaModel::Parser');

    my $source = q|
    class Three {
        has @.six;
    }
    class Four { has $.five; }    
    |;

    $p->parse($source);

    my $Three = $::{'*'}->FETCH('Three');
    isa_ok($Three, 'Object');  
    isa_ok($Three, 'Three');
    is($Three->name, 'Three', '... got the right name for class');  
    ok($Three->has_attribute('@.six'), '... got the @.six attribute for class');       
    is_deeply(
        [ $Three->get_attribute_list ],
        [ '@.six' ],
        '... got the right list of attributes');
        
    my $Four = $::{'*'}->FETCH('Four');
    isa_ok($Four, 'Object');      
    isa_ok($Four, 'Four');
    is($Four->name, 'Four', '... got the right name for class');  
    ok($Four->has_attribute('$.five'), '... got the $.five attribute for class');       
    is_deeply(
        [ $Four->get_attribute_list ],
        [ '$.five' ],
        '... got the right list of attributes');        

}


