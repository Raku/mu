#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 59;
use Test::Exception; 

do 'lib/genesis.pl'; 

is_deeply(
    $::Package->superclasses, 
    [ $::Object ], 
    '... $::Package->superclasses() is [ $::Object ]'); 
    
is_deeply(
    $::Package->subclasses, 
    [ $::Module ], 
    '... $::Package->subclasses() is [ $::Module ]');        
    
is_deeply(
    $::Module->superclasses, 
    [ $::Package ], 
    '... $::Module->superclasses() is [ $::Package ]');   
    
is_deeply(
    $::Module->subclasses, 
    [ $::Class ], 
    '... $::Module->subclasses() is [ $::Class ]');      
    
is_deeply(
    $::Class->superclasses, 
    [ $::Module ], 
    '... $::Class->superclasses() is [ $::Module ]');        
    
is_deeply(
    $::Class->subclasses, 
    [], 
    '... $::Class->subclasses() is []');            

is_deeply(
    $::Object->superclasses, 
    [], 
    '... $::Object->superclasses() is []');   
    
is_deeply(
    $::Object->subclasses, 
    [ $::Package ], 
    '... $::Object->subclasses() is [ $::Package ]');                 

is_deeply(
    [ $::Package->MRO() ], 
    [ $::Package, $::Object ], 
    '... $::Module->MRO() is ($::Module, $::Package, $::Object)');   

is_deeply(
    [ $::Module->MRO() ], 
    [ $::Module, $::Package, $::Object ], 
    '... $::Module->MRO() is ($::Module, $::Package, $::Object)');   
    
is_deeply(
    [ $::Class->MRO() ], 
    [ $::Class, $::Module, $::Package, $::Object ], 
    '... $::Class->MRO() is ($::Class, $::Module, $::Package, $::Object)');      

is_deeply(
    [ $::Object->MRO() ], 
    [ $::Object ], 
    '... $::Object->MRO() is ($::Object)');       
    
is($::Package->name, 'Package', '... $::Package->name eq Package'); 
is($::Module->name, 'Module', '... $::Module->name eq Module'); 
is($::Class->name, 'Class', '... $::Class->name eq Class'); 
is($::Object->name, 'Object', '... $::Object->name eq Object'); 

ok($::Class->is_a($::Class), '... $::Class->is_a($::Class)');
ok($::Class->isa('Class'), '... $::Class->isa(Class)');

ok($::Class->is_a($::Module), '... $::Class->is_a($::Module)');
ok($::Class->isa('Module'), '... $::Class->isa(Module)');

ok($::Class->is_a($::Object), '... $::Class->is_a($::Object)');
ok($::Class->isa('Object'), '... $::Class->isa(Object)');

ok($::Module->is_a($::Module), '... $::Module->is_a($::Module)');
ok($::Module->isa('Module'), '... $::Module->isa(Module)');

ok($::Module->is_a($::Object), '... $::Module->is_a($::Object)');
ok($::Module->isa('Object'), '... $::Module->isa(Object)');

ok($::Object->is_a($::Object), '... $::Object->is_a($::Object)');
ok($::Object->isa('Object'), '... $::Object->isa(Object)');

# Class can call all of Modules's methods ...
foreach my $method_name (qw(name version authority identifier)) {
    ok($::Class->can($method_name), '... Class->can(' . $method_name . ')');
}

# Module can call all of Modules's methods and all of Object's ...
foreach my $method_name (qw(name version authority identifier
                            BUILD BUILDALL DESTROYALL isa can)) {
    ok($::Module->can($method_name), '... Module->can(' . $method_name . ')');
}

# Object can call all of Modules's methods ...
foreach my $method_name (qw(name version authority identifier)) {
    ok($::Object->can($method_name), '... Object->can(' . $method_name . ')');
}

# now create an Object

my $iObject = $::Object->new();
ok($iObject->isa('Object'), '... iObject->isa(Object)');

cmp_ok($iObject->id, '>', 3, '... $iObject is at least the third object in the system');

foreach my $method_name (qw(BUILD BUILDALL DESTROYALL isa can)) {
    ok($iObject->can($method_name), '... iObject->can(' . $method_name . ')');
}

# now create a Module

# now create an Object

my $MyModule = $::Module->new();
ok($MyModule->isa('Module'), '... MyModule->isa(Module)');
ok($MyModule->isa('Object'), '... MyModule->isa(Object)');

cmp_ok($MyModule->id, '>', 3, '... $MyModule is the at least the third object in the system');

# Module can call all of Modules's methods and all of Object's ...
foreach my $method_name (qw(name version authority identifier)) {
    ok($MyModule->can($method_name), '... MyModule->can(' . $method_name . ')');
}

