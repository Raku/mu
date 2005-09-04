#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 28;
use Test::Exception; 

do 'lib/genesis.pl';

is_deeply(
    $::Class->superclasses, 
    [ $::Object ], 
    '... $::Class->superclasses() is $::Object');

is_deeply(
    [ $::Object->MRO() ], 
    [ $::Object ], 
    '... $::Object->MRO() is ($::Object)');      
    
is_deeply(
    [ $::Class->MRO() ], 
    [ $::Class, $::Object ], 
    '... $::Class->MRO() is ($::Class, $::Object)');    

ok($::Class->is_a('Object'), '... $::Class->is_a(Object)');
ok($::Class->isa('Object'), '... $::Class->isa(Object)');

# can call all of Object's class methods ...
foreach my $method_name (qw(new bless CREATE isa can)) {
    ok($::Class->can($method_name), '... Class->can(' . $method_name . ')');
}

# can call all of Object's instance methods as well ...
foreach my $method_name (qw(BUILD BUILDALL DESTROYALL isa can)) {
    ok($::Class->can($method_name), '... Class->can(' . $method_name . ')');
}

# now call some Object methods

ok($::Object->isa('Object'), '... Object->isa(Object)');

foreach my $method_name (qw(new bless CREATE isa can)) {
    ok($::Object->can($method_name), '... Object->can(' . $method_name . ')');
}

my $iObject = $::Object->new();
ok($iObject->isa('Object'), '... iObject->isa(Object)');

is($iObject->id, 3, '... $iObject is the third object in the system');

foreach my $method_name (qw(BUILD BUILDALL DESTROYALL isa can)) {
    ok($iObject->can($method_name), '... iObject->can(' . $method_name . ')');
}
