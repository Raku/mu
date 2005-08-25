#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 1;
use Test::Exception; 

do 'lib/genesis.pl';

# avoid warnings for now
my @classes = ($::Class, $::Object);

use Data::Dumper;

#warn Dumper $::Class;

is_deeply(
    $::Class->META::superclasses, 
    [ $::Object ], 
    '... $::Class->superclasses() is $::Object');
    
#is_deeply(
#    $::Object->META::MRO(), 
#    [ $::Object ], 
#    '... $::Object->MRO() is ($::Object)');      
    
#is_deeply(
#    $::Class->META::MRO(), 
#    [ $::Class, $::Object ], 
#    '... $::Class->MRO() is ($::Class, $::Object)');    

#ok($::Class->is_a('Object'), '... $::Class->isa(Object)');