#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;
use Test::Exception;

require 'lib/pneuma.pl'; 

# check some methods

is($::Object->id, 2, '... $::Object is the second object in the system');

is_deeply(
    $::Object->superclasses(), 
    [], 
    '... $::Object->superclasses() is []');

is_deeply(
    [ $::Object->MRO() ], 
    [ $::Object ], 
    '... $::Object->MRO() is $::Object');
    
my $d = $::Object->dispatcher(':canonical');    
is(ref($d), 'CODE', '... got a dispatcher function');  
is($d->(), $::Object, '... got the right value from the dispatcher');
