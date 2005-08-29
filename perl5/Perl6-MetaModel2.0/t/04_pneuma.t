#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 18;
use Test::Exception;

do 'lib/pneuma.pl'; 

# check some methods

is($::Object->name, 'Object', '... $::Object->name() is Object');
is($::Object->version, '0.0.0', '... $::Object->version() is 0.0.0');

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

# now call some class methods

ok($::Object->class::isa('Object'), '... Object->class::isa(Object)');

foreach my $method_name (qw(new bless CREATE isa can)) {
    ok($::Object->class::can($method_name), '... Object->class::can(' . $method_name . ')');
}

my $iObject = $::Object->class::new();
ok($iObject->isa('Object'), '... iObject->isa(Object)');

foreach my $method_name (qw(BUILD BUILDALL DESTROYALL isa can)) {
    ok($iObject->can($method_name), '... iObject->can(' . $method_name . ')');
}
