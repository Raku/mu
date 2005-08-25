#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 18;
use Test::Exception;

do 'lib/pneuma.pl'; 

# check some META:: methods

is($::Object->META::name, 'Object', '... $::Object->META::name() is Object');
is($::Object->META::version, '0.0.0', '... $::Object->META::version() is 0.0.0');

is_deeply(
    $::Object->META::superclasses(), 
    [], 
    '... $::Object->META::superclasses() is []');

is_deeply(
    [ $::Object->META::MRO() ], 
    [ $::Object ], 
    '... $::Object->META::MRO() is $::Object');
    
my $d = $::Object->META::dispatcher(':canonical');    
is(ref($d), 'CODE', '... got a dispatcher function');  
is($d->(), $::Object, '... got the right value from the dispatcher');

# now call some class methods

ok($::Object->isa('Object'), '... Object->isa(Object)');

foreach my $method_name (qw(new bless CREATE isa can)) {
    ok($::Object->can($method_name), '... Object->can(' . $method_name . ')');
}

my $iObject = $::Object->new();
ok($iObject->isa('Object'), '... iObject->isa(Object)');

foreach my $method_name (qw(BUILD BUILDALL DESTROYALL isa can)) {
    ok($iObject->can($method_name), '... iObject->can(' . $method_name . ')');
}
