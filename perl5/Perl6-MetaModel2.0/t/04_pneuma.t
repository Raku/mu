#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 6; #18;
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

exit;

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
