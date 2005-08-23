#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 1;
use Test::Exception;

do 'lib/genesis.pl';

# avoid warnings for now
my @classes = ($::Class, $::Object);

is_deeply(
    $::Class->superclasses, 
    [ $::Object ], 
    '... $::Class->superclasses() is $::Object');