#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

# Perl6
use_ok('Perl6::MetaModel');
use_ok('Perl6::MetaClass');
use_ok('Perl6::Object');   
use_ok('Perl6::Class');
use_ok('Perl6::Role');
 
use_ok('Perl6::Attribute');
use_ok('Perl6::Method');

    # Perl6/Util

    # Perl6/Role
    use_ok('Perl6::Role::Attribute');

    # Perl6/Instance
    use_ok('Perl6::Instance::Attribute');

    # Perl6/Class
    use_ok('Perl6::Class::Attribute');

# avoid "used only once error"
my %this_is_annoying = %Perl6::Class::ALL_CLASSES;

is_deeply(
    [ 'Perl6::MetaClass', 'Perl6::Object' ],
    [ sort keys %Perl6::Class::ALL_CLASSES ], 
    '... got the expected list of class loaded');
