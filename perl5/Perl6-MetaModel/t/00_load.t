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
use_ok('Perl6::SubMethod');

    # Perl6/Util

    # Perl6/Role
    use_ok('Perl6::Role::Attribute');
    use_ok('Perl6::Role::Method');

    # Perl6/Instance
    use_ok('Perl6::Instance::Attribute');
    use_ok('Perl6::Instance::Method');

    # Perl6/Class
    use_ok('Perl6::Class::Attribute');
    use_ok('Perl6::Class::Method');
