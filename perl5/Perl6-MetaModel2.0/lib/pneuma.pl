#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/metamorph.pl" };

# ... this makes ::Object

$::Object = undef;

# The 'Object' class
$::Object = ::create_class(
    '$:name'    => 'Object',
    '%:methods' => {
        'new'        => ::classmethod {},   
        'bless'      => ::classmethod {},       
        'CREATE'     => ::classmethod {},
        'isa'        => ::classmethod {},   
        'can'        => ::classmethod {},        
        
        'BUILD'      => ::submethod {},
        'BUILDALL'   => ::method {},
        'DESTROYALL' => ::method {},      
        'isa'        => ::method {},   
        'can'        => ::method {},
        'id'         => ::method { ::opaque_instance_id(shift) },
    },
);
