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
        'new' => sub ($%) {
            my ($class, %attrs) = @_;
            return ::create_opaque_instance(\$class, %attrs);
        },      
        'id' => sub ($) {
            ::opaque_instance_id(shift)
        },
        'class' => sub ($) {
            ::opaque_instance_class(shift)
        }
    },
);
