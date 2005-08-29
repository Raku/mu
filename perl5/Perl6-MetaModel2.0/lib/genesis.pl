#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/pneuma.pl" };

# ... this makes ::Class a subclass of ::Object
# the result of this is (Theos)

# < Class is a subclass of Object >
::opaque_instance_attrs($::Class)->{'@:superclasses'} = [ $::Object ];

::opaque_instance_attrs($::Class)->{'@:MRO'} = [ $::Class, $::Object ];
::opaque_instance_attrs($::Object)->{'@:MRO'} = [ $::Object ];

1;
