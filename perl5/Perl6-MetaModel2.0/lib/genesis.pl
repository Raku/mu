#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/psyche.pl" };

## ----------------------------------------------------------------------------
## now for some bootstrapping ....

## See http://article.gmane.org/gmane.comp.lang.perl.perl6.language/4599 for 
## more info on the Class isa Module isa Package isa Object thing.

# ... this makes ::Class a subclass of ::Object (through ::Package and ::Module)
# the result of this is (Theos)

# < Class is a subclass of Module is a subclass of Package is a subclass of Object >
::opaque_instance_attr($::Class => '@:superclasses') = [ $::Module ];
::opaque_instance_attr($::Module => '@:subclasses')  = [ $::Class, $::Role  ];

# NOTE:
# this is to avoid recursion
::opaque_instance_attr($::Class => '@:MRO')  = [ $::Class, $::Module, $::Package, $::Object ];
::opaque_instance_attr($::Object => '@:MRO') = [ $::Object ];

# now make sure we set everyone's name properly
$::Package->name('Package');
$::Module->name('Module');
$::Class->name('Class');
$::Object->name('Object');
$::Role->name('Role');

# now create the * (root) package ... 

$::{'*'} = $::Package->new('$:name' => '*');

# and add our meta-objects to it ...

$::{'*'}->STORE('::Package' => $::Package);
$::{'*'}->STORE('::Module'  => $::Module);
$::{'*'}->STORE('::Class'   => $::Class);
$::{'*'}->STORE('::Object'  => $::Object);
$::{'*'}->STORE('::Role'    => $::Role);

# and create our Main package 

$::{'*'}->STORE('::Main' => $::Package->new('$:name' => 'Main'));

1;

__END__

=pod

=head1 NAME

genesis

=head1 DESCRIPTION

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut