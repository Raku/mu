#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/pneuma.pl" };

## Now create some of the other things we need ...

## ----------------------------------------------------------------------------
## Module (added with r6792)

$::Module = $::Class->new();

$::Module->superclasses([ $::Object ]);

$::Module->add_attribute('$:name'      => ::make_attribute('$:name'));
$::Module->add_attribute('$:version'   => ::make_attribute('$:version'));
$::Module->add_attribute('$:authority' => ::make_attribute('$:authority'));

$::Module->add_method('name' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attrs($self)->{'$:name'} = shift if @_;        
    ::opaque_instance_attrs($self)->{'$:name'};
}, $::Module));

$::Module->add_method('version' => ::make_method(sub {
    my ($self, $version) = @_;
    if (defined $version) {
        ($version =~ /^\d+\.\d+\.\d+$/)
            || confess "The version ($version) is not in the correct format '0.0.0'";
        ::opaque_instance_attrs($self)->{'$:version'} = $version;
    }
    ::opaque_instance_attrs($self)->{'$:version'};    
}, $::Module));

$::Module->add_method('authority' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attrs($self)->{'$:authority'} = shift if @_;        
    ::opaque_instance_attrs($self)->{'$:authority'};
}, $::Module));

$::Module->add_method('identifier' => ::make_method(sub {
    return join '-' => ($::SELF->name, $::SELF->version, ($::SELF->authority || ()));
}, $::Module));

# ... this makes ::Class a subclass of ::Object
# the result of this is (Theos)

# < Class is a subclass of Module is a subclass of Object >
::opaque_instance_attrs($::Class)->{'@:superclasses'} = [ $::Module ];

# NOTE:
# this is to avoid recursion
::opaque_instance_attrs($::Class)->{'@:MRO'} = [ $::Class, $::Module, $::Object ];
::opaque_instance_attrs($::Object)->{'@:MRO'} = [ $::Object ];

# now make sure we set everyone's name properly
$::Class->name('Class');
$::Object->name('Object');
$::Module->name('Module');

## ----------------------------------------------------------------------------
## Role

$::Role = $::Class->new('$:name' => 'Role');

$::Role->superclasses([ $::Module ]);

$::Role->add_attribute('@:subroles'   => ::make_attribute('@:subroles'));
$::Role->add_attribute('%:methods'    => ::make_attribute('%:methods'));
$::Role->add_attribute('%:attributes' => ::make_attribute('%:attributes'));

$::Role->add_method('add_method' => ::make_method(sub {
    my ($self, $label, $method) = @_;
    # NOTE: 
    # the $method here can be undefined, this is to allow
    # for methods which do not have an implementation. These
    # methods are then required to be implemented by the 
    # composing class.
    # (see A12/Class Composition with Roles/Declaration of Roles/Interfaces)
    ::opaque_instance_attrs($self)->{'%:methods'}->{$label} = $method;
}, $::Role));

$::Role->add_method('get_method_list' => ::make_method(sub {
    keys %{::opaque_instance_attrs($::SELF)->{'%:methods'}};
}, $::Role));

$::Role->add_method('add_attribute' => ::make_method(sub {
    my ($self, $label, $attribute) = @_;
    ::opaque_instance_attrs($self)->{'%:attributes'}->{$label} = $attribute;
}, $::Role));

$::Role->add_method('get_attribute_list' => ::make_method(sub {
    keys %{::opaque_instance_attrs($::SELF)->{'%:attributes'}};
}, $::Role));

$::Role->add_method('subroles' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attrs($self)->{'@:subroles'} = shift if @_;
    ::opaque_instance_attrs($self)->{'@:subroles'};
}, $::Role));

$::Role->add_method('does' => ::make_method(sub {
    my $self = shift;
    if (my $role_name = shift) {
        foreach (@{::opaque_instance_attrs($self)->{'@:subroles'}}) {
            return 1 if $_->name eq $role_name;            
        }
        return 0;
    }
    return map { $_->name } @{::opaque_instance_attrs($self)->{'@:subroles'}};
}, $::Role));


1;
