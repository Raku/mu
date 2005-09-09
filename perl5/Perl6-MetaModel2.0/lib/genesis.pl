#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/pneuma.pl" };

## Now create some of the other things we need ...
## see http://article.gmane.org/gmane.comp.lang.perl.perl6.language/4599 for 
## more info on the Class isa Module isa Package isa Object thing.

## ----------------------------------------------------------------------------
## Package

$::Package = $::Class->new();

$::Package->superclasses([ $::Object ]);

$::Package->add_attribute('$:name'      => ::make_attribute('$:name'));
$::Package->add_attribute('%:namespace' => ::make_attribute('%:namespace'));

$::Package->add_method('name' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attrs($self)->{'$:name'} = shift if @_;        
    ::opaque_instance_attrs($self)->{'$:name'};
}, $::Package));

$::Package->add_method('FETCH' => ::make_method(sub {
    my ($self, $label) = @_;
    (defined $label && $label)
        || confess "Cannot FETCH at (" . ($label || 'undef') . ")";
    ::opaque_instance_attrs($self)->{'%:namespace'}->{$label};
}, $::Package));

$::Package->add_method('STORE' => ::make_method(sub {
    my ($self, $label, $value) = @_;
    (defined $label && $label)
        || confess "Cannot STORE at (" . ($label || 'undef') . ")";    
    ::opaque_instance_attrs($self)->{'%:namespace'}->{$label} = $value;
}, $::Package));

## ----------------------------------------------------------------------------
## Module

$::Module = $::Class->new();

$::Module->superclasses([ $::Package ]);

$::Module->add_attribute('$:version'   => ::make_attribute('$:version'));
$::Module->add_attribute('$:authority' => ::make_attribute('$:authority'));

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

## ----------------------------------------------------------------------------
## now for some bootstrapping ....

# ... this makes ::Class a subclass of ::Object
# the result of this is (Theos)

# < Class is a subclass of Module is a subclass of Package is a subclass of Object >
::opaque_instance_attrs($::Class)->{'@:superclasses'} = [ $::Module ];
::opaque_instance_attrs($::Module)->{'@:subclasses'}  = [ $::Class  ];

# NOTE:
# this is to avoid recursion
::opaque_instance_attrs($::Class)->{'@:MRO'}  = [ $::Class, $::Module, $::Package, $::Object ];
::opaque_instance_attrs($::Object)->{'@:MRO'} = [ $::Object ];

# now make sure we set everyone's name properly
$::Package->name('Package');
$::Module->name('Module');
$::Class->name('Class');
$::Object->name('Object');

## ----------------------------------------------------------------------------
## make Class conform to the Package interface

$::Class->add_method('FETCH' => ::make_method(sub {
    my ($self, $label) = @_;
    (defined $label && $label)
        || confess "Cannot FETCH at (" . ($label || 'undef') . ")";
    if ($label =~ /^\&(.*)$/) {
        # check for instance method
        return $self->has_method($1, for => 'instance') ? 
                    $self->get_method($1, for => 'instance') 
                    :
                    # check for class method
                    $self->has_method($1, for => 'class') ?
                        $self->get_method($1, for => 'class')
                        :
                        # if all else fails, maybe it is 
                        # a sub, so we just call next METHOD
                        ::next_METHOD();
    }   
    # XXX -
    # this reg-exp is probably not correct ...
    elsif ($label =~ /^.(\.|\:).*$/) {
        # check for instance attribute
        return $self->has_attribute($label, for => 'instance') ?
                    $self->get_attribute($label, for => 'instance')
                    :
                    # check for class attribute
                    $self->has_attribute($label, for => 'class') ?
                        $self->get_attribute($label, for => 'class')
                        :
                        # otherwise we got nothin
                        undef;
    } 
    else {
        ::next_METHOD();
    }    
}, $::Class));

$::Class->add_method('STORE' => ::make_method(sub {
    my ($self, $label, $value) = @_;
    (defined $label && $label)
        || confess "Cannot STORE at (" . ($label || 'undef') . ")";    
    # only store method objects,.. regular subs go in the namespace
    if ($label =~ /^\&(.*)$/ && (blessed($value) && $value->isa('Perl6::Method'))) {
        return $self->add_method($1, $value);
    }  
    # XXX -
    # this reg-exp is probably not correct ...     
    elsif ($label =~ /^.(\.|\:).*$/) {
        # NOTE:
        # since the twigil ($. and $:) is only allowed
        # for instance/class attributes, then we do not
        # check the type of $value. This means that this
        # could potentially fail
        return $self->add_attribute($label, $value);
    } 
    else {
        ::next_METHOD();
    }
}, $::Class));


__END__

## ----------------------------------------------------------------------------
## Role

# Notes on Roles:
# - role methods need to be special, this is because we need to be able to bind
#   the $?ROLE value at Role creation time, but the $?CLASS value at composition 
#   time. Currently I think the best approach is to make role methods a special 
#   case of some kind, and then "upgrade" them when we compose them into the class. 

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
