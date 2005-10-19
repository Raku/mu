#!/usr/bin/perl

use strict;
use warnings;
use Perl6::MetaModel::Pneuma;

# This is psyche, this creates ::Role

# From Answers.com:
# According to the Gnostics, the Demiurge was able to endow man 
# only with psyche (sensuous soul) — the pneuma (rational soul) 
# having been added by God.

# Naming Rationale:
# pneuma is the rational soul, really concreate and practical 
# things like Object, Package and Module. It was also created
# by God (which means it is part of the core model)
#
# psyche is the sensuous soul, meaning "relating to, or derived 
# from the senses". Think of Roles like you might think of Duck
# typing, but extend the metaphor to encompass all the senses. 
#
#   If it looks like a Duck, smells like a Duck, tastes like a Duck
#   feels like a Duck and quacks like a Duck, it must them implement
#   the Duck Role.
#
# The other point is that Demiurge endows man with the psyche, this
# sort of matches with how I plan to bootstrap Roles through Class.
# Which is not yet implemented but can be seen in MiniMetaModel_w_Roles.pl

$::Role = undef;

## ----------------------------------------------------------------------------
## Roles

# Questions on Role methods sent to p6l 
# - see recent thread: 'Concerning Roles and $?ROLE'

$::Role = $::Class->new();

$::Role->superclasses([ $::Module ]);

$::Role->add_attribute('@:roles'      => ::make_attribute('@:roles'));
$::Role->add_attribute('%:methods'    => ::make_attribute('%:methods'));
$::Role->add_attribute('%:attributes' => ::make_attribute('%:attributes'));

## methods

$::Role->add_method('add_method' => ::make_method(sub {
    my ($self, $label, $method) = @_;
    # NOTE: 
    # the $method here can be undefined, this is to allow
    # for methods which do not have an implementation. These
    # methods are then required to be implemented by the 
    # composing class.
    # (see A12/Class Composition with Roles/Declaration of Roles/Interfaces)
    ::opaque_instance_attr($self => '%:methods')->{$label} = (
        defined $method ? 
            ::wrap_role_method($method, $self)
            :
            ::make_stub_method());
}));

$::Role->add_method('get_method' => ::make_method(sub {
    my ($self, $label) = @_;
    ::opaque_instance_attr($self => '%:methods')->{$label};
}));

$::Role->add_method('has_method' => ::make_method(sub {
    my ($self, $label) = @_;
    exists ::opaque_instance_attr($self => '%:methods')->{$label} ? 1 : 0;
}));

$::Role->add_method('is_method_stub' => ::make_method(sub {
    my ($self, $label) = @_;
    blessed(::opaque_instance_attr($self => '%:methods')->{$label}) eq 'Perl6::StubMethod';
}));

$::Role->add_method('get_method_list' => ::make_method(sub {
    keys %{::opaque_instance_attr($::SELF => '%:methods')};
}));

## attributes

$::Role->add_method('add_attribute' => ::make_method(sub {
    my ($self, $label, $attribute) = @_;
    ::opaque_instance_attr($self => '%:attributes')->{$label} = (
        defined $attribute ?
            $attribute
            :
            ::make_stub_attribute($label));
}));

$::Role->add_method('get_attribute' => ::make_method(sub {
    my ($self, $label) = @_;    
    ::opaque_instance_attr($self => '%:attributes')->{$label};
}));

$::Role->add_method('has_attribute' => ::make_method(sub {
    my ($self, $label) = @_;    
    exists ::opaque_instance_attr($self => '%:attributes')->{$label} ? 1 : 0;
}));

$::Role->add_method('is_attribute_stub' => ::make_method(sub {
    my ($self, $label) = @_;    
    blessed(::opaque_instance_attr($self => '%:attributes')->{$label}) eq 'Perl6::StubAttribute';
}));

$::Role->add_method('get_attribute_list' => ::make_method(sub {
    keys %{::opaque_instance_attr($::SELF => '%:attributes')};
}));

## subroles

$::Role->add_method('roles' => ::make_method(sub {
    my $self = shift;                        
    ::opaque_instance_attr($self => '@:roles') = shift if @_;
    ::opaque_instance_attr($self => '@:roles');
}));

## ----------------------------------------------------------------------------
## BEGIN BOOTSTRAPPING AREA
## ----------------------------------------------------------------------------
## NOTE:
## code in this section has special cases within it to deal with 
## meta-circularity issues and other such ugliness 

## introspective methods

$::Role->add_method('does' => ::make_method(sub {
    my ($self, $role_name) = @_;
    return undef unless defined $role_name;
    # obviously if we are a role, we do Role, 
    # and the same goes for Class too, this
    # is a special case though ...
    return 1 if $role_name eq 'Role' && ($self->isa('Role') || $self->isa('Class'));
    # regular checking here ...
    return 1 if $self->name eq $role_name;
    foreach my $sub_role ($self->collect_all_roles) {      
        return 1 if $sub_role->name eq $role_name;            
    }
    return 0;
}));

## composition methods

my $_collect_all_roles = sub {
    my ($self, $seen) = @_;
    $seen ||= {};
    my @roles;
    foreach my $role (@{$self->roles}) {
        unless (exists $seen->{$role->name}) {
            $seen->{$role->name}++;             
            push @roles => $role;
            push @roles => $role->collect_all_roles($seen) if scalar @{$role->roles};            
        }              
    }
    return @roles;
};
$::Role->add_method('collect_all_roles' => ::make_method($_collect_all_roles));

my $_resolve = sub {
    my ($self) = @_;
    my @subroles = $self->collect_all_roles;
    foreach my $role (@subroles) {
        foreach my $method_name ($role->get_method_list) {
            # if we have a conflict, but it is not, or has 
            # not already been stubbed out then ...
            if ($self->has_method($method_name)) {
                # we stub the method for the class to implement
                #$self->add_method($method_name => undef);
            }
            else {
                # no conflicts ... add method
                $self->add_method($method_name => $role->get_method($method_name));
            }
        }
        foreach my $attr_name ($role->get_attribute_list) {
            # same as for methods, if we have the attribute
            # and it is not already a stub then ...
            if ($self->has_attribute($attr_name)) {
                # we stub it out ...
                #$self->add_attribute($attr_name => undef);
            }
            else {
                # no conflicts ... add attribute
                $self->add_attribute($attr_name => $role->get_attribute($attr_name));
            }
        }        
    }  
    $self->add_method('does' => ::make_method(sub{
        my ($self, $role_name) = @_;
        ::opaque_instance_class($self)->does($role_name);
    })) if $self != $::Class && !$self->isa('Role');
};
$::Role->add_method('resolve' => ::make_method($_resolve));

## ----------------------------------------------------------------------------
## BOOTSTRAPPING ROLES
## ----------------------------------------------------------------------------

# stub this for the moment ...
$::Class->add_method('collect_all_roles' => ::make_method(sub { $::Role }));

# manually add the ::Role
# NOTE:
# this is done again
::opaque_instance_add_new_attribute($::Class => '@:roles');  
::opaque_instance_attr($::Class => '@:roles') = [ $::Role ];
::opaque_instance_attr($::Role  => '@:roles') = [ $::Role ];
$_resolve->($::Class);

# restore this correctly
$::Class->add_method('collect_all_roles' => ::make_method($_collect_all_roles));

## ----------------------------------------------------------------------------
## END BOOTSTRAPPING ROLES
## ----------------------------------------------------------------------------

## NOTE:
## we want to prevent the default version 
## of this from taking over which would make
## things like: FooRole.isa(FooRole) true,
## when they really should not be true
$::Role->add_method('isa' => ::make_method(sub { 
    my ($self, $class_name) = @_;
    return undef unless $class_name;
    return 1 if $class_name eq 'Role';
    return 0;
}));

# TODO ... implement the package interface

$::Role->add_method('FETCH' => ::make_method(sub {
    my ($self, $label) = @_;
    (defined $label && $label)
        || confess "Cannot FETCH at (" . ($label || 'undef') . ")";
    if ($label =~ /^\&(.*)$/) {
        # check for instance method
        return $self->has_method($1) ? 
                    $self->get_method($1) 
                    :
                    # if all else fails, maybe it is 
                    # a sub, so we just call next METHOD
                    ::next_METHOD();
    }   
    # XXX -
    # this reg-exp is probably not correct ...
    elsif ($label =~ /^.(\.|\:).*$/) {
        # check for instance attribute
        return $self->has_attribute($label) ?
                    $self->get_attribute($label)
                    :
                    # class attributes are really just package 
                    # variables with an "our" scope... so we 
                    # just go to the next method for them 
                    ::next_METHOD();
    } 
    else {
        ::next_METHOD();
    }
}));


$::Role->add_method('STORE' => ::make_method(sub {
    my $self = shift;
    # ...
}));

=pod

-------------------------------------------------------------------------------

This is a rough sketch of that role(Role) might look like. 

role Role {
    has @:roles is rw;  # makes the roles mutator/accessor

    # -----------------------------------------------      
    # this collects a unique list of roles 
    # gather recursively, &resolve uses it
    # to gather all the roles to be 'resolved'
    # -----------------------------------------------      
    method collect_all_roles () { "<implemented>" }  

    # -----------------------------------------------    
    # NOTE:
    # I am not sure I need to define these attrs in 
    # the Role it can remain an implementation detail 
    # for the methods described below.
    # -----------------------------------------------
    # has %:methods;
    # has %:attributes;
    # -----------------------------------------------    
      
    # -----------------------------------------------  
    # collects all the roles, and applies them to the
    # invocant, which usually will be a class, but it
    # can be a role too.
    # -----------------------------------------------    
    method resolve () { "<implemented>" }

    # -----------------------------------------------    
    # NOTE:
    # &resolve requires the following interface
    # to be implemented, so it makes abstract 
    # methods which must be implemented
    # -----------------------------------------------

    method add_method;
    method get_method;
    method has_method;
    method get_method_list;        
    
    method add_attribute;
    method get_attribute;
    method has_attribute;
    method get_attribute_list;            
}

-------------------------------------------------------------------------------
Random Thoughts on Role state:
-------------------------------------------------------------------------------

What if Roles could provide their own BUILD submethods? This would 
mean that BUILD methods would need to be collected in some way.
So that BUILDALL not only ran the Class BUILD submethod, but any
Role BUILD submethods. It is important that submethods are used
because they will not get inherited. This provides more predictable 
behavior to class consturction because it isolates the Role specifc
code to the class that consumed it.

-------------------------------------------------------------------------------

With this Role meta-model , any Class instances, themselves does Role since they
are just instances of Class which does Role too. So this means that all classes
and roles can be polymorphic to one another. 

Because instances of class all conform to the Role interface/API they are 
interchangable with one another, classes and Roles are unified. THis is like
Scala IIRC. 


=cut

1;

__END__

=pod

=head1 NAME

psyche

=head1 DESCRIPTION

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut


