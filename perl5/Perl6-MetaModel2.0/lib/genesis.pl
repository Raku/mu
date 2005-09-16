#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/pneuma.pl" };

## Now create some of the other things we need ...

## See http://article.gmane.org/gmane.comp.lang.perl.perl6.language/4599 for 
## more info on the Class isa Module isa Package isa Object thing.

## See http://article.gmane.org/gmane.comp.lang.perl.perl6.language/4956 for
## disucssion on how Packages work

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
}));

$::Package->add_method('FETCH' => ::make_method(sub {
    my ($self, $label) = @_;
    (defined $label && $label)
        || confess "Cannot FETCH at (" . ($label || 'undef') . ")";
    ::opaque_instance_attrs($self)->{'%:namespace'}->{$label};
}));

$::Package->add_method('STORE' => ::make_method(sub {
    my ($self, $label, $value) = @_;
    (defined $label && $label)
        || confess "Cannot STORE at (" . ($label || 'undef') . ")";   
    # NOTE: special case
    # if we are storing CODE in the package, we 
    # need to wrap it so that $?PACKAGE is bound
    # correctly within it
    if ($label =~ /^\&/ && ref($value) eq 'CODE') {
        $value = ::wrap_package_sub($value, $self);
    } 
    ::opaque_instance_attrs($self)->{'%:namespace'}->{$label} = $value;
}));

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
}));

$::Module->add_method('authority' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attrs($self)->{'$:authority'} = shift if @_;        
    ::opaque_instance_attrs($self)->{'$:authority'};
}));

$::Module->add_method('identifier' => ::make_method(sub {
    return join '-' => ($::SELF->name, $::SELF->version, ($::SELF->authority || ()));
}));

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
                    # class attributes are really just package 
                    # variables with an "our" scope... so we 
                    # just go to the next method for them 
                    ::next_METHOD();
    } 
    else {
        ::next_METHOD();
    }    
}));

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
    elsif ($label =~ /^.(\.|\:).*$/ && (blessed($value) && $value->isa('Perl6::Attribute'))) {
        # only store instance attributes with the meta model, 
        # class attributes are just package scoped "our" variables
        # so they are added to the Package normally
        return $self->add_attribute($label, $value);
    } 
    else {
        ::next_METHOD();
    }
}));

$::Class->add_method('does' => ::make_method(sub {
    undef;
}));

$::Object->add_method('does' => ::make_method(sub {
    # ... this should forward to $::Class?
    undef;
}));

## ----------------------------------------------------------------------------
## Roles

# Questions on Role methods sent to p6l 
# - see recent thread: Concerning Roles and $?ROLE

# More questions ...
# - should a Class hold references to the Roles it does?
# - should Class does Role? or Object does Role?

$::Role = $::Class->new('$:name' => 'Role');

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
    ::opaque_instance_attrs($self)->{'%:methods'}->{$label} = (
        defined $method ? 
            ::wrap_role_method($method, $self)
            :
            ::make_stub_method());
}));

$::Role->add_method('get_method' => ::make_method(sub {
    my ($self, $label) = @_;
    ::opaque_instance_attrs($self)->{'%:methods'}->{$label};
}));

$::Role->add_method('has_method' => ::make_method(sub {
    my ($self, $label) = @_;
    exists ::opaque_instance_attrs($self)->{'%:methods'}->{$label} ? 1 : 0;
}));

$::Role->add_method('is_method_stub' => ::make_method(sub {
    my ($self, $label) = @_;
    blessed(::opaque_instance_attrs($self)->{'%:methods'}->{$label}) eq 'Perl6::StubMethod';
}));

$::Role->add_method('get_method_list' => ::make_method(sub {
    keys %{::opaque_instance_attrs($::SELF)->{'%:methods'}};
}));

## attributes

$::Role->add_method('add_attribute' => ::make_method(sub {
    my ($self, $label, $attribute) = @_;
    ::opaque_instance_attrs($self)->{'%:attributes'}->{$label} = (
        defined $attribute ?
            $attribute
            :
            ::make_stub_attribute($label));
}));

$::Role->add_method('get_attribute' => ::make_method(sub {
    my ($self, $label) = @_;    
    ::opaque_instance_attrs($self)->{'%:attributes'}->{$label};
}));

$::Role->add_method('has_attribute' => ::make_method(sub {
    my ($self, $label) = @_;    
    exists ::opaque_instance_attrs($self)->{'%:attributes'}->{$label} ? 1 : 0;
}));

$::Role->add_method('is_attribute_stub' => ::make_method(sub {
    my ($self, $label) = @_;    
    blessed(::opaque_instance_attrs($self)->{'%:attributes'}->{$label}) eq 'Perl6::StubAttribute';
}));

$::Role->add_method('get_attribute_list' => ::make_method(sub {
    keys %{::opaque_instance_attrs($::SELF)->{'%:attributes'}};
}));

## subroles

$::Role->add_method('roles' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attrs($self)->{'@:roles'} = shift if @_;
    ::opaque_instance_attrs($self)->{'@:roles'};
}));

## introspective methods

$::Role->add_method('does' => ::make_method(sub {
    my ($self, $role_name) = @_;
    return undef unless defined $role_name;
    return 1 if $self->name eq $role_name;
    foreach my $sub_role (@{$self->roles}) {
        return 1 if $sub_role->does($role_name);            
    }
    return 0;
}));

$::Role->add_method('collect_all_roles' => ::make_method(sub {
    my ($self, $seen) = @_;
    $seen ||= {};
    my @roles;
    foreach my $role ($self, @{$self->roles}) {
        unless (exists $seen->{$role->name}) {
            $seen->{$role->name}++;
            push @roles => $role;
            push @roles => $role->collect_all_roles($seen) if scalar @{$role->roles};            
        }
    }
    #warn Data::Dumper::Dumper $seen;
    return @roles;
}));

## composition methods

$::Role->add_method('resolve' => ::make_method(sub {
    my $composite;
    $composite = $::Role->new('$:name' => 'Composite<' . (0 + \$composite) . '>');
    my @subroles = $::SELF->collect_all_roles;
    foreach my $role (@subroles) {
        foreach my $method_name ($role->get_method_list) {
            # if we have a conflict, but it is not, or has 
            # not already been stubbed out then ...
            if ($composite->has_method($method_name) &&
                !$composite->is_method_stub($method_name)) {
                # we stub the method for the class to implement
                $composite->add_method($method_name => undef);
            }
            else {
                # no conflicts ... add method
                $composite->add_method($method_name => $role->get_method($method_name));
            }
        }
        foreach my $attr_name ($role->get_attribute_list) {
            # same as for methods, if we have the attribute
            # and it is not already a stub then ...
            if ($composite->has_attribute($attr_name) &&
                !$composite->is_attribute_stub($attr_name)) {
                # we stub it out ...
                $composite->add_attribute($attr_name => undef);
            }
            else {
                # no conflicts ... add attribute
                $composite->add_attribute($attr_name => $role->get_attribute($attr_name));
            }
        }        
    }
    $composite->roles(\@subroles);  
    return $composite;
}));

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
    my $self = shift;
    # ...
}));


$::Role->add_method('STORE' => ::make_method(sub {
    my $self = shift;
    # ...
}));

1;
