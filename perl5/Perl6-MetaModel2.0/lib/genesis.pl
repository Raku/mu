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

## ----------------------------------------------------------------------------
## Now we make Class conform to the Package interface

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


1;
