#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/metamorph.pl" };

# ... this makes ::Object

$::Object = undef;

# The 'Object' class
$::Object = ::create_class('$:name' => 'Object');

## class methods

$::Object->add_method('new' => ::make_class_method(sub {
    my ($class, %params) = @_;
    return $class->class::bless(undef, %params);    
}, $::Object));

$::Object->add_method('bless' => ::make_class_method(sub {
    my ($class, $canidate, %params) = @_;
    $canidate ||= 'P6opaque'; # opaque is our default
    my $self = $class->class::CREATE(repr => $canidate, %params);
    $self->BUILDALL(%params);
    return $self;  
}, $::Object));

$::Object->add_method('CREATE' => ::make_class_method(sub { 
    my ($class, %params) = @_;
    ($params{repr} eq 'P6opaque') 
        || confess "Sorry, No other types other than 'P6opaque' are currently supported";    
    # this just gathers all the 
    # attributes that were defined
    # for the instances.
    my %attrs;
    my $dispatcher = $class->dispatcher(':descendant');
    while (my $c = ::WALKCLASS($dispatcher)) {
        foreach my $attr ($c->get_attribute_list()) {
            my $attr_obj = $c->get_attribute($attr);
            $attrs{$attr} = ::instantiate_attribute_container($attr_obj);
        }
    }
    # this is our P6opaque data structure
    # it's nothing special, but it works :)
    my $self = ::create_opaque_instance(\$class, %attrs);              
    # and now return it ...
    return $self;
}, $::Object));

$::Object->add_method('isa' => ::make_class_method(sub { 
    my ($self, $class) = @_;
    return undef unless $class;
    return $self->is_a($class);
}, $::Object));

$::Object->add_method('can' => ::make_class_method(sub { 
    my ($self, $label) = @_;
    return undef unless $label;
    return ::WALKMETH($self->dispatcher(':canonical'), $label, for => 'class');
}, $::Object));

## submethods

$::Object->add_method('BUILD' => ::make_submethod(sub { 
    my ($self, %params) = @_;
    foreach my $key (keys %params) {
        # XXX -
        # The default BUILD method should accept
        # params which are not included in the 
        # attributes. It will do nothing with them
        # but it will allow them to exist.
        # - (see t_oo/submethods.t)
        ::opaque_instance_attrs($self)->{$key} = $params{$key}
            # NOTE:
            # this is an ugly way to do this, ideally
            # we would peek into the instance structure
            # itself and see if we had the spot, and
            # otherwise ignore it ... but this will do
            if ::opaque_instance_class($self)->find_attribute_spec($key);
    }
}, $::Object));

## instance methods

$::Object->add_method('BUILDALL' => ::make_method(sub { 
    my ($self, %params) = @_;
    my $dispatcher = ::opaque_instance_class($self)->dispatcher(':descendant');
    while (my $method = ::WALKMETH($dispatcher, 'BUILD')) {                      
        $method->($Perl6::Submethod::FORCE, $self, %params);                  
    }      
}, $::Object));

$::Object->add_method('DESTROYALL' => ::make_method(sub { 
    my ($self) = @_;
    my $dispatcher = ::opaque_instance_class($self)->dispatcher(':ascendant');
    while (my $method = ::WALKMETH($dispatcher, 'DESTROY')) {  
        $method->($Perl6::Submethod::FORCE, $self);   
    }  
}, $::Object));

$::Object->add_method('id' => ::make_method(sub { ::opaque_instance_id(shift) }, $::Object));

$::Object->add_method('isa' => ::make_method(sub { 
    my ($self, $class) = @_;
    return undef unless $class;
    return ::opaque_instance_class($self)->is_a($class);    
}, $::Object));

$::Object->add_method('can' => ::make_method(sub { 
    my ($self, $label) = @_;    
    return undef unless $label;
    return ::WALKMETH(::opaque_instance_class($self)->dispatcher(':canonical'), $label, for => 'instance');    
}, $::Object));

1;
