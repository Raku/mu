#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/metamorph.pl" };

# ... this makes ::Object

$::Object = undef;

# The 'Object' class
$::Object = $::Class->new();

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
}));

## instance methods

$::Object->add_method('BUILDALL' => ::make_method(sub { 
    my ($self, %params) = @_;
    my $dispatcher = ::opaque_instance_class($self)->dispatcher(':descendant');
    while (my $method = ::WALKMETH($dispatcher, 'BUILD')) {                      
        $method->($Perl6::Submethod::FORCE, $self, %params);                  
    }      
}));

$::Object->add_method('DESTROYALL' => ::make_method(sub { 
    my ($self) = @_;
    my $dispatcher = ::opaque_instance_class($self)->dispatcher(':ascendant');
    while (my $method = ::WALKMETH($dispatcher, 'DESTROY')) {  
        $method->($Perl6::Submethod::FORCE, $self);   
    }  
}));

$::Object->add_method('id' => ::make_method(sub { ::opaque_instance_id(shift) }));

$::Object->add_method('isa' => ::make_method(sub { 
    my ($self, $class_name) = @_;
    return undef unless $class_name;
    return ::opaque_instance_class($self)->isa($class_name);    
}));

$::Object->add_method('can' => ::make_method(sub { 
    my ($self, $label) = @_;   
    return undef unless $label;
    return ::WALKMETH(::opaque_instance_class($self)->dispatcher(':canonical'), $label);    
}));

1;
