#!/usr/bin/perl

use strict;
use warnings;

use Perl6::Runtime;

use Perl6::Core::Nil;
use Perl6::Core::Str;
use Perl6::Core::Num;
use Perl6::Core::Ref;
use Perl6::Core::Hash;
use Perl6::Core::List;

use Perl6::MM::Opaque;
use Perl6::MM::Method;
use Perl6::MM::Attribute;

use Carp 'confess';
use Scalar::Util 'blessed';

# create our basic variables/functions for 
# the metamodel that the runtime needs to 
# have available ...
$::Class   = undef;
$::Object  = undef;
$::Module  = undef;
$::Package = undef;

# for sending messages to an opaque object 
# (very primitive at the moment)
sub opaque::send {
    my ($inv, $label, @args) = @_;
    
    $label = str->new($label) 
        unless (blessed($label) && $label->isa('str'));
    
    my $args;
    if (blessed($args[0]) && $args[0]->isa('list')) {
        $args[0]->unshift($inv);
        $args = $args[0];
    }
    else {
        $args = list->new($inv, @args);
    }
    
    # gather all the classes to look through
    my @classes = ($inv);
    # we take the MRO first, however at some early
    # stages of the bootstrap, this is not yet 
    # populated with anything, so ....
    my @supers = $inv->get_attr(str->new('@:MRO'))->to_native;
    # if nothing is in MRO, we take the superclasses
    # because we know that is there ...
    @supers = $inv->get_attr(str->new('@:superclasses'))->to_native 
        if scalar @supers == 0;    
        
    foreach my $class (@classes) {
        my $methods = $inv->get_attr(str->new('%:methods'));
        return $methods->fetch($label)->do($args) 
            if $methods->exists($label);             
    }
    confess "Method ($label) not found in \$::Class";             
}

$::ENV = Perl6::Runtime::get_top_level_env();

## ----------------------------------------------------------------------------
## now begin creating the metamodel

=pod

class Class {
    has @:MRO;
    has @:subclasses;
    has @:superclasses;
    has %:private_methods;
    has %:attributes;
    has %:methods;
}

=cut

# create the basic Class
$::Class = opaque->new(
        reference->new($opaque::NULL_OBJECT),
        hash->new(
            attribute->new('@:MRO')             => list->new(),
            attribute->new('@:subclasses')      => list->new(),
            attribute->new('@:superclasses')    => list->new(),
            attribute->new('%:private_methods') => hash->new(),
            attribute->new('%:attributes')      => hash->new(),
            attribute->new('%:methods')         => hash->new(),                     
        )
    );
    
# link Class back to Class
$::Class->change_class(reference->new($::Class));

=pod

method add_method ($self: str $label, method $method) returns nil {
    %:methods{$label} = $method;
}

=cut

{
    # add the first method
    my $_add_method = method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'), 
                symbol->new('$label' => 'str'), 
                symbol->new('$method' => 'method')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');
                my $method = $e->get('$method');
                $self->get_attr(str->new('%:methods'))->store($label, $method);
            }
        );

    # and use it to add itself to the $::Class
    $_add_method->do(list->new($::Class, str->new('add_method'), $_add_method));
}

=pod

method has_method ($self: str $label) returns bit { 
    %:methods.exists($label);
}

=cut

$::Class->send('add_method' => (
    # method label
        str->new('has_method'), 
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'), 
                symbol->new('$label' => 'str')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');  
                $self->get_attr(str->new('%:methods'))->exists($label);                                                  
            }
        )
    )
);

=pod

method new ($class: hash %params) returns opaque {
    $class.bless(undef, %params);
}

=cut

$::Class->send('add_method' => (
    # method label
        str->new('new'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$class:' => 'opaque'), 
                symbol->new('%params' => 'hash')
            ),
            sub {
                my $e      = shift;
                my $class  = $e->get('$class:');
                my $params = $e->get('%params');
                return $class->send('bless' => (nil->new(), $params));    
            }
        )
    )
);

=pod

method bless ($class: str $canidate, hash %params) returns opaque {
    $canidate //= 'P6opaque';
    my $self = $class.CREATE(repr => $canidate, %params);
    $self.BUILDALL($params);
    return $self;
}

=cut

$::Class->send('add_method' => (
    # method label
        str->new('bless'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$class:' => 'opaque'), 
                symbol->new('$canidate' => 'str'), 
                symbol->new('%params' => 'hash')
            ),
            sub {
                my $e        = shift;
                my $class    = $e->get('$class:');
                my $canidate = $e->get('$canidate');                
                my $params   = $e->get('%params');

                # p6opaque is our default
                $canidate = str->new('P6opaque') if $canidate == $nil::NIL; 
                $params->store(str->new('repr') => $canidate);
                
                # create and init the object
                my $self = $class->send('CREATE' => $params);
                $self->send('BUILDALL' => ($params));
                return $self;                  
            }
        )
    )
);

