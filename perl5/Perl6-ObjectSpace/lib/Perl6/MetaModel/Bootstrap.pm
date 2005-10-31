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

# for sending messages to an object
sub ::send {
    my ($inv, $label, $args) = @_;
    (blessed($label) && $label->isa('str'))
        || confess "Label must be a string";
        
    my $methods = $inv->get_attr(str->new('%:methods'));
    ($methods->exists($label))
        || confess "Method cannot be found";
    return $methods->fetch($label)->do($args);
}

$::ENV = Perl6::Runtime::get_top_level_env();

## now begin creating the metamodel

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

{
    # add the first method
    my $_add_method = method->new(
            $::ENV,
            list->new(str->new('$self:'), str->new('$label'), str->new('$method')),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');
                my $method = $e->get('$method');
                ($label->isa('str'))
                    || confess "A label must be a str type";                        
                ($method->isa('method'))
                    || confess "A method must be a method type";
                $self->get_attr(str->new('%:methods'))->store($label, $method);
            }
        );

    # and use it to add itself to the $::Class
    $_add_method->do(list->new($::Class, str->new('add_method'), $_add_method));
}

::send($::Class, str->new('add_method'), list->new(
        $::Class, str->new('has_method'), method->new(
            $::ENV,
            list->new(str->new('$self:'), str->new('$label')),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');  
                ($label->isa('str'))
                    || confess "A label must be a str type";
                $self->get_attr(str->new('%:methods'))->exists($label);                                                  
            }
        )
    )
);