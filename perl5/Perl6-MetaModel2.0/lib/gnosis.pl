#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/chaos.pl" };

# The 'Class' class -- placed here so ::create_class can refer to it
$::Class = undef;

sub ::create_class (%) {
    my (%attrs) = @_;
    return ::create_opaque_instance(
        # < a Class object is an instance of the Class class >
        \$::Class,
        (
            '@:MRO'              => [],
            '@:subclasses'       => [],
            '@:superclasses'     => [],
            '%:private_methods'  => {},
            '%:attributes'       => {},
            '%:methods'          => {},
            '%:class_attributes' => {},
            '%:class_methods'    => {},            
        )
    );
}

# The 'Class' class
$::Class = ::create_class();

## create the body of 'add_method' here,.. 
my $_add_method = sub {
    my ($self, $label, $method) = @_;
    (defined $label && $label)
        || confess "You must supply a valid method label";
    (blessed($method))
        || confess "A method must be a blessed Perl6::Method object";
     
=pod  
    # NOTE:
    # this is useful to some degree when doing profiling
    # however this only captures the name of the outer method
    # and not the other closures inside 
    
    use Sub::Name ();
    
    $method = Sub::Name::subname((
                    $self == $::Class ?
                        'Class'
                        :
                        $self == $::Object ?
                            'Object'
                            :
                            $self == $::Package ?
                                'Package'
                                :
                                $self == $::Module ?
                                    'Module'
                                    :
                                    $self->name) . '::' . $label, $method);    
=cut
        
    if (blessed($method) eq 'Perl6::Method'   ||
        blessed($method) eq 'Perl6::Submethod') {
        ::opaque_instance_attrs($self)->{'%:methods'}->{$label} = $method;
    }
    elsif (blessed($method) eq 'Perl6::ClassMethod') {
        ::opaque_instance_attrs($self)->{'%:class_methods'}->{$label} = $method;                
    }
    elsif (blessed($method) eq 'Perl6::PrivateMethod') {
        ::opaque_instance_attrs($self)->{'%:private_methods'}->{$label} = $method;                
    }            
    else {
        confess "I do not recognize the method type ($method)";
    }
};

# and use it to add itself to the $::Class
$_add_method->($::Class, 'add_method', ::make_method($_add_method, $::Class));

# ... now we have all we need to construct our $::Class 

1;
