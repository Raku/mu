#!/usr/bin/perl

use strict;
use warnings;
use Perl6::MetaModel::Chaos;

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
            '%:class_methods'    => {},                      
        )
    );
}

# The 'Class' class
$::Class = ::create_class();

## create the body of 'add_method' here,.. 
{
    my $_add_method = sub {
        my ($self, $label, $method) = @_;
        (defined $label && $label)
            || confess "You must supply a valid method label";
        (blessed($method))
            || confess "A method must be a blessed Perl6::Method object";
    
        ::bind_method_to_class($method, $self);
        
        if (blessed($method) eq 'Perl6::Method'     ||
            blessed($method) eq 'Perl6::Submethod'  ||
            blessed($method) eq 'Perl6::ClassMethod') {
            ::opaque_instance_attr($self => '%:methods')->{$label} = $method;
        }
        #elsif (blessed($method) eq 'Perl6::ClassMethod') {
        #    ::opaque_instance_attr($self => '%:class_methods')->{$label} = $method;                
        #}
        elsif (blessed($method) eq 'Perl6::PrivateMethod') {
            ::opaque_instance_attr($self => '%:private_methods')->{$label} = $method;                
        }            
        else {
            confess "I do not recognize the method type ($method)";
        }
    };

    # and use it to add itself to the $::Class
    $_add_method->($::Class, 'add_method', ::make_method($_add_method));
}
# ... now we have all we need to construct our $::Class 

1;

__END__

=pod

=head1 NAME

gnosis

=head1 DESCRIPTION

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
