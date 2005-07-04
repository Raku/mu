
package Perl6::Object;

use strict;
use warnings;

use Perl6::MetaClass;

use Scalar::Util 'blessed';
use Carp 'croak';

sub new {
    my ($class, %params) = @_;
    return $class->meta->new_instance(%params);
}

sub isa {
    my ($self, $class) = @_;
    return undef unless $class;
    return $self->meta->is_a($class);
}

sub can {
    my ($self, $label) = @_;
    if (blessed($self)) {
        return $self->meta->responds_to($label);
    }
    else {
        return $self->meta->class_responds_to($label);
    }
}

sub get_class_value {
    my ($self, $label) = @_;
    my $prop = $self->meta->find_class_attribute_spec($label)
        || croak "Cannot locate class property ($label) in class ($self)";
    $prop->get_value();
}

sub set_class_value {
    my ($self, $label, $value) = @_;
    my $prop = $self->meta->find_class_attribute_spec($label)
        || croak "Cannot locate class property ($label) in class ($self)";
    $prop->set_value($value);
}

sub get_value {
    my ($self, $label) = @_;
    $self->{instance_data}->{$label};
}

sub set_value {
    my ($self, $label, $value) = @_;
    my $prop = $self->meta->find_attribute_spec($label)
        || croak "Perl6::Attribute ($label) no found";

    # since we are not private, then check the type
    # assuming there is one to check ....
    if (my $type = $prop->type()) {
        if ($prop->is_array()) {
            (blessed($_) && ($_->isa($type) || $_->does($type))) 
                || croak "IncorrectObjectType: expected($type) and got($_)"
                    foreach @$value;                        
        }
        else {
            (blessed($value) && ($value->isa($type) || $value->does($type))) 
                || croak "IncorrectObjectType: expected($type) and got($value)";                        
        }
    }  
    else {
        (ref($value) eq 'ARRAY') 
            || croak "You can only asssign an ARRAY ref to the label ($label)"
                if $prop->is_array();
        (ref($value) eq 'HASH') 
            || croak "You can only asssign a HASH ref to the label ($label)"
                if $prop->is_hash();
    }                      

    # We are doing a 'binding' here by linking the $value into the $label
    # instead of storing into the container object available at $label
    # with ->store().  By that time the typechecking above will go away
    $self->{instance_data}->{$label} = $value;
}

sub AUTOLOAD {
    my $label = (split '::', our $AUTOLOAD)[-1];
    return if $label =~ /DESTROY/;
    my $self = shift;
    my @return_value;
    if (blessed($self)) {
        my $method;
        if ($label eq 'SUPER') {
            $label = shift;
            $method = $self->meta->find_method_in_superclasses($label);
        }
        else {
            $method = $self->meta->find_method($label);
        }
        (blessed($method) && $method->isa('Perl6::Method')) 
            || croak "Method ($label) not found for instance ($self)";
        @return_value = $method->call($self, @_);        
    }
    else {
        my $method = $self->meta->find_class_method($label);
        
        (defined $method) 
            || croak "Method ($label)  not found for class ($self)";
        @return_value = $method->call($self, @_);
    }
    return wantarray ?
                @return_value
                :
                $return_value[0];
}

my $META;
sub meta {
    my ($class) = @_;
    $class = blessed($class) if blessed($class);       
    no strict 'refs';
    ${$class .'::META'} ||= Perl6::MetaClass->new(name => $class);
}

1;

__END__

=pod

=head1 AUTHOR

Stevan Little stevan@iinteractive.com
Autrijus Tang autrijus@autrijus.org

=cut
