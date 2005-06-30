
package Perl6::Object;

use strict;
use warnings;

use Data::Dumper;

our $DEBUG = 0;
sub debug { return unless $DEBUG; print ">>> ", @_, "\n" }

use Scalar::Util 'blessed';

sub new_instance {
    my ($class, %params) = @_;
    return $class->class->metaclass->new_instance(%params);
}

sub isa {
    my ($self, $class) = @_;
    return undef unless $class;
    return $self->class->metaclass->is_a($class . '::Class');
}

sub can {
    my ($self, $label) = @_;
    if (blessed($self)) {
        return $self->class->metaclass->responds_to($label);
    }
    else {
        return $self->class->metaclass->class_responds_to($label);
    }
}

sub class {
    my ($self) = @_;
    $self = blessed($self) if blessed($self);
    return $self . '::Class';    
}

sub get_value {
    my ($self, $label) = @_;
    $self->{instance_data}->{$label};
}

sub set_value {
    my ($self, $label, $value) = @_;
    my $prop = $self->class->metaclass->find_attribute_spec($label)
        || die "Perl6::Attribute ($label) no found";

    # since we are not private, then check the type
    # assuming there is one to check ....
    if (my $type = $prop->type()) {
        if ($prop->is_array()) {
            (blessed($_) && ($_->isa($type) || $_->does($type))) 
                || die "IncorrectObjectType: expected($type) and got($_)"
                    foreach @$value;                        
        }
        else {
            (blessed($value) && ($value->isa($type) || $value->does($type))) 
                || die "IncorrectObjectType: expected($type) and got($value)";                        
        }
    }  
    else {
        (ref($value) eq 'ARRAY') 
            || die "You can only asssign an ARRAY ref to the label ($label)"
                if $prop->is_array();
        (ref($value) eq 'HASH') 
            || die "You can only asssign a HASH ref to the label ($label)"
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
            $method = $self->class->metaclass->find_method_in_superclasses($label);
        }
        else {
            $method = $self->class->metaclass->find_method($label);
        }
        (blessed($method) && $method->isa('Perl6::Method')) 
            || die "Method ($label) not found for instance ($self)";
        @return_value = $method->call($self, @_);        
    }
    else {
        my $method = $self->class->metaclass->find_class_method($label);
        
        (defined $method) 
            || die "Method ($label)  not found for class ($self)";
        @return_value = $method->call($self->class->metaclass, @_);
    }
    return wantarray ?
                @return_value
                :
                $return_value[0];
}

package Perl6::Object::Class;

use Perl6::MetaClass;

my $META;
sub metaclass {
    my ($class) = @_;
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
