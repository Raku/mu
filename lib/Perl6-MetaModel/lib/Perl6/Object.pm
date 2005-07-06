
package Perl6::Object;

use strict;
use warnings;

use Perl6::MetaClass;

use Scalar::Util 'blessed';
use Carp 'croak';

sub new {
    my ($class, %params) = @_;
    return $class->bless(undef, %params);
}

sub bless : method {
    my ($class, $canidate, %params) = @_;
    $canidate ||= 'P6opaque'; # opaque is our default
    my $instance_structure = $class->CREATE(repr => $canidate, %params);
    my $self = CORE::bless($instance_structure => $class);
    $self->BUILDALL(%params);
    return $self;
}

sub CREATE {
    my ($class, %params) = @_;
    ($params{repr} eq 'P6opaque') 
        || croak "Sorry, No other types other than 'P6opaque' are currently supported";    
    
    my %attrs;
    $class->meta->traverse_post_order(sub {
        my $c = shift;
        foreach my $attr ($c->get_attribute_list) {
            my $attr_obj = $c->get_attribute($attr);
            $attrs{$attr} = $attr_obj->instantiate_container;
        }
    }); 
        
    return {
        class         => $class->meta,
        instance_data => \%attrs,
    };         
}

sub BUILDALL {
    my ($self, %params) = @_;
    # XXX - hack here to call Perl6::Object::BUILD
    $self->Perl6::Object::BUILD(%params);
    # then we post order traverse the rest of the class
    # hierarchy. This will all be fixed when Perl6::Object
    # is properly bootstrapped
    $self->meta->traverse_post_order(sub {
        my $c = shift;
        $c->get_method('BUILD')->call($self, %params) if $c->has_method('BUILD');        
    });    
}

sub BUILD {
    my ($self, %params) = @_;
    $self->set_value($_ => $params{$_}) foreach keys %params;
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
        return $self->meta->responds_to($label, for => 'Class');
    }
}

sub get_class_value {
    my ($self, $label) = @_;
    my $prop = $self->meta->find_attribute_spec($label, for => 'Class')
        || croak "Cannot locate class property ($label) in class ($self)";
    $prop->get_value();
}

sub set_class_value {
    my ($self, $label, $value) = @_;
    my $prop = $self->meta->find_attribute_spec($label, for => 'Class')
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
        my $method = $self->meta->find_method($label, for => 'Class');

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
