
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
    if (my $prop = $self->class->metaclass->find_attribute_spec($label)) {
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
    }  
    else {
        die "Perl6::Attribute ($label) no found";
    }      
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

=head1 NAME

MCP - MetaClass Perl

=head1 DESCRIPTION

This provides and example implemetation of the programming environment described in the paper
"A Core Calculus for MetaClasses". The main idea of the system is that objects not only have
superclasses, but also have "kinds", which are represented by metaclasses. Think of this as a
"kind-of" relationship, much as a (super|sub)class system is an "is-a" relationship and a
delegation relationship can be described as a "has-a" relationship.

The simplest way to view this is that class methods/attributes are part of an object's "kind".
While instance methods/attributes are descibed by the object's "class". This is really the same
as the idea that each class is really an instance of a metaclass which describes the class itself.

The real change here is in the division of responsibilties. Where in a traditional metaclass based
system, each metaclass would describe the class, and through that describe the behaviors of the
instances. This system has a "kind" which describes the behavior of the class itself, while the
"class" would define the behavior of it's instances. This allows for greater variance since the
"kind" is not restricted to just describing the "class" alone, but can itself have a richer inheritence
hierarchy and functionality.

=head1 EXAMPLE

The example used above is take (mostly) from the paper, to describe a biological relationship. Here
is how it might be described in "regular" OO (using pseudo-Perl(5|6)):

   class Species extends Object {}
   class TheEagle extends Species {}
   class Eagle extends Object {}

   my $the_eagle_species = TheEagle->new();

   Eagle $harry = Eagle->new(name => 'Harry');
   $the_eagle_species->add_animal($harry);

Here the link between an Eagle instance and TheEagle species is handled manually, and is not inherant
in the class hierarchy itself. TheEagle instance would certainly need to be a singleton as well.

Now with the "kind" based it system not only handles some of the details for you, but the class heirarchy 
it creates ends up mapping to the real world much more smoothly.

    class Species kind Object extends Object {}
    class Eagle kind Species extends Object {}

    Eagle $harry = Eagle->new(name => 'Harry');

Now, this may seem like it does not do much more, or that what it does is somehow handled behind the scences
and therefore not much different then coding the above example differently. However, where it does vary is
that, it inherintly describes the relatioships better.

Species can keep track of their "instances", and since Eagle->new() is a class method and handled by Eagle's "kind"
as opposed to it's class. This means that Species can easily track each Eagle instance without polluting the Eagle
class itself.

In essence your class's "kind" becomes a singleton metaclass which can be used to store variables and information just as easy
as it can dictate behavior.

=head1 SEE ALSO

L<http://research.sun.com/projects/plrg/core-calculus.pdf>

=head1 AUTHOR

Stevan Little stevan@iinteractive.com

=cut
