
package Perl6::MetaClass;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'croak';

use constant INSTANCE_TABLE => 'class_definition';
use constant CLASS_TABLE    => 'class_data';

sub new {
    my ($class, %params) = @_;
    my $meta = bless {
        name         => undef,
        superclasses => [],
        class_definition => {
            methods      => {},
            attributes   => {},
        },
        class_data => {
            methods      => {},
            attributes   => {},
        },        
    }, $class;
    $meta->name($params{name}) if exists $params{name};
    $meta->superclasses($params{superclasses}) if exists $params{superclasses};    
    return $meta;
}

sub name {
    my ($self, $name) = @_;
    $self->{name} = $name if defined $name;
    $self->{name};
}

sub is_a {
    my ($self, $class) = @_;
    $class = $class->name if blessed($class) && $class->isa('Perl6::MetaClass');
    return 1 if $self->name eq $class;
    foreach my $super (@{$self->superclasses}) {
        return 1 if $super->is_a($class);
    }
    return 0;    
}

sub superclasses {
    my ($self, $superclasses) = @_;
    if (defined $superclasses) {
        (ref($superclasses) eq 'ARRAY')
            || croak "BadType : You must pass the superclasses as an ARRAY ref";
        (blessed($_) && $_->isa('Perl6::MetaClass'))
            || croak "IncorrectObjectType : A superclass must be a Perl6::MetaClass instance"
                foreach @{$superclasses};
        $self->{superclasses} = $superclasses;    
    }
    $self->{superclasses};
}

sub class_precedence_list {
    my ($self, $seen) = @_;
    $seen ||= {};
    my @class_precedence_list;
    foreach my $super (@{$self->superclasses}) {
        unless (exists $seen->{$super}) {
            $seen->{$super}++;
            push @class_precedence_list => (
                $super, 
                $super->class_precedence_list($seen)
            );        
        }
    }
    return @class_precedence_list;
}

sub traverse_pre_order {
    my ($self, $visitor) = @_;
    $visitor->($self);
    foreach my $super (@{$self->superclasses}) {
        $super->traverse_pre_order($visitor);
    }
}

sub traverse_post_order {
    my ($self, $visitor) = @_;
    foreach my $super (@{$self->superclasses}) {
        $super->traverse_post_order($visitor);
    }
    $visitor->($self);    
}

## INSTANCE CREATION

sub new_instance {
    my ($self, %params) = @_;

    my %attrs;
    $self->traverse_post_order(sub {
        my $c = shift;
        foreach my $attr ($c->get_attribute_list) {
            my $attr_obj = $c->get_attribute($attr);
            $attrs{$attr} = $attr_obj->instantiate_container;
        }
    });    


    # XXX - This should actually be in the default
    # Object::BUILD method, and not happen 
    # automagically here!
    $attrs{$_} = $params{$_} foreach keys %params;

    my ($class_name) = ($self->name);

    my $instance = bless {
        class         => $self,
        instance_data => \%attrs,
    }, $class_name;

    $self->traverse_post_order(sub {
        my $c = shift;
        $c->get_method('BUILD')->call($instance, %params) if $c->has_method('BUILD');        
    });    

    return $instance;
}

## METHODS

# Instance Methods

sub add_method {
    my ($self, $label, $method) = @_;
    (defined $label && defined $method)
        || croak "InsufficientArguments : you must provide a method and a label";
    (blessed($method) && $method->isa('Perl6::Method'))
        || croak "IncorrectObjectType : Method must be a Perl6::Method object got($method)";
    my $method_table;
    if ($method->isa('Perl6::Instance::Method')) {
        $method_table = INSTANCE_TABLE;
    }
    elsif ($method->isa('Perl6::Class::Method')) {
        $method_table = CLASS_TABLE;
    }
    elsif ($method->isa('Perl6::SubMethod')) {
        # XXX - this is flat out wrong, but we 
        # will leave it for now, until we have
        # proper sub-methods
        $method_table = INSTANCE_TABLE; 
    }    
    else {
        croak "Incorrect Object Type : I dont know what to do with ($method)";
    }    
    $self->{$method_table}->{methods}->{$label} = $method;
}

sub get_method {
    my ($self, $label, %params) = @_;
    (defined $label)
        || croak "InsufficientArguments : you must provide a label";
    $self->{$self->_which_table(\%params)}->{methods}->{$label};
}

sub has_method {
    my ($self, $label, %params) = @_;
    $self->get_method($label, %params) ? 1 : 0;    
}

# XXX - Should this use the class_precedence_list?
sub find_method {
    my ($self, $label, %params) = @_;
    return $self->get_method($label, %params) if $self->has_method($label, %params);
    return $self->find_method_in_superclasses($label, %params);
}

sub find_method_in_superclasses {
    my ($self, $label, %params) = @_;
    foreach my $super (@{$self->superclasses}) {
        my $method = $super->find_method($label, %params);
        return $method if defined $method;
    }
    return undef;
}

sub responds_to {
    my ($self, $label) = @_;
    $self->find_method($label) ? 1 : 0;    
}

# Class Methods
# XXX -- This is a straight copy + paste from Instance Methods; we probably
#        want a generalised intermediate dispatch for the visitor methods
#        that can add the class_ prefix to the parameters in the eg. _get_uniq
#        calls so we don't end up maintaining two copies of traversal
#        (but this can wait a bit)

sub add_class_method {
    my ($self, $label, $method) = @_;
    (defined $label && defined $method)
        || croak "InsufficientArguments : you must provide a method and a label";
    (blessed($method) && $method->isa('Perl6::Class::Method'))
        || croak "IncorrectObjectType : Method must be a Perl6::Class::Method object got($method)";
    $self->{class_data}->{methods}->{$label} = $method;
}

sub get_class_method {
    my ($self, $label) = @_;
    (defined $label)
        || croak "InsufficientArguments : you must provide a label";
    $self->{class_data}->{methods}->{$label};
}

sub has_class_method {
    my ($self, $label) = @_;
    $self->get_class_method($label) ? 1 : 0;    
}

# XXX - Should this use the class_precedence_list?
sub find_class_method {
    my ($self, $label) = @_;
    return $self->get_class_method($label) if $self->has_class_method($label);
    return $self->find_class_method_in_superclasses($label);
}

sub find_class_method_in_superclasses {
    my ($self, $label) = @_;
    foreach my $super (@{$self->superclasses}) {
        my $method = $super->find_class_method($label);
        return $method if defined $method;
    }
    return undef;
}

sub class_responds_to {
    my ($self, $label) = @_;
    $self->find_class_method($label) ? 1 : 0;    
}

## ATTRIBUTES

# Instance Attributes

sub add_attribute {
    my ($self, $label, $attribute) = @_;
    (defined $label && defined $attribute)
        || croak "InsufficientArguments : you must provide an attribute and a label";
    (blessed($attribute) && $attribute->isa('Perl6::Instance::Attribute'))
        || croak "IncorrectObjectType : Attributes must be a Perl6::Instance::Attribute instance got($attribute)";

    if ($attribute->is_public()) {
        unless ($self->has_method($attribute->accessor_name())) {
             $self->add_method($attribute->accessor_name() => Perl6::Instance::Method->new(
             $self->name(), sub {
                my ($self, $value) = @_;
                $self->set_value($label => $value) if defined $value;
                $self->get_value($label);
            }));        
        }
    }          

    $self->{class_definition}->{attributes}->{$label} = $attribute;
}

sub get_attribute {
    my ($self, $label, %params) = @_;
    (defined $label)
        || croak "InsufficientArguments : you must provide a label";
    $self->{$self->_which_table(\%params)}->{attributes}->{$label};
}

sub has_attribute {
    my ($self, $label, %params) = @_;
    $self->get_attribute($label, %params) ? 1 : 0;
}

sub get_attribute_list {
    my ($self, %params) = @_;
    keys %{$self->{$self->_which_table(\%params)}->{attributes}};
}

sub get_all_attributes {
    my ($self, %params) = @_;
    $self->_get_uniq('_get_all_attributes', %params);
}

sub _get_all_attributes {
    my ($self, %params) = @_;
    $self->_get_all('get_attribute_list', %params);
}

sub _get_uniq {
    my ($self, $method, %params) = @_;
    my @attrs = $self->$method(%params);
    my %attrs = map { $_ => undef } @attrs;
    return sort keys %attrs;
}

sub _get_all {
    my ($self, $method, %params) = @_;
    return ((map { $_->_get_all($method, %params) } @{$self->superclasses}), $self->$method(%params));
}

# "spec" here means "whatever annotation went with this attribute when it's declared"
sub find_attribute_spec {
    my ($self, $label, %params) = @_;
    return $self->get_attribute($label, %params) if $self->has_attribute($label, %params);
    foreach my $super (@{$self->superclasses}) {
        my $spec = $super->find_attribute_spec($label, %params);
        return $spec if $spec;
    }
    return undef;
}

# Class Attributes

sub add_class_attribute {
    my ($self, $label, $attribute) = @_;
    (defined $label && defined $attribute)
        || croak "InsufficientArguments : you must provide an attribute and a label";
    (blessed($attribute) && $attribute->isa('Perl6::Class::Attribute'))
        || croak "IncorrectObjectType : Attributes must be a Perl6::Class::Attribute instance got($attribute)";

    if ($attribute->is_public()) {
        unless ($self->has_method($attribute->accessor_name(), for => 'Class')) {
             $self->add_method($attribute->accessor_name() => Perl6::Class::Method->new(
             $self->name(), sub {
                my ($self, $value) = @_;
                $attribute->set_value($value) if defined $value;
                $attribute->get_value();
            }));        
        }
    }          

    $self->{class_data}->{attributes}->{$label} = $attribute;
}

## PRIVATE METHODS

sub _which_table {
    my ($self, $params) = @_;
    my $method_table;
    if (not exists $params->{for} || lc($params->{for}) eq 'instance') {
        return INSTANCE_TABLE;
    }
    elsif (lc($params->{for}) eq 'class') {
        return CLASS_TABLE;
    }
    elsif (lc($params->{for}) eq 'submethod') {
        # XXX - this is flat out wrong, but we 
        # will leave it for now, until we have
        # proper sub-methods
        return INSTANCE_TABLE; 
    }    
    else {
        croak "Incorrect Parameter : methods cannot be found for " . $params->{for};
    }
}

1;

__END__

=pod

=head1 NAME 

Perl6::MetaClass - Metaclass in the Perl6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new>

=item B<name>

=item B<superclasses>

=item B<class_precedence_list>

=item B<add_method>

=item B<get_method>

=item B<sub has_method>

=item B<find_method>

=item B<responds_to>

=item B<add_attribute>

=item B<get_attribute>

=item B<has_attribute>

=item B<get_attribute_list>

=item B<get_all_attributes>

=item B<find_attribute_spec>

=back

=head1 AUTHOR

Stevan Little

=cut
