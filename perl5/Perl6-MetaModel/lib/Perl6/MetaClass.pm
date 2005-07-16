
package Perl6::MetaClass;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

use Perl6::MetaClass::Dispatcher;

use constant INSTANCE_TABLE  => 'class_definition';
use constant CLASS_TABLE     => 'class_data';

sub new {
    my ($class, %params) = @_;
    my $meta = bless {
        # meta-information
        name         => undef,
        version      => '0.0.0',
        authority    => undef,
        # the guts of the metaclass
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
    $meta->name($params{name})                 if exists $params{name};
    $meta->version($params{version})           if exists $params{version};
    $meta->authority($params{authority})       if exists $params{authority};    
    $meta->superclasses($params{superclasses}) if exists $params{superclasses};        
    return $meta;
}

# meta-information methods

sub name {
    my ($self, $name) = @_;
    $self->{name} = $name if defined $name;
    $self->{name};
}

sub version {
    my ($self, $version) = @_;
    if (defined $version) {
        ($version =~ /^\d+\.\d+\.\d+$/)
            || confess "The version ($version) is not in the correct format '0.0.0'";
        $self->{version} = $version;
    }
    $self->{version};    
}

sub authority {
    my ($self, $authority) = @_;
    $self->{authority} = $authority if defined $authority;
    $self->{authority};
}

sub identifier {
    my ($self) = @_;
    return join '-' => ($self->{name}, $self->{version}, ($self->{authority} || ()));
}

# .... 

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
            || confess "BadType : You must pass the superclasses as an ARRAY ref";
        (blessed($_) && $_->isa('Perl6::MetaClass'))
            || confess "IncorrectObjectType : A superclass must be a Perl6::MetaClass instance"
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

## METHODS

# Instance Methods

sub add_method {
    my ($self, $label, $method) = @_;
    (defined $label && defined $method)
        || confess "InsufficientArguments : you must provide a method and a label";
    (blessed($method) && $method->isa('Perl6::Method'))
        || confess "IncorrectObjectType : Method must be a Perl6::Method object got($method)";
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
        confess "Incorrect Object Type : I dont know what to do with ($method)";
    }    
    $self->{$method_table}->{methods}->{$label} = $method;
}

sub get_method {
    my ($self, $label, %params) = @_;
    (defined $label)
        || confess "InsufficientArguments : you must provide a label";
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
    my ($self, $label, %params) = @_;
    $self->find_method($label, %params) ? 1 : 0;    
}

## ATTRIBUTES

# Instance Attributes

sub add_attribute {
    my ($self, $label, $attribute) = @_;
    (defined $label && defined $attribute)
        || confess "InsufficientArguments : you must provide an attribute and a label";
    (blessed($attribute) && $attribute->isa('Perl6::Attribute'))
        || confess "IncorrectObjectType : Attributes must be a Perl6::Attribute instance got($attribute)";
    $self->_create_accessor($attribute);         
    
    my $method_table;
    if ($attribute->isa('Perl6::Instance::Attribute')) {
        $method_table = INSTANCE_TABLE;
    }
    elsif ($attribute->isa('Perl6::Class::Attribute')) {
        $method_table = CLASS_TABLE;
    }

    $self->{$method_table}->{attributes}->{$label} = $attribute;
}

sub get_attribute {
    my ($self, $label, %params) = @_;
    (defined $label)
        || confess "InsufficientArguments : you must provide a label";
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

## PRIVATE METHODS

sub _create_accessor {
    my ($self, $attribute) = @_;
    # no accessors if it's not public ...
    return unless $attribute->is_public();
    # do not overwrite already defined methods ...
    return if $self->has_method($attribute->accessor_name());
    
    # otherwise ...
    my $label = $attribute->label();
    my ($method_type, $method_code);
    if ($attribute->isa('Perl6::Instance::Attribute')) {
        $method_type = 'Perl6::Instance::Method';
        $method_code = sub {
            my ($i, $value) = @_;
            $i->set_value($label => $value) if defined $value;
            $i->get_value($label);
        } if $attribute->is_rw;
        $method_code = sub {
            my $i = shift;
            (@_) && confess "the attribute '$label' is read-only";
            $i->get_value($label);
        } if $attribute->is_ro;        
    }
    elsif ($attribute->isa('Perl6::Class::Attribute')) {
        $method_type = 'Perl6::Class::Method';
        $method_code = sub {
            my (undef, $value) = @_;
            $attribute->set_value($label => $value) if defined $value;
            $attribute->get_value($label);
        } if $attribute->is_rw;
        $method_code = sub {
            (@_) && confess "the attribute '$label' is read-only";            
            $attribute->get_value($label);
        } if $attribute->is_ro;                
    }
    else {
        confess "Incorrect Object Type : I do not understand the attribute class ($attribute)";
    }
    
    $self->add_method(
        $attribute->accessor_name(),
        $method_type->new($self->name(), $method_code)
    ); 
}

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
        confess "Incorrect Parameter : methods cannot be found for " . $params->{for};
    }
}

sub dispatcher {
    my ($self, $order) = @_;
    Perl6::MetaClass::Dispatcher->new($self, $order);
}

1;

__END__

=pod

=head1 NAME 

Perl6::MetaClass - Metaclass in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new>

=item B<name>

=item B<superclasses>

=item B<class_precedence_list>

=item B<add_method>

=item B<get_method>

=item B<has_method>

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

Stevan Little E<lt>stevan@iinteractive.comE<gt>

Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>

=cut
