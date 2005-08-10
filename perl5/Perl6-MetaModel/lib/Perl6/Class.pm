
package Perl6::Class;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'Perl6::Instance'; # << this is where the Perl 5 sugar is now ...

use Perl6::MetaClass;
use Perl6::Role;

use Perl6::Method;

use Perl6::Class::Attribute;
use Perl6::Instance::Attribute;

## Private methods

our %ALL_CLASSES;

sub new_class {
    my ($class, $identifier, $params) = @_;
    # XXX -
    # this should actually probably goto $AUTOLOAD ...
    confess "Cannot call new_class with a blessed instance ($class)" if blessed($class);
    my ($name, $version, $authority) = _extract_name_from_identifier($identifier);      
    my $self = ::create_P6opaque($class => ( 
        name       => $name,
        identifier => $identifier,
        meta       => undef
    ));
    _validate_params($params) if $params;
    ## BOOTSTRAPPING
    # since Perl6::MetaClass calls this
    # method to create it's own Perl6::Class
    # then we need to avoid trying to build
    # the metaclass for it 
    if ($name ne 'Perl6::MetaClass') {
        my $meta = _create_metaclass($name, $version, $authority);     
        _build_metaclass($self, $meta, $params);         
        ::get_P6opaque_instance_data($self)->{meta} = $meta;
    }
    # stash these into the class ...
    $ALL_CLASSES{$identifier} = $self;
    # also stash the short name if 
    # it is different from the identifier
    $ALL_CLASSES{$name} = $self if $name ne $identifier; 
    return $self;
}

sub _apply_class_to_environment {
    my ($self) = @_;    
    my ($name) = ::get_P6opaque_instance_data($self)->{name};          
    # create the package ...
    my $code = qq|
        package $name;
        \@$name\:\:ISA = 'Perl6::Instance';
        1;
    |;    
    eval $code || confess "Could not initialize class '$name'";                
    # alias the full name ...
    eval {  
        no strict 'refs';         
        *{::get_P6opaque_instance_data($self)->{identifier} . '::'} = *{$name . '::'};
    };
    confess "Could not create full name : $@" if $@;   
}

sub _validate_params {
    my ($params) = @_;

    my %allowed = map { $_ => undef } qw(is does instance class);
    my %allowed_in = map { $_ => undef } qw(attrs BUILD DESTROY methods submethods);

    foreach my $key (keys %{$params}) {
        confess "Invalid key ($key) in params" 
            unless exists $allowed{$key};
        if ($key eq 'class' || $key eq 'instance') {
            foreach my $sub_key (keys %{$params->{$key}}) {
                confess "Invalid sub_key ($sub_key) in key ($key) in params" 
                    unless exists $allowed_in{$sub_key};                
            }
        }
    }
}

sub _extract_name_from_identifier {
    my ($identifier) = @_;
    # shortcut for classes with no extra meta-info
    return ($identifier, undef, undef) if $identifier !~ /\-/;
    # XXX - this will actually need work, 
    # but it is sufficient for now.
    return split '-' => $identifier;
}

sub _create_metaclass {
    my ($name, $version, $authority) = @_;

    # create the metaclass ...     
    my $meta; 
    eval {
        if ($name eq 'Perl6::Object') {
            ## BOOTSTRAPPING
            # XXX - Perl6::Object cannot call the 
            # regular new() becuase Perl6::MetaClass
            # actually inherits new() from Perl6::Object
            # meta-circulatiry rules :P
            $meta = Perl6::MetaClass::new(
                '$.name' => $name,
                (defined $version   ? ('$.version'   => $version)   : ()),
                (defined $authority ? ('$.authority' => $authority) : ())                              
            );  
        }
        else {
            $meta = ::dispatch('Perl6::MetaClass', 'new', (
                '$.name' => $name,
                (defined $version   ? ('$.version'   => $version)   : ()),
                (defined $authority ? ('$.authority' => $authority) : ())                              
            ));
        }  
    };
    confess "Could not initialize the metaclass for $name : $@" if $@;     
    
    return $meta;
}

sub _build_metaclass {
    my ($self, $meta, $params) = @_;

    my $superclasses = $params->{is};
    ::dispatch($meta, 'superclasses', ([ map { ::meta($_) } @{$superclasses} ]));        

    if (my $instance = $params->{instance}) {

        ::dispatch($meta, 'add_method', ('BUILD' => Perl6::Method->create_submethod($self => $instance->{BUILD})))
            if exists $instance->{BUILD};            
        ::dispatch($meta, 'add_method', ('DESTROY' => Perl6::Method->create_submethod($self => $instance->{DESTROY})))
            if exists $instance->{DESTROY};
            
        if (exists $instance->{methods}) {
            foreach (keys %{$instance->{methods}}) {
                if (/^_/) {
                    ::dispatch($meta, 'add_method', ($_ => Perl6::Method->create_private_method($self, $instance->{methods}->{$_})));
                }
                else {
                    ::dispatch($meta, 'add_method', ($_ => Perl6::Method->create_instance_method($self, $instance->{methods}->{$_})));
                }
            }
        }
        if (exists $instance->{submethods}) {
            ::dispatch($meta, 'add_method', ($_ => Perl6::Method->create_submethod($self, $instance->{submethods}->{$_})))
                foreach keys %{$instance->{submethods}};
        }        
        if (exists $instance->{attrs}) {
            foreach my $attr (@{$instance->{attrs}}) {
                my $props;
                if (ref($attr) eq 'ARRAY') {
                    ($attr, $props) = @{$attr}; 
                }
                ::dispatch($meta, 'add_attribute', (
                    $attr => Perl6::Instance::Attribute->new($self => $attr, $props)
                ));              
            }
        }        
    }
    if (my $class = $params->{class}) {  
        
        if (exists $class->{attrs}) {
            foreach my $attr (@{$class->{attrs}}) {
                my $props;
                if (ref($attr) eq 'ARRAY') {
                    ($attr, $props) = @{$attr}; 
                }
                ::dispatch($meta, 'add_attribute', (
                    $attr => Perl6::Class::Attribute->new($self => $attr, $props)
                ));
            }            

        }
        if (exists $class->{methods}) {
            foreach my $label (keys %{$class->{methods}}) {
                if ($label =~ /^_/) {
                    ::dispatch($meta, 'add_method', ($label => Perl6::Method->create_private_method($self, $class->{methods}->{$label})));
                }
                else {
                    ::dispatch($meta, 'add_method', ($label => Perl6::Method->create_class_method($self, $class->{methods}->{$label})));
                }
            }
        }
    }


    Perl6::Role->flatten_roles_into($meta, @{$params->{does}})
        if $params->{does};
}


1;

__END__

=pod

=head1 NAME

Perl6::Class 

=head1 SYNOPSIS

    my $foo_class = Perl6::Class->new('Foo' => {
            is => [ 'MyBaseClass' ],
            does => [ 'MyRole' ],
            class => {
                methods => {
                    a_class_method => sub { ... }
                }
            },
            instance => {
                attrs => [ '$.foo' ],
                BUILD => sub {
                    my ($self) = @_;
                    $self->set_value('$.foo' => 'Foo::Bar');
                },
                methods => {
                    tester => sub {
                        my ($self) = shift;
                        $self->test(); # call the role method
                        print $self->get_value('$.foo');
                    }
                }
            }
        }
    );
    $foo_class->apply(); # this injects the Class into the Object environment

=head1 DESCRIPTION

=head1 METHODS

=over 4

=item B<new ($name, \%params)>

=item B<name>

=item B<apply>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
