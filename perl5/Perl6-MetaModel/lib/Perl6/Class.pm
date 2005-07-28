
package Perl6::Class;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

our %CLASSES;

sub meta {
    my ($class) = @_;
    return $CLASSES{ref($class) || $class}->{meta};  
}

sub isa {
    our $AUTOLOAD = 'isa';
    goto &AUTOLOAD;
}

sub can {
    our $AUTOLOAD = 'can';
    goto &AUTOLOAD;
}

sub AUTOLOAD {
    my @AUTOLOAD = split '::', our $AUTOLOAD;
    my $label = $AUTOLOAD[-1];
    my $self = shift;    
    # NOTE:
    # DESTROYALL is what should really be called
    # so we just deal with it like this :)
    if ($label =~ /DESTROY/) {
        # XXX - hack to avoid destorying Perl6::Class object
        # as that presents some issues for some reason
        return unless blessed($self) ne 'Perl6::Class';
        $label = 'DESTROYALL';        
    }
    my @return_value;

    # check if this is a private method
    if ($label =~ /^_/) {
        ($self->meta->has_method($label, (for => 'private')))
            || confess "Private Method ($label) not found for instance ($self)";        
        my $method = $self->meta->get_method($label, (for => 'private'));
        @return_value = $method->call($self, @_);             
    }
    else {
        # get the dispatcher instance ....
        my $dispatcher = $self->meta->dispatcher(':canonical');

        # just discard it if we are calling SUPER
        $dispatcher->next() if ($AUTOLOAD[0] eq 'SUPER');

        # this needs to be fully qualified for now
        my $method = ::WALKMETH($dispatcher, $label, (blessed($self) ? () : (for => 'Class')));
        (blessed($method) && $method->isa('Perl6::Method'))
            || confess "Method ($label) not found for instance ($self)";        

        push @Perl6::MetaModel::CURRENT_DISPATCHER => [ $dispatcher, $label, $self, @_ ];

        @return_value = $method->call($self, @_);     

        # we can dispose of this value, as it 
        # should never be called outside of 
        # a method invocation
        pop @Perl6::MetaModel::CURRENT_DISPATCHER;
    }
    # return our values
    return wantarray ?
                @return_value
                :
                $return_value[0];
}


package Perl6::Class::Util;

use Carp 'confess';
use Scalar::Util 'blessed';

use Perl6::MetaClass;
use Perl6::Role;

use Perl6::SubMethod;

use Perl6::Class::Attribute;
use Perl6::Class::Method;

use Perl6::Instance::Attribute;
use Perl6::Instance::Method;

## Private methods

sub _create_new_class {
    my ($class, $name, $params) = @_;
    my $self = bless { 
        name   => $name,
        meta   => undef,
        params => {}
    }, $class;
    Perl6::Class::Util::_validate_params($self, $params);
    die "Duplicate class name ($name)" if exists $CLASSES{$name};
    $CLASSES{$name} = $self;
    return $self;
}

sub _apply_class_to_environment {
    my ($self) = @_;
    my ($name, $version, $authority) = Perl6::Class::Util::_get_class_meta_information($self);
    my $code = qq|
        package $name;
        \@$name\:\:ISA = 'Perl6::Class';
    |;
    eval $code || confess "Could not initialize class '$name'";   
    my $meta; 
    eval {
        $meta = Perl6::MetaClass->new(
            name => $name,
            (defined $version   ? (version   => $version)   : ()),
            (defined $authority ? (authority => $authority) : ())                              
        );
    };
    confess "Could not initialize the metaclass for $name : $@" if $@;
    eval {
        no strict 'refs';              
        $self->{meta} = $meta;
        $CLASSES{$name} = $self; # store short name too               
        *{$self->{name} . '::'} = *{$name . '::'};
    };
    confess "Could not create full name " . $self->name . " : $@" if $@;    
    Perl6::Class::Util::_build_class($self, $meta);    
}

sub _validate_params {
    my ($self, $params) = @_;

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

    $self->{params} = $params;
}

sub _get_class_meta_information {
    my ($self) = @_;
    my $identifier = $self->{name};
    # shortcut for classes with no extra meta-info
    return ($identifier, undef, undef) if $identifier !~ /\-/;
    # XXX - this will actually need work, 
    # but it is sufficient for now.
    return split '-' => $identifier;
}

sub _build_class {
    my ($self, $meta) = @_;

    my $name = $meta->name;

    my $superclasses = $self->{params}->{is};
    $meta->superclasses([ map { $_->meta } @{$superclasses} ]);        

    if (my $instance = $self->{params}->{instance}) {

        $meta->add_method('BUILD' => Perl6::SubMethod->new($name => $instance->{BUILD}))
            if exists $instance->{BUILD};            
        $meta->add_method('DESTROY' => Perl6::SubMethod->new($name => $instance->{DESTROY}))          
            if exists $instance->{DESTROY};
            
        if (exists $instance->{methods}) {
            foreach (keys %{$instance->{methods}}) {
                if (/^_/) {
                    $meta->add_method($_ => Perl6::PrivateMethod->new($name, $instance->{methods}->{$_}));
                }
                else {
                    $meta->add_method($_ => Perl6::Instance::Method->new($name, $instance->{methods}->{$_}));                
                }
            }
        }
        if (exists $instance->{submethods}) {
            $meta->add_method($_ => Perl6::SubMethod->new($name, $instance->{submethods}->{$_})) 
                foreach keys %{$instance->{submethods}};
        }        
        if (exists $instance->{attrs}) {
            foreach my $attr (@{$instance->{attrs}}) {
                my $props;
                if (ref($attr) eq 'ARRAY') {
                    ($attr, $props) = @{$attr}; 
                }
                $meta->add_attribute(
                    $attr => Perl6::Instance::Attribute->new($name => $attr, $props)
                );              
            }
        }        
    }
    if (my $class = $self->{params}->{class}) {  
        
        if (exists $class->{attrs}) {
            foreach my $attr (@{$class->{attrs}}) {
                my $props;
                if (ref($attr) eq 'ARRAY') {
                    ($attr, $props) = @{$attr}; 
                }
                $meta->add_attribute(
                    $attr => Perl6::Class::Attribute->new($name => $attr, $props)
                );              
            }            

        }
        if (exists $class->{methods}) {
            foreach my $label (keys %{$class->{methods}}) {
                if ($label =~ /^_/) {
                    $meta->add_method($label => Perl6::PrivateMethod->new($name, $class->{methods}->{$label}));
                }
                else {
                    $meta->add_method($label => Perl6::Class::Method->new($name, $class->{methods}->{$label}));                
                }
            }
        }
    }


    Perl6::Role->flatten_roles_into($meta, @{$self->{params}->{does}})
        if $self->{params}->{does};
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
