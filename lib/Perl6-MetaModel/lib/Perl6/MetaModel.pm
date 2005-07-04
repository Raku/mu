
package Perl6::MetaModel;

use strict;
use warnings;

use Perl6::Object;
use Perl6::Role;

use Perl6::SubMethod;

use Perl6::Class::Attribute;
use Perl6::Class::Method;

use Perl6::Instance::Attribute;
use Perl6::Instance::Method;

use Carp 'croak';

sub import {
    no strict 'refs';
    *{caller() . '::class'}   = \&class;
    *{caller() . '::role'}    = \&role;
}

sub role {
    my ($name, $role) = @_;
    Perl6::Role::add_role($name, $role);
}

sub class {
    my ($name, $params) = @_;
    
    my %allowed = map { $_ => undef } qw(extends instance class does);
    my %allowed_in = map { $_ => undef } qw(attrs BUILD methods);
    
    foreach my $key (keys %{$params}) {
        croak "Invalid key ($key) in params" 
            unless exists $allowed{$key};
        if ($key eq 'class' || $key eq 'kind') {
            foreach my $sub_key (keys %{$params->{$key}}) {
                croak "Invalid sub_key ($sub_key) in key ($key) in params" 
                    unless exists$allowed_in{$sub_key};                
            }
        }
    }
    
    
    my $extends = $params->{extends} || [ 'Perl6::Object' ];
    my $code = qq|
package $name;
\@$name\:\:ISA = 'Perl6::Object';
|;
    eval $code;
     
    ($name)->meta->superclasses([ map { $_->meta } @{$extends} ]);        
    
    if (exists $params->{instance}) {
        my $instance = $params->{instance};
        
        if (exists $instance->{BUILD}) {
            ($name)->meta->add_method('BUILD' => Perl6::SubMethod->new($name => $instance->{BUILD}));            
        }
        if (exists $instance->{methods}) {
            ($name)->meta->add_method($_ => Perl6::Instance::Method->new($name, $instance->{methods}->{$_})) 
                foreach keys %{$instance->{methods}};
        }
        if (exists $instance->{attrs}) {
            foreach my $attr (@{$instance->{attrs}}) {
                my $type;
                if (ref($attr) eq 'ARRAY') {
                    ($type, $attr) = @{$attr}; 
                }
                ($name)->meta->add_attribute(
                    $attr => Perl6::Instance::Attribute->new($name => $attr, $type)
                );              
            }
        }        
    }
    if (exists $params->{class}) {
        my $class = $params->{class};
        if (exists $class->{attrs}) {
            foreach my $attr (@{$class->{attrs}}) {
                my $type;
                if (ref($attr) eq 'ARRAY') {
                    ($type, $attr) = @{$attr}; 
                }
                ($name)->meta->add_class_attribute(
                    $attr => Perl6::Class::Attribute->new($name => $attr, $type)
                );              
            }            
            
        }
        if (exists $class->{methods}) {
            foreach my $label (keys %{$class->{methods}}) {
                ($name)->meta->add_class_method(
                    $label => Perl6::Class::Method->new($name, $class->{methods}->{$label})
                );
            }
        }
    }
    if ($params->{does}) {
        Perl6::Role->flatten_roles_into(($name), @{$params->{does}});
    }
}

1;

__END__

=pod

=head1 NAME

Perl6::MetaModel - Perl5 Prototype of the Perl6 Metaclass model

=head1 SYNOPSIS

    use Perl6::MetaModel;

    role MyRole => {
        methods => {
            test => sub { print 'MyRole::test' }
        }
    };

    class MyClass => {
        extends => [ 'MyBaseClass' ],
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
    };

    my $c = MyClass->new();

    $c->foo('Testing 1 2 3');

    $c->tester();

=head1 DESCRIPTION

This set of modules is a prototype for the Perl6 Metaclass model, which is the model which 
descibes the interactions of classes, objects and roles in the Perl6 object space. 

I am prototyping this in Perl5 as part of the Perl6 -> PIL compiler to run on a Perl5 VM. 
It is currently in the early stages of a refactoring from the original which was just a 
hacked together prototype. 

  +------------------+
  | Perl6::MetaClass |
  +------------------+       +------------------+
  | name             |------>| Perl6::Attribute |....{ subclasses:
  | attrs            |---+   +------------------+       - Perl6::Class::Attribute
  | methods          |   |   | associated_with  |       - Perl6::Instance::Attribute
  | superclasses     |   |   | accessor_name    |       - Perl6::Role::Attribute
  +------------------+   |   | visibility       |     }
                         |   | type             |
                         |   | label            |
                         |   +------------------+
                         |
                         |   +------------------+
                         +-->| Perl6::Method    |....{ subclasses:
                             +------------------+       - Perl6::Class::Method
                             | associated_with  |       - Perl6::Instance::Method
                             | code             |       - Perl6::Role::Method
                             +------------------+     }

=head1 SEE ALSO

=over 4

=item All the Perl6 documentation. 

In particular the Apocolypse and Synopsis 12 which describes the object system.

=item L<Class::Role>, L<Class::Roles> & L<Class::Trait>

The first two are early attempts to prototype role behavior, and the last is an implementation
of the Trait system based on the paper which originally inspired Roles.

=item Any good Smalltalk book.

I prefer the Brown book by Adele Goldberg and David Robinson, but any one which talks about the
smalltalk metaclasses is a good reference.

=item CLOS

The Common Lisp Object System has a very nice meta-model, and plently of reference on it. In 
particular there is a small implementation of CLOS called TinyCLOS which is very readable (if 
you know enough Scheme that is)

=back

=head1 AUTHOR

Stevan Little stevan@iinteractive.com

=cut


