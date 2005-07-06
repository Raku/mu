
package Perl6::MetaModel;

use strict;
use warnings;

use Perl6::Role;
use Perl6::Class;

use Carp 'croak';

sub import {
    no strict 'refs';
    *{caller() . '::class'}   = \&class;
    *{caller() . '::role'}    = \&role;
}

sub role {
    my ($name, $role) = @_;
    Perl6::Role->add_role($name, $role);
}

sub class {
    my ($name, $params) = @_;
    my $class = Perl6::Class->new($name, $params);
    $class->apply();
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


