
package Perl6::MetaModel;

use strict;
use warnings;

use Perl6::Object;
use Perl6::Role;

use Perl6::Class::Attribute;
use Perl6::Class::Method;

use Perl6::Instance::Attribute;
use Perl6::Instance::Method;

use Perl6::Role::Attribute;
use Perl6::Role::Method;

sub import {
    no strict 'refs';
    *{caller() . '::class'}   = \&class;
    *{caller() . '::role'}    = \&role;
    *{caller() . '::CALLER'}  = \&CALLER;    
}

sub CALLER {
    my $depth = shift;
    $depth ||= 0;
    no strict 'refs';
    return unless $Perl6::Object::CALL_STACK[$depth];
    return @{$Perl6::Object::CALL_STACK[$depth]};
}

sub role {
    my ($name, $role) = @_;
    Perl6::Role::add_role($name, $role);
}

sub class {
    my ($name, $params) = @_;
    
    my %allowed = map { $_ => undef } qw(extends kind_of class kind does);
    my %allowed_in = map { $_ => undef } qw(attrs init methods);
    
    foreach my $key (keys %{$params}) {
        die "Invalid key ($key) in params" 
            unless exists $allowed{$key};
        if ($key eq 'class' || $key eq 'kind') {
            foreach my $sub_key (keys %{$params->{$key}}) {
                die "Invalid sub_key ($sub_key) in key ($key) in params" 
                    unless exists$allowed_in{$sub_key};                
            }
        }
    }
    
    
    my $extends = $params->{extends} || 'Perl6::Object';
    my $kind    = $params->{kind_of} || 'Perl6::Object';
    my $code = qq|
package $name;
\@$name\:\:ISA = 'Perl6::Object';

package $name\:\:Class;
\@$name\:\:Class\:\:ISA = '$extends\:\:Class';

package $name\:\:Kind;
\@$name\:\:Kind\:\:ISA = '$kind\:\:Kind';
our \%ATTRS;
our \%METHODS;
|;
    eval $code;
 
    ($name . '::Class')->metaclass->superclasses([ ($extends . '::Class')->metaclass ]);    
    
    no strict 'refs';
    if (exists $params->{class}) {
        my $class = $params->{class};
        
        my $kind = "$name\:\:Kind";
        if (exists $class->{init}) {
            ($name . '::Class')->metaclass->add_method('init' => Perl6::Instance::Method->new($name => $class->{init}));
        }
        if (exists $class->{methods}) {
            ($name . '::Class')->metaclass->add_method($_ => Perl6::Instance::Method->new($name, $class->{methods}->{$_})) 
                foreach keys %{$class->{methods}};
        }
        if (exists $class->{attrs}) {
            foreach my $attr (@{$class->{attrs}}) {
                my $type;
                if (ref($attr) eq 'ARRAY') {
                    ($type, $attr) = @{$attr}; 
                }
                ($name . '::Class')->metaclass->add_attribute(
                    $attr => Perl6::Instance::Attribute->new($name => $attr, $type)
                );              
            }
        }        
    }
    if (exists $params->{kind}) {
        my $kind = $params->{kind};
        if (exists $kind->{attrs}) {
            %{"${name}::Kind::ATTRS"} = map { $_ => Perl6::Class::Attribute->new($name => $_) } @{$kind->{attrs}};
        }
        if (exists $kind->{init}) {
            *{"${name}::Kind::init"} = $kind->{init};
        }
        if (exists $kind->{methods}) {
            foreach my $label (keys %{$kind->{methods}}) {
                *{"${name}::Kind::${label}"} = $kind->{methods}->{$label};
            }
        }
    }
    if ($params->{does}) {
        Perl6::Role::flatten_roles_into(($name . '::Class'), @{$params->{does}});
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
            attrs => [ '$.foo' ],
            methods => {
                tester => sub {
                    my ($self) = shift;
                    $self->test(); # call the role method
                    print $self->get_value('$.foo');
                }
            }
        }
    };

    my $c = MyClass->new_instance();

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


