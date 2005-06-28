
package MetaModel;

use strict;
use warnings;

use Object;
use Role;
use Perl6::Attribute;

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
    return unless $Object::CALL_STACK[$depth];
    return @{$Object::CALL_STACK[$depth]};
}

sub role {
    my ($name, $role) = @_;
    Role::add_role($name, $role);
}

sub class {
    my ($name, $params) = @_;
    my $extends = $params->{extends} || 'Object';
    my $kind    = $params->{kind_of} || 'Object';
    my $code = qq|
package $name;
\@$name\:\:ISA = 'Object';

package $name\:\:Class;
\@$name\:\:Class\:\:ISA = '$extends\:\:Class';

package $name\:\:Kind;
\@$name\:\:Kind\:\:ISA = '$kind\:\:Kind';
our \%ATTRS;
our \%METHODS;
|;
    eval $code;

    #warn "|||||> adding superclasses to " . ($name . '::Class') . " [" . ($extends . '::Class') . "]";      
    ($name . '::Class')->metaclass->superclasses([ ($extends . '::Class')->metaclass ]);    
    
    no strict 'refs';
    if (exists $params->{class}) {
        my $class = $params->{class};
        
        my $kind = "$name\:\:Kind";
        if (exists $class->{init}) {
            ($name . '::Class')->metaclass->add_method('init' => $class->{init});
        }
        if (exists $class->{methods}) {
            ($name . '::Class')->metaclass->add_method($_ => $class->{methods}->{$_}) 
                foreach keys %{$class->{methods}};
        }
        if (exists $class->{attrs}) {
            foreach my $attr (@{$class->{attrs}}) {
                my $type;
                if (ref($attr) eq 'ARRAY') {
                    ($type, $attr) = @{$attr}; 
                }
                ($name . '::Class')->add_attribute(
                    $attr => Perl6::Attribute->new($name => $attr, $type)
                );              
            }
        }        
    }
    if (exists $params->{kind}) {
        my $kind = $params->{kind};
        if (exists $kind->{attrs}) {
            %{"${name}::Kind::ATTRS"} = map { $_ => Perl6::Attribute->new($name => $_) } @{$kind->{attrs}};
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
#        my $kind = "$name\:\:Kind";    
        Role::flatten_roles_into(($name . '::Class'), @{$params->{does}});
    }
}

1;

__END__

=pod

=head1 NAME

MCP MetaModel

=head1 SYNOPSIS

    class Foo => {
        kind_of => 'Bar',
        extends => 'Baz',
        class   => {
            attrs   => [ 'name' ],
            init    => sub {
                (shift)->{name} = "default name";
            },
            methods => {
                name => sub { (shift)->{name} }
            }
        },
        kind => {
            attrs => [ 'bang', 'boom', 'bash' ],
            init => sub {
                my ($self) = @_;
                $self->{bang} = [];
                $self->{boom} = {};
                $self->{bash} = '~/home';
            },
            methods => {
                make => sub {
                    my ($class, $name) = @_;
                    return $class->new_instance('Foo' => (name => $name));
                }
            }
        }
    };

=cut
