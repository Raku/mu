
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
                ($name . '::Class')->add_attribute(
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
