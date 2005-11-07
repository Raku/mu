
package Perl6::MM::Opaque;

use Perl6::Core::Num;
use Perl6::Core::Hash;
use Perl6::Core::Str;
use Perl6::Core::Ref;

use Perl6::MM::Attribute;

package opaque;

use strict;
use warnings;

use Carp 'confess';
use Scalar::Util 'blessed';

use base 'type::comparable';

my $OBJECT_ID = num->new(0);

# create a null object to 
# be used in bootstrapping
our $NULL_OBJECT = bless [ 
    $OBJECT_ID, 
    reference->new(nil->new), 
    hash->new()
    ] => __PACKAGE__;

sub new {
    my ($class, $klass, $attrs) = @_;
    ((blessed($klass) && $klass->isa('reference')) &&
     ($klass->fetch->isa('opaque')))
        || confess "Class must be an instance of reference";
    (blessed($attrs) && $attrs->isa('hash'))
        || confess "Attrs must be an instance of hash";        
    bless [ 
        ($OBJECT_ID = $OBJECT_ID->increment), 
        $klass, 
        $attrs 
    ] => $class;
}

# we dont really have a native type
sub to_native { shift }

# conversion to other native types
sub to_num { (shift)->[0]           } # return the id
sub to_str { str->new((shift) . '') } # get a str like ref
sub to_bit { bit->new(1)            } # we are always true

# comparitor

sub _compare {
    my ($left, $right) = @_;
    (blessed($right) && $right->isa('opaque'))
        || confess "You can only compare opaque types with other opaque types (not $right)";
    return $left->id->to_native <=> $right->id->to_native;
}

## methods

sub id    { (shift)->[0]        }
sub class { (shift)->[1]->fetch }

sub change_class {
    my ($self, $klass) = @_;
    return $self->[1]->store($klass);
}

sub get_attr {
    my ($self, $name) = @_;
    (blessed($name) && $name->isa('symbol'))
        || confess "Attribute names must be symbols";
    ($self->[2]->exists($name) == $bit::TRUE)
        || confess "Attribute (" . $name->to_str->to_native . ") does not exist";
    return $self->[2]->fetch($name);
}

sub set_attr {
    my ($self, $name, $value) = @_;
    (blessed($name) && $name->isa('symbol'))
        || confess "Attribute names must be symbols";    
    ($self->[2]->exists($name) == $bit::TRUE)
        || confess "Attribute (" . $name->to_str->to_native . ") does not exist";        
    return $self->[2]->store($name, $value);
}

sub add_attr {
    my ($self, $name) = @_;
    (blessed($name) && $name->isa('symbol'))
        || confess "Attribute names must be symbols";
    ($self->[2]->exists($name) == $bit::FALSE)
        || confess "Attribute (" . $name->to_str->to_native . ") already exists";
    return $self->[2]->store($name, $nil::NIL);    
}

1;

__END__

=pod

=head1 NAME

opaque - the core opaque instance type

=cut
