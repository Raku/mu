
package Perl6::Util::MethodTable;

use strict;
use warnings;

use Scalar::Util 'blessed';

sub new {
    my ($class) = @_;
    bless { methods => {} }, $class;
}

sub get_method {
    my ($self, $label) = @_;
    (defined $label)
    	|| die "Insufficient Arguments : you must supply a label to get";
    return undef unless exists $self->{methods}->{$label};
    return $self->{methods}->{$label};
}

sub has_method {
    my ($self, $label) = @_;
    $self->get_method($label) ? 1 : 0
}

sub add_method {
    my ($self, $label, $method) = @_;
    (defined $label && defined $method)
    	|| die "InsufficientArguments : you must provide a method and a label";
    (blessed($method) && $method->isa('Perl6::Method'))
    	|| die "IncorrectObjectType : Method must be a Perl6::Method object got($method)";
    $self->{methods}->{$label} = $method;
}

1;
