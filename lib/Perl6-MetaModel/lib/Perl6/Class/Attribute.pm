
package Perl6::Class::Attribute;

use strict;
use warnings;

use base 'Perl6::Attribute';

sub new {
    my ($class, $associated_with, $label, $type) = @_;
    my $self = $class->SUPER::new($associated_with, $label, $type);
    $self->{value} = ($self->is_array ? [] : ($self->is_hash ? {} : undef));
    return $self;
}

sub get_value {
    my $self = shift;
    $self->{value};
}

sub set_value { 
    my ($self, $value) = @_;
    $self->{value} = $value
}

1;

__END__