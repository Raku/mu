
package Perl6::Class::Attribute;

use strict;
use warnings;

use base 'Perl6::Attribute';

sub new {
    my ($class, $associated_with, $label, $type) = @_;
    my $self = $class->SUPER::new($associated_with, $label, $type);
    $self->{value} = $self->instantiate_container;

    return $self;
}

sub get_value { shift->{value} }

sub set_value { 
    my ($self, $value) = @_;
    $self->{value} = $value
}

1;

__END__
