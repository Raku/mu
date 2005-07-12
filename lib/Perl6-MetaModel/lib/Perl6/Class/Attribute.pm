
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

sub get_value { ${shift->{value}} }

sub set_value { 
    my ($self, $value) = @_;
    ${$self->{value}} = $value
}

1;

__END__

=pod

=head1 NAME

Perl6::Class::Attribute - Class Attributes in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 SUPERCLASS

=over 4

=item I<Perl6::Attribute>

=back

=head1 METHODS

=over 4

=item B<new ($associated_with, $label, ?$type)>

=item B<get_value>

=item B<set_value ($value)>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
