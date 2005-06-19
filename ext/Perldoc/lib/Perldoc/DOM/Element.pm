
class Perldoc::DOM::Element is Perldoc::DOM::Node;

has $.name;

has %.attr;

method dom_attr($self:) returns Hash {
    my %att = $self.SUPER::dom_attr() or die;

    # yeah, we just clobber them, so what :)
    %att = (%att, %.attr);

    return %att;
}

=head1 NAME

Perldoc::DOM::Element - element in a Perldoc::DOM tree

=head1 SYNOPSIS

See L<Perldoc::DOM::Node>.

=head1 DESCRIPTION

An `element'.

All meta-information provided by dialects should be stored in this
tree as an attribute.  The property C<attr> is used for this.

=cut

