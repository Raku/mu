package Perldoc::DOM::PI;
use Perldoc::DOM::Node -Base;

=head1 NAME

Perldoc::DOM::PI - a processing instruction in a Perldoc::DOM tree

=head1 SYNOPSIS

See L<Perldoc::DOM::Node>.

=head1 DESCRIPTION

These nodes can be used to, eg, note to the L<Pod::Writer> that an
upcoming closing node is to be represented in a certain non-normative
way in the source.

=head2 SUB-CLASS PROPERTIES

This node type keeps the C<source> property, and adds nothing else.

More specialised processing instructions may sub-class this module or
provide special behaviour when a property is set.  If we ever need
them C<:)>

=cut

sub event_type {
    "processing_instruction"
}
