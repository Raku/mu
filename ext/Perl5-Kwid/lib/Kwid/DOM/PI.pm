
package Kwid::DOM::PI;

use strict;
use warnings;
use base 'Kwid::DOM::Node';

=head1 NAME

Kwid::DOM::PI - a processing instruction in a Kwid::DOM tree

=head1 SYNOPSIS

See L<Kwid::DOM::Node>.

=head1 DESCRIPTION

These nodes can be used to, eg, note to the L<Pod::Writer> that an
upcoming closing node is to be represented in a certain way in source.

=head2 SUB-CLASS PROPERTIES

This node type keeps the C<source> property, and adds nothing else.

More specialised processing instructions may sub-class this module or
provide special behaviour when a property is set.  If we ever need
them C<:)>

=cut

1;
