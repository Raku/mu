
package Kwid::DOM;
use strict;
use warnings;
use Kwid::Base;
use base 'Kwid::Base';
use Carp;

=head1 NAME

Kwid::Emitter - a description of how to `emit' parse events

=head1 SYNOPSIS

  $emitter->emit_to($receiver);

=head1 METHODS

=over

=item B<emit_to($object)>

Emits all events to the specified place.  $object must be a
(C<-E<gt>isa>) Kwid::Receiver.  Sub-classes should provide this
method.

Depending on the Emitter class, this might fire lots of events to
C<$object> straight away, or pass the next events it receives.

Emitters should ensure that events emit are correctly balanced.  This
module may end up providing utility functions to help facilitate this.

=cut

field 'target';

sub emit_to {
    my $self = shift;
    my $target = shift;

    $target and blessed $target and $target->isa("Kwid::Receiver")
	or croak "can't emit to `$target'";

    $self->target($target);
}

# this wrapper function will assist sending the events to the target.
# design tbc.
sub emit {
    # ...
}

=item B<start_document({})>

This should start the stream.  pass a hash of options

=item B<end_document>

=item B<start_element({})>

Start a L<POD::DOM::Element>

=item B<end_element>

=item B<characters>

=item B<processing_instruction>

=item B<ignorable_whitespace>

=back

=cut

