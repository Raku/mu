
package Perldoc::Sender;
use strict;
use warnings;

use Spiffy qw(field);
use Carp;

=head1 NAME

Perldoc::Sender - a description of how to `send' parse events

=head1 SYNOPSIS

  # get a sender from a sub-class constructor, like a Parser
  my $reader = Perldoc::Reader->new( file => $filename );
  my $sender = Perldoc::Parser->new( reader => $reader );
  my $receiver = Perldoc::DOM->new();

  # either plumb it up first;
  $sender->receiver($receiver);

  $sender->send_all;

  1 while $sender->send_one;

  # or specify the receiver immediately
  $sender->send_all_to($receiver);

  1 while $sender->send_one_to($receiver);

=head1 METHODS

=over

=item B<send_to($object)>

Sends all events to the specified place.  $object must be a
(C<-E<gt>isa>) Perldoc::Receiver.  Sub-classes should provide this
method.


C<$object> straight away, or pass the next events it receives.

If you use the sub-class API provided in this module, then the events
you send will automatically be correctly balanced.  This does mean
that C<send_one()> might actually send more that one event at a time
as those extra events are inserted into the event stream.

Also, the utility functions provided by this module do not give you a
re-entrant sender.  Even the C<send_all> method cannot be safely used
re-entrantly.  That means you must not try to use the same object in
more than one stream simultaneously; wait for it to finish, you
impatient lout!  :)

Of course, you don't need to use this module to successfully send
events to C<Perldoc::Receiver> classes.  See L<Perldoc::Receiver> for more.

This module requires C<Spiffy>, and uses C<Spiffy> to create accessors
- but does not turn sub-classes of this module into C<Spiffy>
sub-classes.

=back

=cut

field 'receiver';

# this wrapper function will assist sending the events to the receiver.
# design tbc.
sub send_all {
    my $self = shift;
    1 while $self->send_one;
}

=head2 SUB-CLASS API

The below method is used for sending by sub-classes of this module.

=over

=item $self->send($event, @args)

Sends an event to the configured receiver.

Allowable events are;

=cut

field 'sendstate';

field 'sendstack';
our $DEBUG = 0;

# this is the API that the sender sub-classes use
sub send {
    my $self = shift;
    my $event = shift;

    print STDERR "Request to emit: $event @_\n" if $DEBUG;
    $event and $event =~ m/^(?:(?:start|end)_(?:document|element)|characters|processing_instruction|ignoreable_whitespace|comment)$/x
	or croak "$self sent bad event `$event'";

    if ( $event eq "start_document" and $self->sendstate ) {
	$self->send("end_document");
    }

    # check to see if we need to start a document
    $self->send("start_document")
	if ( ! $self->sendstate and $event ne "start_document" );

    $self->send("start_element", { name => "perldoc" })
	if ( $self->sendstate and
	     $self->sendstate eq "start" and
	     $event ne "start_element" );

    # check to see if any dummy events are needed
    if ( $event eq "end_element" and defined(my $name = $_[0]) ) {
	my $stack = $self->sendstack;
	my $top;
	croak "can't close unseen element `$name'"
	    unless grep { $_ eq $name } @$stack;

	$self->send("end_element", $top)
	    while ( ($top = $stack->[$#$stack]) ne $name );
    }

    $self->send("end_element", $self->sendstack->[0])
	if ( $event eq "end_document" and
	     $self->sendstack and @{$self->sendstack});

    # ok, enough state sanity - send.
    my $receiver = $self->receiver or croak "no receiver!";
    if ( $event eq "start_element" ) {
	defined(my $name = $_[0])
	    or croak "start_element event with no name";
	push @{ $self->sendstack }, $name;
    }

    print STDERR "Emitting: $event @_\n" if $DEBUG;
    if ( $receiver->can($event) ) {
	$receiver->$event(@_);
    }
    if ( $event eq "end_element" ) {
	pop @{ $self->sendstack };
    }

    # fixme - add more checking...
    my $ss = $self->sendstate;
    if ( ! $ss ) {
	$self->sendstate("start");
	$self->sendstack([]);
    } elsif ( $ss eq "start" ) {
	$self->sendstate("body");
    } elsif ( $ss eq "body" and !@{ $self->sendstack } ) {
	$self->sendstate("end");
    } elsif ( $ss eq "end" ) {
	$self->sendstate(undef);
	$self->sendstack(undef);
    }

}


=over

=item B<start_document({})>

This should start the stream.  pass a hash of options.

=item B<end_document>

=item B<start_element(name, { name => "foo", ...})>

Start a L<POD::DOM::Element>.  C<name> must be set.  Note that the

=item B<end_element(name)>

Close a L<POD::DOM::Element>.  

=item B<characters(text)>

=item B<processing_instruction({})>

=item B<ignorable_whitespace(text)>

=item B<comment>

=back

=cut

1;
