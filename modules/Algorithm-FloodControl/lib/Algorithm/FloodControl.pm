module Algorithm::FloodControl-1.0;
use v6;

##############################################################################
#
#  Algorith::FloodControl
#  Vladi Belperchinov-Shabanski "Cade" <cade@biscom.net> <cade@datamax.bg>
#
#  DISTRIBUTED UNDER GPL! SEE `COPYING' FILE FOR DETAILS
#
##############################################################################

our Array %FLOOD;

sub flood_check(Num $count, Num $time, Str ?$name is copy) is export {
  # See L<S06/"The C<caller> function">.
  $name //= join ":", caller.file, caller.line;
  
  my $ar = $FLOOD{$en}; # get array ref for event's flood array
  my $ec = +@$ar;       # events count in the flood array
  
  if $ec >= $fc {
    # flood array has enough events to do real flood check
    my $ot = $ar[0];      # oldest event timestamp in the flood array
    my $tp = time - $ot;  # time period between current and oldest event
    
    # now calculate time in seconds until next allowed event
    my $wait = int(($ot + ($ec * $fp / $fc)) - time);
    if $wait > 0 {
      # positive number of seconds means flood in progress
      # event should be rejected or postponed
      # print "WARNING: next event will be allowed in $wait seconds\n";
      return $wait;
    }

    # negative or 0 seconds means that event should be accepted
    # oldest event is removed from the flood array
    shift @$ar;
  }

  # flood array is not full or oldest event is already removed
  # so current event has to be added
  push @$ar, time;
  # event is ok
  return 0;
}

sub flood_storage() is rw is export {
  return new Proxy:
    FETCH => { %FLOOD },
    STORE => { %FLOOD = %^new };
}

1;

=pod

=head1 NAME

Algorithm::FloodControl - Limit event processing to count/time ratio.

=head1 SYNOPSIS

  use Algorithm::FloodControl;

  my $wait = flood_check 5, 60, 'FLOOD EVENT NAME';

  if $wait {
    say "Please wait {$wait}s before requesting this resource again.";
  } else
    say "Ok, here you are.";
  }

=head1 DESCRIPTION

"Flood control" method is used to restrict the number of events to happen or 
to be processed in specific perion of time. Few examples are: web server can 
limit requsets number to a page or you may want to receive no more than 10 SMS 
messages on your GSM Phone per hour. Applications of this method are unlimited.

=head1 FUNCTIONS

This module exports several functions:

=over 4

=item flood_check(Num $count, Num $time, Str ?$name)

This function is the core of the module. It receives 3 arguments: maximum event 
count, maximum time period (in seconds) for this event count and finally the event 
name. There is internal storage so flood_check() can track several events by name.

The third argument could be omitted. In this case the event name will be
constructed from file name and line number of the calling point. However this
is not recommended unless you need it for very simple programs.

The return value is time in seconds that this event must wait to be processed
or 0 if event can be processed immediately.

=item flood_storage = %hash

If you want to save and restore the internal storage (for example for CGI use),
you can get it with flood_storage() function which returns the storage and it
can be stored with other module like Storable.

=back

=head1 AUTHOR

Vladi Belperchinov-Shabanski "Cade"

E<lt>cade@biscom.netE<gt> E<lt>cade@datamax.bgE<gt> E<lt>cade@cpan.orgE<gt>

L<http://cade.datamax.bg>

Port to Perl 6 by Ingo Blechschmidt E<lt>iblech@web.deE<gt>

=cut
