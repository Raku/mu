module POE-0.0.1;

# Global POE::Kernel object.
our $POE::Kernel = ::POE::Kernel.new;

class POE::Kernel {
  # FIFO containing the events to call.
  has Code @:events;

  submethod BUILD ($self:) {
    die "There can only be one instance of POE::Kernel."
      if defined $POE::Kernel;

    $POE::Kernel = $self;
  }

  # Internal method which pushes a given $callback to @:events.
  my method enqueue (Code $callback) {
    push @:events, $callback;
    nothing;
  }

  # Perform one step -- i.e. get an event from @:events and call it.
  my method step () {
    return unless @:events;

    my $callback = shift @:events;
    return $callback();
  }

  method run () {
    while @:events {
      ./:step;
    }
  }

  # Post $event to $session and return the result.
  # The session's event handler won't be executed until @:events[0] =:= the
  # callback we enqueue().
  method post (POE::Session $session, Str $event, *@args) {
    # Push our callback on @:events.
    ./:enqueue({
      my $result = $session.dispatch($event, *@args);
      $result;
    });
    
    # And enter the runloop again.
    ./:step(); #/#--vim
  }
}

class POE::Session {
  method dispatch (Str $event, *@args) {
    die 'Please override &dispatch in your subclass of POE::Session.';
  }
}

=head1 NAME

POE - Perl Object Environment for Perl 6

=head1 SYNOPSIS

  class MySession is POE::Session {
    method dispatch (Str $event, *@args) {
        given $event {
            when "say_hello" {
                return "Hello, @args[0]!";
            }

            ...;
        }
    }
  }

  my $session = MySession.new;
  say $POE::Kernel.post($session, "say_hello", "Ingo");
  # "Hello, Ingo!"

=head1 DESCRIPTION

This is an port of Perl 5's POE to Perl 6. It is only an experiment, it's not
API compatible to the original POE, and is possibly very unstable.

=head1 METHODS ON C<$POE::Kernel>

=head2 C<post(POE::Session $session, Str $event, *@args)>

Calls C<$session>'s C<$event> event handler, passing C<@args> as arguments.
Note that possibly, the event handler is I<not> called immediately -- other
events may be pending processing, too.

=head2 C<run()>

Enter the main runloop.

=head1 AUTHOR

Ingo Blechschmidt C<< <iblech@web.de> >>
