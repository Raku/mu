module Net::IRC-0.03;
# This is a take at a Perl 6 IRC library.
# You can run it, too.
# This library provides the "classes" bot and queue, to be used in other bots.
# One will be able/is able to say:
#   my $bot = new_bot(nick => $nick, host => $host, port => $port);
#   $bot<connect>();
#   $bot<login>();
#   $bot<join>("#channel");
#   $bot<run>();            # Enter main event loop
# Note that this is *not* a port of Perl 5's Net::IRC.

use v6;

sub debug(Str $msg) is export {
  state $is_fresh;
  $is_fresh //= 1;

  print "» " if $is_fresh;
  if($msg ~~ rx:P5/ $/) {
    print "$msg";
    $is_fresh = 0;
  } else {
    say $msg;
    $is_fresh++;
  }
}
sub debug_recv(Str $msg) { say "< $msg" }
sub debug_sent(Str $msg) { say "> $msg" }

# Definition of the bot "class"
sub new_bot(
  Str $nick is copy,
  Str ?$username = $nick,
  Str ?$ircname  = $nick,
  Str $host,
  Int ?$port         = 6667,
  Int ?$autoping     = 90,     # Autoping the server when we haven't seen traffic for 90s
  Int ?$live_timeout = 120,    # Drop connection when we haven't seen traffic for 120s
  Bool ?$debug_raw = 0,
) is export {
  my $connected = 0;
  my $inside    = 0;
  my @on_chans;            # Which chans did we join?
  my $servername;          # What is the IRC servername?
  my $last_traffic;        # Timestamp of last traffic seen from server
  my $last_autoping;       # Timestamp of last ping sent to server
  my $hdl;                 # Socket
  my $queue = new_queue(); # Queue (will later throttle our bot's output)
  my %numeric_handler;     # Callbacks for numeric messages (001)
  my %command_handler;     # Callbacks for command messages (PRIVMSG)

  # Default (passive) handlers
  %numeric_handler<001> = [ -> $event {
    $inside++;
    $servername = $event<server>;
    $nick       = $event<to>;
    debug "Logged in to \"$servername\" as \"$nick\".";
  }];
  %command_handler<JOIN> = [ -> $event {
    if($event<from_nick> eq $nick) {
      push @on_chans, $event<object>;
      debug "Joined channel \"$event<object>\".";
    }
  }];
  %command_handler<PART> = [ -> $event {
    if($event<from_nick> eq $nick) {
      @on_chans .= grep:{ $^chan ne $event<object> };
      debug "Left channel \"$event<object>\".";
    }
  }];
  %command_handler<KICK> = [ -> $event {
    my ($kickee, $reason) = split " ", $event<rest>;
    $reason = strip_colon($reason);
    if($kickee eq $nick) {
      @on_chans .= grep:{ $^chan ne $event<object> };
      debug "Was kicked from channel \"$event<object>\" by \"$event<from>\" (\"$reason\").";
    }
  }];
  %command_handler<NICK> = [ -> $event {
    if($event<from_nick> eq $nick) {
      $nick = $event<object>;
      debug "Changed nick to \"$event<object>\".";
    }
  }];

  # Sub which sends $msg, flushes $hdl and logs $msg to STDERR.
  my $say = -> Str $msg {
    debug_sent $msg if $debug_raw;
    $hdl.say($msg);
    $hdl.flush();
  };

  # Instance methods
  my $self = {
    # Readonly accessors
    nick          => { $nick },
    username      => { $username },
    ircname       => { $ircname },
    servername    => { $servername },
    connected     => { $connected },
    logged_in     => { $inside },
    last_traffic  => { $last_traffic },
    last_autoping => { $last_autoping },

    # Handler register methods
    add_numeric_handler => -> Str $code, Code $cb {
      %numeric_handler{$code}.push($cb);
    },
    add_command_handler => -> Str $cmd,  Code $cb {
      %command_handler{$cmd}.push($cb);
    },

    # Connect/disconnect methods
    connect    => { $self<reconnect>() },
    reconnect  => {
      $self<disconnect>() if $connected;

      debug "Connecting to $host:$port... ";
      try { $hdl = connect($host, $port) }
      if($hdl) {
	try { $hdl.autoflush(1) }
	$connected++;
	$last_traffic  = time;
	$last_autoping = time;
	debug "done.";
      } else {
	debug "failed ($!).";
      }
    },
    disconnect => {
      if($connected) {
	debug "Disconnecting from $host:$port... ";
	try { $hdl.close }
	$connected     = 0;
	$inside        = 0;
	@on_chans      = ();
	$servername    = undef;
	$hdl           = undef;
	$last_traffic  = 0;
	$last_autoping = 0;
	debug "done.";
      }
    },

    # Login
    login => {
      if($connected) {
	$queue<enqueue>({
	  $say("NICK $nick");
	  $say("USER $username * * :$ircname");
	});
      }
    },

    # Process $queue, wait for input from server and process it
    run => {
      while($connected) {
	$queue<run>();
	$self<readline>();
	$self<livecheck>();
      }
    },

    # Read a line from server and process it
    "readline" => {
      my $line = readline $hdl;
      $line ~~ s:P5/[\015\012]*$//; # Hack to remove all "\r\n"s
      debug_recv $line if $debug_raw;
      $last_traffic = time;

      if($line ~~ rx:P5/^:([^ ]+) (\d+) ([^ ]+) ?(.*)$/) {
	$self<handle_numeric>($line, $1, $2, $3, $4);
      } elsif($line ~~ rx:P5/^:([^ ]+) (\w+) ([^ ]+) ?(.*)$/) {
	$self<handle_command>($line, $1, $2, $3, $4);
      } elsif($line ~~ rx:P5/^ERROR ?:?(.*)$/) {
	debug "Error in connection to $host:$port (\"$1\").";
	$self<disconnect>();
      } elsif($line ~~ rx:P5/^PING ?:?(.*)$/) {
	$say("PONG $1");
      } else {
	debug "No handler found for \"$line\".";
      }
    },

    # Handle numeric commands (e.g. 001 -> welcome)
    handle_numeric => -> Str $line, Str $server, Str $code, Str $to, Str $rest {
      my $event = {
	line   => $line,
	server => $server,
	to     => $to,
	rest   => strip_colon($rest),
      };

      if(%numeric_handler{$code}) {
	$_($event) for *%numeric_handler{$code};
      }
    },

    # Handle word commands (e.g. JOIN, INVITE)
    handle_command => -> Str $line, Str $from, Str $command, Str $object is copy, Str $rest is copy {
      my $from_nick; $from_nick = $1 if $from ~~ rx:P5/^([^!]+)!/; #/#--vim
      my $event = {
	line      => $line,
	from      => $from,
	from_nick => $from_nick,
	rest      => strip_colon($rest),
	object    => strip_colon($object),
      };

      if(%command_handler{$command}) {
	$_($event) for *%command_handler{$command};
      }
    },

    # Check that our connection is still alive
    livecheck => {
      if($connected) {
	if($servername and time() - $last_traffic >= $autoping and time() - $last_autoping >= 60) {
	  debug "No traffic seen for {time() - $last_traffic} seconds; pinging server.";
	  $self<raw>("PING :$servername");
	  $last_autoping = time;
	}

	if(time() - $last_traffic >= $live_timeout) {
	  debug "No traffic seen for {time() - $last_traffic} seconds; disconnecting.";
	  $self<disconnect>();
	}
      }
    },

    # Join/part/kick/...
    join => -> Str $chan { if($connected) { $queue<enqueue>({ $say("JOIN $chan") }) } },
    part => -> Str $chan { if($connected) { $queue<enqueue>({ $say("PART $chan") }) } },
    quit => -> Str $reason { if($connected) { $queue<enqueue>({ $say("QUIT :$reason") }) } },

    # PRIVMSG/NOTICE
    privmsg => -> Str $to, Str $text {
      if($connected) { $queue<enqueue>({ $say("PRIVMSG $to :$text") }) }
    },
    notice  => -> Str $to, Str $text {
      if($connected) { $queue<enqueue>({ $say("NOTICE $to :$text") }) }
    },

    # RAW
    raw => -> Str $command { if($connected) { $queue<enqueue>({ $say($command) }) } }
  };

  return $self;
}

# Definition of queue "class"
sub new_queue() {
  my @queue;

  # "Instance methods"
  return {
    # Run all entries of @queue. Will need throttling later.
    run => {
      my @q = splice @queue;
      $_() for @q;
    },

    # Enqueue a new callback
    enqueue => -> Code $code { push @queue, $code },
  };
}

sub strip_colon(Str $str is copy) {
  $str = $1 if $str ~~ rx:P5/^:(.*)/;
  return $str;
}


=head1 NAME

Net::IRC - IRC library for Pugs

=head1 SYNOPSIS

  use Net::IRC;
  
  # Create new bot "object"
  my $bot = new_bot(
    nick     => "blechbot",
    username => "blech",      # (optional, defaults to $nick)
    ircname  => "Ingo's Bot", # (optional, defaults to $nick)
    host     => "localhost",
    port     => 6667,         # (optional)
    autoping => 90,           # (optional, defaults to 90)
    live_timeout => 120,      # (optional, defaults to 120)
    debug_raw => 0,           # (optional, defaults to 0)
  );
  
  # Register callbacks
  $bot<add_command_handler>("INVITE",  \&on_invite);
  $bot<add_command_handler>("PRIVMSG", \&on_privmsg);

  # Connect and login
  $bot<connect>();
  $bot<login>();

  # Enter main event loop
  $bot<run>();

=head1 DESCRIPTION

C<Net::IRC> is an IRC library for Pugs. Note that it is I<not> a port of Perl
5's C<Net::IRC>.

=head1 METHODS

=head2 C<new_bot(...)>

Creates a new bot "object". See L<SYNOPSIS> for accepted parameters.

The bot will autoping the server if it hasn't seen traffic for C<$autoping>
seconds, and it'll drop the connection if it hasn't seen traffic for
C<$live_timeout> seconds.

=head2 C<nick()>, C<username()>, C<ircname()>, C<last_traffic()>, C<last_autoping()>

=head2 C<connected()>

Returns a true value if the bot's socket to the server is currently connected.

=head2 C<logged_in()>

Returns a true value if the bot is logged in.

=head2 C<add_numeric_handler("001", -E<gt> $event {...})>

=head2 C<add_command_handler("JOIN", -E<gt> $event {...})>

Adds a callback to be executed when the bot receives a corresponding message.

The callback is given a C<$event> hashref, containing:

=over

=item C<line>

The original line received from the server (unmodified).

=item C<server>

The server the line was sent from. This is I<not> necessarily the server you
connected to.

=item C<to>

The nick the message was sent to.

=item C<rest>

Additional information (varies).

=item C<from>

The C<nick!hostmask> the message was sent from. Not available for numeric
handlers.

=item C<from_nick>

The nick part of C<from>. Not available for numeric handlers.

=item C<object>

The nick/channel/whatever the message operated (varies).

=back

=head2 C<join("#chan")>, C<part("#chan")>, C<quit("reason")>

=head2 C<privmsg(to =E<gt> "...", text =E<gt> "...")>, C<notice(to =E<gt> "...", text =E<gt> "...")>

=head2 C<raw("...")>

=head1 EXAMPLES

See F<examples/network/seenbot.p6> for a example bot using C<Net::IRC>.

=head1 AUTHOR

Ingo Blechschmidt <iblech@web.de>

=head1 LICENSE

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlgpl> and L<perlartistic> for details.

=cut
