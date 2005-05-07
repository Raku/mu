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
use Algorithm::TokenBucket;

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
  Str $nick,
  Str ?$username = $nick,
  Str ?$ircname  = $nick,
  Str $host,
  Int ?$port          = 6667,
  Int ?$autoping      = 90,     # Autoping the server when we haven't seen traffic for 90s
  Int ?$live_timeout  = 120,    # Drop connection when we haven't seen traffic for 120s
  Bool ?$floodcontrol = 0,      # Check that we don't flood excessively
  Bool ?$debug_raw = 0,
) is export {
  my $connected = 0;
  my $inside    = 0;
  my @on_chans;            # Which chans have we joined?
  my $servername;          # What is the IRC servername?
  my $last_traffic;        # Timestamp of last traffic seen from server
  my $last_autoping;       # Timestamp of last ping sent to server
  my $hdl;                 # Socket
  my $queue = new_queue(floodcontrol => $floodcontrol);
			   # Queue
  my %handler;             # Callbacks for numeric (001) and command (PRIVMSG)
                           # messages
  my $curnick;             # Our current nick.
  my $nickgen = new_permutation($nick);
			   # A permutation object, providing the methods
			   # "reset" and "next".
  my $in_login_phase;

  my $self;

  # Default (passive) handlers
  %handler<001> = [-> $event {
    $inside++;
    $servername     = $event<server>;
    $curnick        = $event<to>;
    $in_login_phase = 0;
    $self<handle_pseudo>("loggedin");
    debug "Logged in to \"$servername\" as \"$curnick\".";
  }];
  %handler<433> = [-> $event {
    if $in_login_phase {
      $self<nick>($nickgen<next>());
    }
  }];
  %handler<JOIN> = [-> $event {
    if(normalize($event<from_nick>) eq normalize($curnick)) {
      push @on_chans, $event<object>;
      debug "Joined channel \"$event<object>\".";
    }
  }];
  %handler<PART> = [-> $event {
    if(normalize($event<from_nick>) eq normalize($curnick)) {
      @on_chans .= grep:{ $^chan ne $event<object> };
      debug "Left channel \"$event<object>\".";
    }
  }];
  %handler<KICK> = [-> $event {
    my ($kickee, $reason) = split " ", $event<rest>;
    $reason = strip_colon($reason);
    if(normalize($kickee) eq normalize($curnick)) {
      @on_chans .= grep:{ $^chan ne $event<object> };
      debug "Was kicked from channel \"$event<object>\" by \"$event<from>\" (\"$reason\").";
    }
  }];
  %handler<KILL> = [-> $event {
    my ($killee, $reason) = $event<object rest>;
    if(normalize($killee) eq normalize($curnick)) {
      @on_chans = ();
      debug "Was killed by \"$event<from>\" (\"$reason\").";
    }
  }];
  %handler<NICK> = [-> $event {
    if(normalize($event<from_nick>) eq normalize($curnick)) {
      $curnick = $event<object>;
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
  $self = {
    # Readonly accessors
    curnick       => { $curnick },
    username      => { $username },
    ircname       => { $ircname },
    servername    => { $servername },
    connected     => { $connected },
    logged_in     => { $inside },
    last_traffic  => { $last_traffic },
    last_autoping => { $last_autoping },
    channels      => { @on_chans },

    # Handler register methods
    add_handler => -> Str $code, Code $cb { %handler{$code}.push($cb) },

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
	$connected      = 0;
	$inside         = 0;
	@on_chans       = ();
	$servername     = undef;
	$hdl            = undef;
	$last_traffic   = 0;
	$last_autoping  = 0;
	$in_login_phase = 0;
	$queue<clear>();
	$nickgen<reset>();
	debug "done.";
      }
    },

    # Login
    login => {
      if($connected) {
	$queue<enqueue>({
	  $in_login_phase++;
	  $say("NICK {$nickgen<next>()}");
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

    # Read a line from the server and process it
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

      if(%handler{$code}) {
	$_($event) for *%handler{$code};
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

      if(%handler{$command}) {
	$_($event) for *%handler{$command};
      }
    },

    handle_pseudo => -> Str $pseudo, *@args {
      my $event = {
	pseudo => $pseudo,
	args   => @args,
      };

      if(%handler{$pseudo}) {
	$_($event) for *%handler{$pseudo};
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
    join => -> Str $chan    { $queue<enqueue>({ $say("JOIN $chan") })    if $connected },
    part => -> Str $chan    { $queue<enqueue>({ $say("PART $chan") })    if $connected },
    quit => -> Str $reason  { $queue<enqueue>({ $say("QUIT :$reason") }) if $connected },
    nick => -> Str $newnick { $queue<enqueue>({ $say("NICK $newnick") }) if $connected },

    # PRIVMSG/NOTICE
    privmsg => -> Str $to, Str $text {
      $queue<enqueue>({ $say("PRIVMSG $to :$text") }) if $connected;
    },
    notice  => -> Str $to, Str $text {
      $queue<enqueue>({ $say("NOTICE $to :$text") }) if $connected;
    },

    # RAW
    raw => -> Str $command { $queue<enqueue>({ $say($command) }) if $connected }
  };

  return $self;
}

# Definition of queue "class"
sub new_queue(Bool ?$floodcontrol = 0) {
  my @queue;

  my $bucket = $floodcontrol
    ?? new_bucket(rate => (1/2),    burst_size => 5)
    :: new_bucket(rate => (1000/2), burst_size => 5000); # hack
  $bucket<fill>();

  # "Instance methods"
  return {
    # Run all entries of @queue. Will need throttling later.
    run => {
      my @q = splice @queue;
      while(@q) {
	if($bucket<conform>(1)) {
	  $bucket<count>(1);
	  @q.shift().();
	} else {
	  last;
	}
      }
      @queue.unshift(@q);
    },

    # Enqueue a new callback
    enqueue => -> Code $code { push @queue, $code },

    # Remove all items from the queue
    clear => { @queue = () },
  };
}

sub new_permutation(Str $nick) {
  my @perms;

  my $self = {
    reset => {
      @perms = (
	"$nick",
	"{$nick}_", "{$nick}__",
	"_{$nick}", "__{$nick}",
	"_{$nick}_", "__{$nick}__",
      );
    },
    "next" => {
      my $cur = @perms.shift;
      @perms.push($cur);

      $cur;
    },
  };

  $self<reset>();
  return $self;
}

sub strip_colon(Str $str is copy) {
  $str = substr $str, 1 if substr($str, 0, 1) eq ":";
  return $str;
}

sub normalize(Str $str) { return lc $str }

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
    floodcontrol => 0,        # (optional, defaults to 0)
    debug_raw => 0,           # (optional, defaults to 0)
  );
  
  # Register callbacks
  $bot<add_command_handler>("INVITE",   \&on_invite);
  $bot<add_command_handler>("PRIVMSG",  \&on_privmsg);
  $bot<add_command_handler>("loggedin", \&on_ready);

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

=head2 C<curnick()>, C<username()>, C<ircname()>, C<last_traffic()>, C<last_autoping()>

=head2 C<connected()>

Returns a true value if the bot's socket to the server is currently connected.

=head2 C<logged_in()>

Returns a true value if the bot is logged in.

=head2 C<channels()>

Returns a list of channels the bot has joined.

=head2 C<add_handler("001", -E<gt> $event {...})>

=head2 C<add_handler("JOIN", -E<gt> $event {...})>

=head2 C<add_handler("loggedin", -E<gt> $event {...})>

Adds a callback to be executed when the bot receives a corresponding message.
Event name all lowercase are "pseudo" events. Currently, there's only the
C<loggedin> pseudo event, indicating that the bot has successfully logged in.

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

=item C<pseudo>

The pseudo event.

=back

=head2 C<join("#chan")>, C<part("#chan")>, C<quit("reason")>, C<nick("newnick")>

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
