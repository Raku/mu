#!/usr/bin/pugs
# This is a take at a Perl 6 IRC library.
# You can run it, too.
# This library provides the "classes" bot and queue, to be used in other bots.
# One will be able/is able to say:
#   my $bot = new_bot(nick => $nick, host => $host, port => $port);
#   $bot<connect>();
#   $bot<login>();
#   $bot<join>("#channel");
#   $bot<run>();            # Enter main event loop

use v6;

sub debug(Str $msg) {
  state $is_fresh;

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

debug "Event-driven Perl 6 IRC Bot.";

# Parse @*ARGS
my $nick   = @*ARGS[0] // "blechbot";
my $server = @*ARGS[1] // "localhost";
my ($host, $port) = split ":", $server;
$port //= 6667;

# Create new bot "object"
my $bot = new_bot(nick => $nick, host => $host, port => $port);
$bot<connect>();
$bot<login>();
$bot<run>();

# Definition of the bot "class"
sub new_bot(Str $nick is copy, Str $host, Int $port) {
  my $connected = 0;
  my $inside    = 0;
  my @on_chans;            # Which chans did we join?
  my $servername;          # What is the IRC servername?
  my $hdl;                 # Socket
  my $queue = new_queue(); # Queue (will later throttle our bot's output)

  # Sub which sends $msg, flushes $hdl and logs $msg to STDERR.
  my $say = -> Str $msg {
    debug_sent $msg;
    $hdl.say($msg);
    $hdl.flush();
  };

  # Instance methods
  my $self = {
    # Readonly accessors
    nick       => { $nick },
    servername => { $servername },
    connected  => { $connected },

    # Connect/disconnect methods
    connect    => { $self<reconnect>() },
    reconnect  => {
      $self<disconnect>() if $connected;

      debug "Connecting to $host:$port... ";
      try { $hdl = connect($host, $port) }
      if($hdl) {
	try { $hdl.autoflush(1) }
	$connected++;
	debug "done.";
      } else {
	debug "failed ($!).";
      }
    },
    disconnect => {
      if($connected) {
	debug "Disconnecting from $host:$port... ";
	try { $hdl.close }
	$connected  = 0;
	$inside     = 0;
	@on_chans   = ();
	$servername = undef;
	$hdl        = undef;
	debug "done.";
      }
    },

    # Login
    login => {
      if($connected) {
	$queue<enqueue>({
	  $say("NICK $nick");
	  $say("USER $nick $nick $nick $nick");
	});
      }
    },

    # Process $queue, wait for input from server and process it
    run => {
      while($connected) {
	$queue<run>();
	$self<readline>();
      }
    },

    # Read a line from server and process it
    "readline" => {
      my $line = readline $hdl;
      $line ~~ s:P5/[\015\012]*$//; # Hack to remove all "\r\n"s
      debug_recv $line;

      if($line ~~ rx:P5/^:([^ ]+) (\d+) ([^ ]+) ?:?(.*)$/) {
	$self<handle_numeric>($line, $1, $2, $3, $4);
      } elsif($line ~~ rx:P5/^:([^ ]+) (\w+) ([^ ]+) ?:?(.*)$/) {
	$self<handle_command>($line, $1, $2, $3, $4);
      } else {
	debug "No handler found for \"$line\".";
      }
    },

    # Handle numeric commands (e.g. 001 -> welcome)
    handle_numeric => -> Str $line, Str $server, Int $code, Str $to, Str $rest {
      my %dispatch = (
	# 001: Welcome to...
	"001" => {
	  $inside++;
	  $servername = $server;
	  $nick       = $to;
	  debug "Logged in to \"$servername\" as \"$nick\".";
	},
      );

      if(%dispatch{$code}) {
	%dispatch{$code}();
      } else {
	debug "No numeric handler found for \"$line\".";
      }
    },

    # Handle word commands (e.g. JOIN, INVITE)
    handle_command => -> Str $line, Str $from, Str $command, Str $object is copy, Str $rest is copy {
      # Strip leading ":"
      $rest   = $1 if $rest   ~~ rx:P5/^:(.*)/;
      $object = $1 if $object ~~ rx:P5/^:(.*)/;
      my $from_nick; $from_nick = $1 if $from ~~ rx:P5/^([^!]+)!/; #/#--vim

      my %dispatch = (
	INVITE => {
	  debug "Got an invitation to join \"$rest\" from \"$from\".";
	  $self<join>($rest);
	},
	JOIN => {
	  if($from_nick eq $nick) {
	    push @on_chans, $object;
	    debug "Joined channel \"$object\".";
	  }
	},
      );

      if(%dispatch{$command}) {
	%dispatch{$command}();
      } else {
	debug "No command handler found for \"$line\".";
      }
    },

    # Join/part/kick/...
    join => -> Str $chan {
      if($connected) {
	$queue<enqueue>({
	  $say("JOIN $chan");
	});
      }
    },
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
