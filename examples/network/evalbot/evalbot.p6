#!/usr/bin/pugs

# Modules
BEGIN { print $*ERR: "evalbot6 started. Loading " }
use v6;
BEGIN { print $*ERR: "Net::IRC... " }
use Net::IRC;
BEGIN { print $*ERR: "File::Spec... " }
use File::Spec;

# Parse @*ARGS
my $nick   = @*ARGS[0] // "evalbot6";
my $server = @*ARGS[1] // "localhost";
my ($host, $port) = split ":", $server;
$port //= 6667;

debug "Summary of configuration:";
debug "  Will connect as...                  $nick";
debug "  to...                               $host:$port";
debug "To change any of these parameters, restart $*PROGRAM_NAME";
debug "and supply appropriate arguments:";
debug "  $*PROGRAM_NAME nick host[:port]";

# Create new bot "object"
my $bot = new_bot(nick => $nick, host => $host, port => $port, debug_raw => 0);
$bot<connect>();
$bot<login>();
$bot<add_handler>("INVITE",   &on_invite);
$bot<add_handler>("PRIVMSG",  &on_privmsg);
$bot<run>();

# Join on invite
sub on_invite($event) {
  my ($from, $chan) = $event<from rest>;
  debug "Got an invitation from \"$from\" to join channel \"$chan\".";
  $bot<join>($chan);
}

# Process private messages
sub on_privmsg($event) {
  given $event<rest> {
    # Logging is important, especially with an *eval*bot.
    debug "Received a ?-request from $event<from>: $event<rest>"
      if substr($event<rest>, 0, 1) eq "?";

    my $reply_to = substr($event<object>, 0, 1) eq "#" ?? $event<object> :: $event<from_nick>;

    when rx:P5/^\?quit\s*(.*)$/ {
      $bot<quit>($0);
    }

    when rx:P5/^\?raw\s+(.+)$/ {
      $bot<raw>($0);
    }

    when rx:P5/^\?uptime$/ {
      my $start_time = INIT { time };
      $bot<privmsg>(to => $reply_to, text => "Running for {int(time() - $start_time)} seconds.");
    }

    when rx:P5/^\?eval\s+(.+)$/ {
      # Only allow ?eval in channels, not per private message, so other people
      # on a channel may see abuse of the evalbot.
      if substr($reply_to, 0, 1) eq "#" {
        $bot<privmsg>(to => $reply_to, text => evalhelper($0));
      } else {
        $bot<privmsg>(to => $reply_to, text => "?eval command is only available in public channels");
      }
    }

    # This is *not* correct CTCP parsing (the CTCP specification is much more
    # complex than this simple regex), but IRC clients only send this when
    # their users enter /PING bot.
    when rx:P5/^\001PING (.*)\001$/ {
      $bot<notice>(to => $event<from_nick>, text => "\001PING $0\001");
    }
  }
}

sub evalhelper(Str $code) {
  # Find the location of evalhelper.p5.
  my $evalhelper = INIT {
    print $*ERR: "evalhelper.p5...";
    my @path_parts = splitpath $*PROGRAM_NAME;
    my $progdir = @path_parts[1];
    $progdir = "." if $progdir ~~ rx:Perl5/^\s*$/;
    print $*ERR: "done.\n";
    "$progdir/evalhelper.p5";
  };

  # Prevent possible abuse.
  return "Code to eval exceeds maximum length limit." if bytes $code > 500;
  return "No code to eval given."
    if bytes $code == 0 or $code ~~ rx:Perl5/^\s*$/;

  # Set the necessary environment vars for evalhelper.p5.
  my $tmpfile = "temp-evalbot-$*PID";
  %*ENV<EVALBOT_CODE>    = $code;
  %*ENV<EVALBOT_TMPFILE> = $tmpfile;
  %*ENV<EVALBOT_PUGS>    = $*EXECUTABLE_NAME;
  # Eval!
  system "perl", $evalhelper;

  # Read the result, but convert all linebreaks into spaces, so we don't flood
  # the channel.
  my $result = join " ", split "\n", slurp $tmpfile;
  unlink $tmpfile;
  return bytes($result) ?? $result :: "(no output)";
}
