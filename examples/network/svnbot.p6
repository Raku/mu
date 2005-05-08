#!/usr/bin/pugs

use v6;
use Net::IRC;

# Parse @*ARGS
my $nick     = @*ARGS[0] // "blechbot";
my $server   = @*ARGS[1] // "localhost";
my $interval = @*ARGS[2] // 120;
my ($host, $port) = split ":", $server;
$port //= 6667;

debug "svnbot started. Summary of configuration:";
debug "  Will connect as...                  $nick";
debug "  to...                               $host:$port";
debug "  checking for new revisions every... $interval seconds.";

# Initialize $cur_svnrev
my $cur_svnrev;
svn_headrev();

# Create new bot "object"
my $bot = new_bot(nick => $nick, host => $host, port => $port, debug_raw => 0);
$bot<add_handler>("INVITE",  \&on_invite);
$bot<add_handler>("PRIVMSG", \&on_privmsg);
$bot<add_handler>("runloop", \&svn_check);
$bot<connect>();
$bot<login>();
$bot<run>();

sub on_invite($event) {
  my ($from, $chan) = $event<from rest>;
  debug "Got an invitation from \"$from\" to join channel \"$chan\".";
  $bot<join>($chan);
}

sub on_privmsg($event) {
  given $event<rest> {
    debug "Received a ?-request from $event<from>: $event<rest>"
      if substr($event<rest>, 0, 1) eq "?";

    my $reply_to = substr($event<object>, 0, 1) eq "#" ?? $event<object> :: $event<from_nick>;

    when rx:P5/^\?quit\s*(.*)$/ {
      $bot<quit>($1);
    }

    when rx:P5/^\?raw\s+(.+)$/ {
      $bot<raw>($1);
    }

    when rx:P5/^\?uptime$/ {
      my $start_time = BEGIN { time };
      $bot<notice>(to => $reply_to, text => "Running for {time() - $start_time} seconds.");
    }
  }
}

sub svn_check($event) {
  state $last_check;
  $last_check //= time;  # "state $last_check = 0" doesn't work yet in Pugs.

  if(time() - $last_check >= $interval) {
    debug "Checking for new commits (ignore error messages)... ";
    my $commits = svn_commits();
    debug "done, HEAD revision: $cur_svnrev";

    $last_check = time;
    my @lines   = split("\n", $commits).grep:{ $_ };

    my $chans = join ",", $bot<channels>();
    return unless $chans;
    $bot<notice>($chans, $_) for @lines;
  }
}

sub svn_commits() {
  my $tempfile = "temp-svnbot";
  END { unlink $tempfile }

  if($cur_svnrev) {
    system "svn log -r {$cur_svnrev + 1}:HEAD . > $tempfile";
  } else {
    system "svn log -r HEAD . > $tempfile";
  }

  my $commits;
  my $fh = open $tempfile;
  for =$fh {
    given $_ {
      state $cur_entry;
      when rx:P5/^-----+/ {
	# ignore
      }

      when rx:P5/^r(\d+) \| (\w+)/ {
	$cur_entry = "r$1 ($2)";
	return if $1 == $cur_svnrev;
	$cur_svnrev = $1;
      }

      when rx:P5/\S/ {
	if($cur_entry) {
	  $_ ~~ rx:P5/^(.*)$/;
	  $commits ~= "* $cur_entry -- $1\n";
	}
      }
    }
  }

  return $commits;
}

sub svn_headrev() {
  debug "Retrieving current HEAD revision... ";
  svn_commits();
  debug "done.";
}
