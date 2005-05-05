#!/usr/bin/pugs

use v6;
use Net::IRC;

# Parse @*ARGS
my $nick   = @*ARGS[0] // "blechbot";
my $server = @*ARGS[1] // "localhost";
my ($host, $port) = split ":", $server;
$port //= 6667;

my %seen;

# Create new bot "object"
my $bot = new_bot(nick => $nick, host => $host, port => $port, debug_raw => 1);
$bot<connect>();
$bot<login>();
$bot<add_command_handler>("INVITE",  \&on_invite);
$bot<add_command_handler>("PRIVMSG", \&on_privmsg);
$bot<run>();

sub on_invite($event) {
  my ($from, $chan) = $event<from rest>;
  debug "Got an invitation from \"$from\" to join channel \"$chan\".";
  $bot<join>($chan);
}

sub on_privmsg($event) {
  %seen{$event<from_nick>} = {
    date => time,
    text => $event<rest>,
  };

  given $event<rest> {
    my $reply_to = substr($event<object>, 0, 1) eq "#" ?? $event<object> :: $event<from_nick>;

    when rx:P5/^\?seen\s+(.+)$/ {
      my $reply_msg = %seen{$1}
	?? "$1 was last seen {time() - %seen{$1}<date>} seconds ago, saying: %seen{$1}<text>"
	:: "Never seen $1.";
      $bot<notice>(to => $reply_to, text => $reply_msg);
    }

    when rx:P5/^\?quit\s*(.*)$/ {
      debug "Got quit request from \"$event<from>\".";
      $bot<quit>($1);
    }

    when rx:P5/^\?raw\s+(.+)$/ {
      debug "Got raw request from \"$event<from>\".";
      $bot<raw>($1);
    }

    when rx:P5/^\?uptime$/ {
      debug "Got uptime request from \"$event<from>\".";
      my $start_time = BEGIN { time };
      $bot<notice>(to => $reply_to, text => "Running for {time() - $start_time} seconds.");
    }
  }
}
