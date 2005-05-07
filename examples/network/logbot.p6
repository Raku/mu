#!/usr/bin/pugs

use v6;
use Net::IRC;

# Parse @*ARGS
my $nick   = @*ARGS[0] // "blechbot";
my $server = @*ARGS[1] // "localhost";
my ($host, $port) = split ":", $server;
$port //= 6667;

# Create new bot "object"
my $bot = new_bot(nick => $nick, host => $host, port => $port, debug_raw => 0);
$bot<add_command_handler>("INVITE",  \&on_invite);
$bot<add_command_handler>("PRIVMSG", \&on_privmsg);
$bot<add_command_handler>("JOIN",    \&on_join);
$bot<add_command_handler>("PART",    \&on_part);
$bot<add_command_handler>("QUIT",    \&on_quit);
$bot<connect>();
$bot<login>();
$bot<run>();

# Later, some kind of timestamp will need to be added, but there isn't yet a
# strftime().
sub log(Str $msg) { print "$msg\n" }

sub on_invite($event) {
  my ($from, $chan) = $event<from rest>;
  debug "Got an invitation from \"$from\" to join channel \"$chan\".";
  $bot<join>($chan);
}

sub on_privmsg($event) {
  if($event<rest> ~~ rx:P5/^\001ACTION (.*)\001?$/) {
    log "* $event<from_nick> $1";
  } else {
    log "<$event<from_nick>> $event<rest>";
  }
}
sub on_join($event)    { log "*** $event<from_nick> has joined $event<object>" }
sub on_part($event)    { log "*** $event<from_nick> has left $event<object> ($event<rest>)" }
sub on_quit($event)    { log "*** $event<from_nick> has quit IRC ($event<object>)" }
