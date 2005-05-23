#!/usr/bin/pugs

use v6;
use Net::IRC;

# Parse @*ARGS
my $nick    = @*ARGS[0] // "blechbot";
my $server  = @*ARGS[1] // "localhost";
my $max_bad = @*ARGS[2] // 6;
my ($host, $port) = split ":", $server;
$port //= 6667;

debug "hangmanbot started. Summary of configuration:";
debug "  Will connect as...                          $nick";
debug "  to...                                       $host:$port";
debug "  using a maximum number of bad guesses of... $max_bad.";
debug "To change any of these parameters, restart $*PROGRAM_NAME";
debug "and supply appropriate arguments:";
debug "  $*PROGRAM_NAME nick host[:port] max_bad_guesses";

# Copied from examples/games/hangman.p6.
sub get_committer_list(Str $dict_file) returns List {
  my @committers;
  my $dict = open($dict_file) err
    die "Couldn't open \"$dict_file\": $!\n";

  # Skip the intro text
  1 while =$dict ~~ rx:Perl5/\S/;

  for =$dict -> $name {
    # Capture the real name part
    if $name ~~ rx:Perl5/^(.+?)(?:\s\s|$)/ {
      my $realname = $0;
      # Remove nickname
      $realname ~~ s:Perl5/\s*".*"\s*/ /;  #/#--vim
      next unless $realname ~~ rx:Perl5/\S/;
      @committers.push($realname);
    }
  }

  return @committers;
}

debug "Reading AUTHORS... ";
my @devs = get_committer_list("AUTHORS");
debug "done.";

# Create new bot "object"
my $bot = new_bot(nick => $nick, host => $host, port => $port);
$bot<connect>();
$bot<login>();
$bot<add_handler>("INVITE",   &on_invite);
$bot<add_handler>("PRIVMSG",  &on_privmsg);
$bot<run>();

sub on_invite($event) {
  my ($from, $chan) = $event<from rest>;
  debug "Got an invitation from \"$from\" to join channel \"$chan\".";
  $bot<join>($chan);
}

sub on_privmsg($event) {
  state $game;

  given $event<rest> {
    my $reply_to = substr($event<object>, 0, 1) eq "#" ?? $event<object> :: $event<from_nick>;
    debug "Got ?-request from \"$event<from>\": $_" if m:Perl5/^\?/;

    when rx:P5/^\?quit\s*(.*)$/ {
      $bot<quit>($0);
    }

    when rx:P5/^\?raw\s+(.+)$/ {
      $bot<raw>($0);
    }

    when rx:P5/^\?uptime$/ {
      my $start_time = INIT { time };
      $bot<privmsg>(to => $reply_to, text => "Running for {time() - $start_time} seconds.");
    }

    when rx:P5/^\?help$/ {
      $bot<privmsg>(to => $reply_to, text => "Available commands: ?quit [reason], ?uptime, ?game, ?show, ?guess letter");
    }

    when rx:P5/^\?game$/ {
      $game = {
	dev         => @devs.pick,
	guesses     => [],
	bad_guesses => 0,
      };

      show_game($reply_to, $game);
    }

    when rx:P5/^\?show/ {
      if $game {
	show_game($reply_to, $game);
      } else {
	$bot<privmsg>(to => $reply_to, text => "There's no game running currently. You can start one using \"?game\".");
      }
    }

    when rx:P5/^\?guess ([a-zA-Z])/ {
      if $game {
	my $guess = lc $0;
	my %guesses;
	# No ».++ yet.
	%guesses{$_}++ for $game<guesses>;

	if %guesses{$guess} {
	  $bot<privmsg>(to => $reply_to, text => "You've already guessed \"$guess\"!");
	} else {
	  %guesses{$guess}++;
	  $game<guesses> = [ %guesses.keys.sort ];
	  if $game<dev> ~~ m:Perl5:i/[$guess]/ {
	    if complete($game) {
	      $bot<privmsg>(to => $reply_to, text => "Congratulations! The developer was $game<dev>.");
	      undef $game;
	    } else {
	      $bot<privmsg>(to => $reply_to, text => "Yeah, \"$guess\" is in the name.");
	      show_game($reply_to, $game);
	    }
	  } else {
	    $game<bad_guesses>++;
	    if $game<bad_guesses> < $max_bad {
	      $bot<privmsg>(to => $reply_to, text => "Sorry, \"$guess\" is not in the name.");
	    } else {
	      $bot<privmsg>(to => $reply_to, text => "Sorry, you exceedded the maximum number of tries. The developer was $game<dev>.");
	      undef $game;
	    }
	  }
	}
      } else {
	$bot<privmsg>(to => $reply_to, text => "There's no game running currently. You can start one using \"?game\".");
      }
    }
  }
}

# Shows the developer, with not already guesssed chars blanked.
sub show_game(Str $to, Hash $game) {
  my $name    = $game<dev>;
  my $guesses = join "", $game<guesses>;

  $name ~~ s:Perl5:g:i/[^$guesses\- ]/_/;
  if $guesses {
    $bot<privmsg>(to => $to, text => "Guess! $name (You've already guessed $guesses)");
  } else {
    $bot<privmsg>(to => $to, text => "Guess! $name");
  }
}

# Returns true if the game is finished.
sub complete(Hash $game) returns Bool {
  my $name    = $game<dev>;
  my $guesses = join "", $game<guesses>;

  $name ~~ s:Perl5:g:i/[^$guesses\- ]/_/;
  return not($name ~~ m:Perl5/_/);
}
