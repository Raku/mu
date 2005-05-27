#!/usr/bin/pugs
# This is a small handy utility for screen(1) users.
# To "install" it, add the following lines to your ~/.screenrc:
#   hardstatus alwayslastline        # Always display the status line
#   hardstatus string "... [%40`]"   # The format of the status line
#   backtick 40 0 0 /path/to/screen-nodestatus.p6 computer1:C:10 router:R:20
#     # This runs screen-nodestatus.p6 with the args "computer1:C:10
#       router:R:20", meaning that screen-nodestatus.p6 will ping(1)
#       "computer1" every 10s and "router" every 20s. If the node was
#       reachable, "C" respectively "R" will be displayed, else "c"
#       respectively "r".
# This program will then output something like this:
#   ????      # ? -- Unknown status
#   ?*??      # * -- "Note! The status is going to change!"
#   ?D??      # Uppercase letter -- the host is on
#   ?D?*
#   ?D?t      # Lowercase letter -- the host is off
#   *D?t
#   AD?t
#   AD*t
#   ADMt
# But as screen(1) will only ever display the last line, you have a nice view
# on the status of your computers! :)
# Note: You might need to change &ping (at the end of this file).
# See a screenshot of screen-nodestatus.p6 in action at
# http://m19s28.vlinux.de/iblech/screen-nodestatus.png (look at the bottom
# right corner).

use v6;

# Mapping hostname => status letter.
my %letter;

# Mapping hostname => ping interval.
my %interval;

# Mapping hostname => last check timestamp.
my %last_check;

# Mapping hostname => current status
my ($ON, $OFF, $UNKNOWN, $CHANGE) = (0..3);
my %status;

# Additional callback which is called when the status of a computer changes.
my &on_change = -> Str $host, Int $new_status {
  system "beep", "-r2", "-l150", "-f", $new_status == $ON ?? "1700" :: "200"
    if $new_status == $ON or $new_status == $OFF;
};

# Parameter parsing.
for @*ARGS -> $arg {
  my &error = -> $err {
    die "While parsing the command line option \"$arg\":\n$err";
  };

  my ($hostname, $letter, $interval) = split ":", $arg;
  $letter .= uc;

  unless $hostname & $letter & $interval {
    error "You didn't specify the hostname, the status letter, or the interval.";
  }

  if defined %letter{$hostname} {
    error "Entry for host \"$hostname\" already defined.";
  }

  if chars $letter != 1 {
    error "The status letter isn't exactly one char long.";
  }

  # Needed because we use the shell to spawn ping(1) later on.
  if $hostname ~~ rx:Perl5/[ <>;&\\\$'"]/ { #"#--vim
    error "The hostname \"$hostname\" contains shell metacharacters.";
  }

  %letter    .{$hostname} = $letter;
  %interval  .{$hostname} = $interval;
  %status    .{$hostname} = $UNKNOWN;
  # We want a more uniformly distributed distribution.
  %last_check.{$hostname} = time - rand $interval;
}

die "No hostnames specified, look at the source of this program for usage help."
  unless %letter;

write_status();

loop {
  for sort keys %last_check -> $host {
    # Skip this host if we've already checked it.
    next if %last_check{$host} > time - %interval{$host};

    %last_check{$host} = time;
    # Update the status (maybe).
    write_if_change $host, { %status{$host} = ping($host) ?? $ON :: $OFF };
  }

  # We don't want to hog the CPU.
  sleep 1;
}

# Write the new status line iff the status of $host has changed after executing
# &callback.
sub write_if_change(Str $host, Code &callback) {
  my $old_status = %status{$host};
  callback();
  my $new_status = %status{$host};

  if $old_status != $new_status {
    # Temporarily mark $host as changing (*).
    %status{$host} = $CHANGE;
    # Write the status line.
    write_status();

    # Set the correct status, call &on_change, and write the correct status.
    %status{$host} = $new_status;
    on_change($host, $new_status);
    write_status();
  }
}

# Write the status line.
sub write_status() {
  for %letter.keys.sort:{ %letter{$^a} cmp %letter{$^b} } {
    my $status = %status{$_};
    my $char   = %letter{$_};

    print ({
      $ON      => uc $char,
      $OFF     => lc $char,
      $UNKNOWN => "?",
      $CHANGE  => "*",
    }{$status});
  }

  print "\n";
}

# Ping a host.
# Returns true/false.
sub ping(Str $host) returns Bool {
  # This line might need changing.
  # "Why don't you use the list form of system()?" -- Because I don't want to
  # redirect STDOUT and STDERR to /dev/null own my own right now.
  system "ping -q -w3 -c1 $host &>/dev/null";
  # -w3 -- Wait a maximum of 3s for a ICMP PING_REPLY.
  # -c1 -- Stop after at least one ICMP PING_REPLY is seen.
}
