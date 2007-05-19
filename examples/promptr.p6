#!/usr/bin/env pugs
use v6;

## Author: Austin Seipp, 2007


#Commands our shell can execute
my %cmds  = (
	     ":about"   => ("about this program",&cmd_about),
	     ":history" => ("gives command history",&cmd_history),
	     ":last"    => ("last command given",&cmd_last),
	     ":help"    => ("this command",&cmd_help),
	     ":quit"    => ("exits prompt",&cmd_quit),
);

#info used by the commands, used for various purposes
my @history = ();
my %stats   = (
	     "count"    => 1,
	     "lastexec" => "nil",
 	     "history"  => @history,
);


#main loop
loop {
  print %stats<count>++ ~ ") ";
  my $in = =$*IN;
  %stats<history>.push($in);
  my ($cmd,@ops) = split " ",$in;
  %stats<lastexec> = ($cmd eq ":last") ?? %stats<lastexec> !! $in;
  if ($cmd eq any(%cmds.keys)) == True {
    for %cmds.keys -> $command {
      if $cmd eq $command {
	%cmds{$command}[1](@ops);
	last;
      }
    }
  } else {
    say "err: invalid command, try :help for help.";
  }
}




#these are our command functions
sub cmd_about {
  "promptr: perl 6 prompt".say
}


sub cmd_history {
  for (1..%stats<count>-1) -> $i {
    say "  " ~ $i ~ ") " ~ %stats<history>[$i-1];
  }
}

sub cmd_last {
  say %stats<lastexec>;
}

sub cmd_help {
  for %cmds.kv -> ($cmd,@values) {
    say " " ~ $cmd ~ "\t" ~ (((split "",$cmd).elems < 8) ?? "\t" !! "") ~ @values[0];
  }
}

sub cmd_quit {
  "Leaving promptr...".say;
  exit;
}
