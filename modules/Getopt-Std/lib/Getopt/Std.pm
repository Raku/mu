module Getopt::Std-1.05;
use v6;

=head1 NAME

getopt, getopts - Process single-character switches with switch clustering

=head1 SYNOPSIS

    use Getopt::Std;

    my %opts = getopt 'oDI';    # -o, -D & -I take arg.  Values in %opts
    my %opts = getopts 'oif:';  # -o & -i are boolean flags, -f takes an argument
                                # Values in %opts

=head1 DESCRIPTION

The getopt() function processes single-character switches with switch
clustering.  Pass one argument which is a string containing all switches
that take an argument.  Switches which take an argument don't care whether
there is a space between the switch and the argument.

The getopts() function is similar, but you should pass to it the list of all
switches to be recognized.  If unspecified switches are found on the
command-line, the user will be warned that an unknown option was given.

To allow programs to process arguments that look like switches, but aren't,
both functions will stop processing switches when they see the argument
C<-->.  The C<--> will be removed from @ARGV.

=head1 C<--help> and C<--version>

If C<-> is not a recognized switch letter, getopts() supports arguments
C<--help> and C<--version>.  If C<main::HELP_MESSAGE()> and/or
C<main::VERSION_MESSAGE()> are defined, they are called; the arguments are
the output file handle, the name of option-processing package, its version,
and the switches string.  If the subroutines are not defined, an attempt is
made to generate intelligent messages; for best results, define $main::VERSION.

If embedded documentation (in pod format, see L<perlpod>) is detected
in the script, C<--help> will also show how to access the documentation.

=cut

# Process single-character switches with switch clustering.  Pass one argument
# which is a string containing all switches that take an argument.  For each
# switch found, sets $opt_x (where x is the switch name) to the value of the
# argument, or 1 if no argument.  Switches which take an argument don't care
# whether there is a space between the switch and the argument.

# Usage:
#	getopt('oDI');  # -o, -D & -I take arg.  Sets opt_* as a side effect.

sub getopt(Str $argumentative) is export {
    my %hash;
    $argumentative //= "";

    while @*ARGV and ($_ = @ARGV[0]) ~~ /^-(.)(.*)/ {
      my ($first, $rest) = ($1, $2);

      if /^--$/ {	# early exit if --
	  shift @*ARGV;
	  last;
      }

      if index($argumentative, $first) >= 0 {
	if $rest ne '' {
	  shift @*ARGV;
	} else {
	  shift @*ARGV;
	  $rest = shift @*ARGV;
	}

	%hash{$first} = $rest;
      } else {
	%hash{$first} = 1;
	if $rest ne "" {
	  @ARGV[0] = "-$rest";
	} else {
	  shift @*ARGV;
	}
      }
    }
  }

  return %hash;
}


sub version_mess(Str $args) {
  my $v = eval '$*::VERSION';
  $v  //= "[unknown]";
  say $*ERR: "$*PROGRAM_NAME version $v calling Getopt::Std::getopts."
}

sub help_mess(Str $args) {
  my (@witharg) = ($args ~~ m:g/(\S)\s*:/);
  my (@rest)    = ($args ~~ m:g/(<-<space>-[:]>)(?!\s*:)/); # XXX - (?:)
  my ($help, $arg) = ('', '');

  if @witharg {
    $help ~= "\n\tWith arguments: -" ~ join " -", @witharg;
    $arg = "\nSpace is not required between options and their arguments.";
  }

  if @rest {
    $help ~= "\n\tBoolean (without arguments): -" ~ join " -", @rest;
  }

  my ($scr) = ($*PROGRAM_NAME ~~ m,(<-[/\\]>+)$,);
  print $h: qq:to/EOH/

Usage: $scr [-OPTIONS [-MORE_OPTIONS]] [--] [PROGRAM_ARG1 ...]
EOH

The following single-character options are accepted:$help

Options may be merged together.  -- stops processing of options.$arg
EOH

  my $has_pod = true $=HEAD1; # XXX -- correct?

  print $h: qq:to/EOH/ if $has_pod;

For more details run
	perldoc -F $*EXECUTABLE_NAME
EOH
}

# Usage:
#   getopts('a:bc');	# -a takes arg. -b & -c not. Sets opt_* as a
#			#  side effect.
sub getopts(Str $argumentative) {
  my (@args, %hash, $exit);
  my $errs = 0;

  @args = split m/ */, $argumentative;
  while @*ARGV and ($_ = @ARGV[0]) ~~ m/^-(.)(.*)/) {
    my ($first, $rest) = ($1, $2);

    if (/^--$/) {	# early exit if --
      shift @*ARGV;
      last;
    }

    my $pos = index $argumentative, $first;
    if $pos >= 0 {
      if defined @args[$pos+1] and @args[$pos+1] eq ':' {
	shift @*ARGV;
	if $rest eq "" {
	  ++$errs unless @*ARGV;
	  $rest = shift @*ARGV;
	}

	%hash{$first} = $rest;
      } else {
	%hash{$first} = 1;
	if $rest eq "" {
	  shift @*ARGV;
	} else {
	  @ARGV[0] = "-$rest";
	}
      }
    } else {
      if $first eq "-" and $rest eq "help" {
	version_mess $argumentative;
	help_mess $argumentative;
	exit 0; # XXX - why?
	shift @*ARGV;
	next;
      } elsif $first eq "-" and $rest eq "version" {
	version_mess $argumentative;
	exit 0; # XXX - why?
	shift @*ARGV;
	next;
      }

      warn "Unknown option: $first\n";
      ++$errs;
      if $rest ne "" {
	@ARGV[0] = "-$rest";
      } else {
	shift @*ARGV;
      }
    }
  }

  return %hash;
}

1;
