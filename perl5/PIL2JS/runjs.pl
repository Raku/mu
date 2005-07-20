#!/usr/bin/perl

use warnings;
use strict;

use Getopt::Long;
use IPC::Open2;

my $psep = $^O eq "MSWin32" ? "\\" : "/";
my %cfg = (
  js        => "js",
  pugs      => $^O eq "MSWin32" ? ".${psep}pugs.exe" : ".${psep}pugs",
  pil2js    => "perl5${psep}PIL2JS${psep}pil2js.pl",
  preludepc => "perl5${psep}PIL2JS${psep}preludepc.js",
  prelude   => "perl5${psep}PIL2JS${psep}lib6${psep}Prelude${psep}JS.pm",
);

GetOptions(
  "js=s"          => \$cfg{js},
  "pugs=s"        => \$cfg{pugs},
  "pil2js=s"      => \$cfg{pil2js},
  "p6preludepc=s" => \$cfg{preludepc},
  "p6prelude=s"   => \$cfg{prelude},
  "help"          => \&usage,
) or usage();

# bin/js's only output function is print, which is like Perl 6's &say, i.e.
# there's always a newline at the end. So our fake document.write outputs
# "string_to_output#IGNORE NEXT LINEFEED#\n", and we can s/#IGNORE NEXT
# LINEFEED#\n// later.
my $MAGIC_NOLF = "#IGNORE NEXT LINEFEED#";

my $js = <<EOF . "\n" . compile(@ARGV);
// Stubs inserted by runjs.pl.
var document = {
  write: function (str) {
    print(str + "$MAGIC_NOLF");
  },
  body:  {},
  getElementById: function (id) {}
};

var window    = { scrollTo: function (x, y) {} };
var navigator = { userAgent: undefined };
EOF

js($js);

sub compile {
  my $pathsep = $^O eq "MSWin32" ? "\\" : "/";
  unless(-e $cfg{preludepc} and -s $cfg{preludepc}) {
    die "* Error: Precompiled Prelude (\"$cfg{preludepc}\") does not exist.\n";
  } elsif(-e $cfg{prelude} and -M $cfg{prelude} <= -M $cfg{preludepc}) {
    die "* Error: Your precompiled Prelude is outdated.\n";
  } elsif(not -e $cfg{prelude}) {
    warn "* Warning: Couldn't check whether your compiled Prelude is outdated.\n";
    warn "           Please run runjs.pl with an approproate --p6prelude option.\n";
  }

  my $pil = pugs("-CPIL", @_);
  die "Error: Couldn't compile to PIL!\n" if not defined $pil;
  my $js  = pil2js(
    $pil,
    "--jsprelude-mode=inline", "--p6preludepc-mode=inline",
    "--p6preludepc-path=$cfg{preludepc}",
  ) or die "Error: Couldn't compile to JavaScript!\n";

  return $js;
}

sub pugs {
  my @args = @_;

  $ENV{PERL5LIB} = join $^O eq "MSWin32" ? ";" : ":",
    "perl5${psep}PIL2JS${psep}lib",
    $ENV{PERL5LIB} || "";
    
  open my $fh, "-|", "$cfg{pugs}", @args or
    warn "Couldn't open pipe to \"$cfg{pugs} @args\": $!\n" and return;
  local $/;
  my $res = <$fh>;
  return undef if not defined $res or length($res) == 0;
  close $fh or
    warn "Couldn't close pipe \"$cfg{pugs} @args\": $!\n" and return;

  return $res;
}

sub pil2js {
  my ($pil, $to, @args) = @_;
  my @cmd = ($cfg{pil2js}, @args);

  my $pid = open2 my($read_fh), my($write_fh), @cmd
    or die "Couldn't open pipe to \"@cmd\": $!\n";

  print $write_fh $pil or die "Couldn't write into pipe to \"@cmd\": $!\n";
  close $write_fh      or die "Couldn't close pipe to \"@cmd\": $!\n";

  local $/;
  my $js = <$read_fh>;
}

sub js {
  my $js = shift;

  my $pid = open2 my($read_fh), my($write_fh), $cfg{js}
    or die "Couldn't open pipe to \"$cfg{js}\": $!\n";

  print $write_fh $js or die "Couldn't write into pipe to \"$cfg{js}\": $!\n";
  close $write_fh     or die "Couldn't close pipe to \"$cfg{js}\": $!\n";

  $|++;
  while(defined(my $line = <$read_fh>)) {
    $line =~ s/\Q$MAGIC_NOLF\E\n//g;
    print $line;
  }
}

sub usage { print STDERR <<EOF; exit }
$0 -- Compiles Perl 6 to JavaScript and runs it.
Usage: $0 [options] -- regular_pugs_options

Available options are:
  --js=/path/to/js/interpreter
  --pugs=/path/to/pugs
  --pil2js=/path/to/pil2js.pl               (usually in perl5/PIL2JS/)
  --p6prelude=/path/to/lib6/Prelude/JS.pm   (usually in perl5/PIL2JS/lib6/)
  --p6preludepc=/path/to/preludepc.js       (compile using
                                            perl5/PIL2JS/jspugs.pl)
  --help
EOF
