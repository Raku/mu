package PIL2JS;

use warnings;
use strict;

use FindBin;
use IPC::Open2;
use Config;
use File::Spec;

use base "Exporter";
our @EXPORT    = qw<
  compile_perl6_to_standalone_js
  compile_perl6_to_mini_js
  compile_perl6_to_htmljs_with_links
  compile_perl6_to_pil
  precomp_module_to_mini_js
  jsbin_hack
  run_pugs run_pil2js run_js
>;
our @EXPORT_OK = qw< pwd >;

sub pwd { File::Spec->catfile($FindBin::Bin, @_) }

our %cfg = (
  js        => "js",
  pugs      => pwd(qw< .. .. >, "pugs$Config{_exe}"),
  pil2js    => pwd('pil2js.pl'),
  preludepc => pwd('preludepc.js'),
  testpc    => pwd('Test.js'),
  prelude   => pwd(qw< lib6 Prelude JS.pm >),
);

sub diag($) { warn "# $_[0]\n" if $cfg{verbose} }

# bin/js's only output function is print, which is like Perl 6's &say, i.e.
# there's always a newline at the end. So our fake document.write outputs
# "string_to_output#IGNORE NEXT LINEFEED#\n", and we can s/#IGNORE NEXT
# LINEFEED#\n// later.
our $MAGIC_NOLF = "#IGNORE NEXT LINEFEED#";

sub preludepc_check {
  unless(-e $cfg{preludepc} and -s $cfg{preludepc}) {
    die "* Error: Precompiled Prelude (\"$cfg{preludepc}\") does not exist.\n";
  } elsif(-e $cfg{prelude} and -M $cfg{prelude} <= -M $cfg{preludepc}) {
    die "* Error: Your precompiled Prelude is outdated.\n";
  } elsif(not -e $cfg{prelude}) {
    warn "* Warning: Couldn't check whether your compiled Prelude is outdated.\n";
    warn "           Please run runjs.pl with an approproate --p6prelude option.\n";
  }
}

sub compile_perl6_to_standalone_js {
  preludepc_check();

  my $pil  = run_pugs("-CPIL", @_);
  die "Error: Couldn't compile to PIL!\n" if not defined $pil;
  my $mini = run_pil2js(\$pil);
  my $js   = run_pil2js("--link=js", jsprelude_path(), $cfg{preludepc}, $cfg{testpc}, \$mini);

  return $js;
}

sub compile_perl6_to_mini_js {
  my $pil = run_pugs("-CPIL", @_);
  die "Error: Couldn't compile to PIL!\n" if not defined $pil;
  my $js  = run_pil2js(\$pil);

  return $js;
}

sub compile_perl6_to_htmljs_with_links {
  preludepc_check();

  my $mini = compile_perl6_to_mini_js(@_);
  die "Error: Couldn't compile to PIL!\n" if not defined $mini;
  my $js   = run_pil2js("--link=html", "~".jsprelude_path(), "~$cfg{preludepc}", "~$cfg{testpc}", \$mini);

  return $js;
}

sub precomp_module_to_mini_js {
  my $pil = eval { run_pugs("-CPIL", @_, "-e", "''") };
  die $@ if $@;
  my $js  = eval { run_pil2js("-v", \$pil) };
  die $@ if $@;
  return $js;
}

sub compile_perl6_to_pil {
  my $pil = run_pugs("-CPIL", @_);
  die "Error: Couldn't compile to PIL!\n" if not defined $pil;
  return $pil;
}

sub run_pugs {
  my @args = @_;
  diag "$cfg{pugs} @args";

  $ENV{PERL5LIB} = join $Config{path_sep}, pwd('lib'), ($ENV{PERL5LIB} || "");
    
  open my $fh, "-|", "$cfg{pugs}", @args or
    warn "Couldn't open pipe to \"$cfg{pugs} @args\": $!\n" and return;
  local $/;
  my $res = <$fh>;
  return undef if not defined $res or length($res) == 0;
  close $fh or
    warn "Couldn't close pipe \"$cfg{pugs} @args\": $!\n" and return;

  return $res;
}

# Runs pil2js.pl. If there's a reference in @args, it will be substituted by
# "-" and the contents of the reference will be written to pil2js.pl's STDIN.
sub run_pil2js {
  my @args = @_;
  my $push;
  for(@args) {
    if(ref $_ and defined $push) {
      die "Only one reference argument may be given to &PIL2JS::run_pil2js!";
    } elsif(ref $_) {
      $push = $$_;
      $_ = "-";
    }
  }
  my @cmd = ($cfg{pil2js}, @args);
  diag "@cmd";

  my $pid = open2 my($read_fh), my($write_fh), @cmd
    or die "Couldn't open pipe to \"@cmd\": $!\n";

  print $write_fh $push or die "Couldn't write into pipe to \"@cmd\": $!\n"
    if defined $push;
  close $write_fh       or die "Couldn't close pipe to \"@cmd\": $!\n";

  local $/;
  my $ret = <$read_fh>;
}

sub run_js {
  my $js = shift;
  diag $cfg{js};

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

sub jsbin_hack {
  my $js = <<EOF . "\n" . $_[0];
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
var alert     = function (msg) { document.write("Alert: " + msg) };
EOF
}

sub jsprelude_path { pwd qw< libjs PIL2JS.js > }

1;
