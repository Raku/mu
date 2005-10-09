#!/usr/bin/perl

use warnings;
use strict;

use FindBin;
use File::Spec;
use lib File::Spec->catdir($FindBin::Bin, "lib");
use lib File::Spec->catdir($FindBin::Bin);
BEGIN {
  eval { require Class::Rebless };
  if($@) {
    warn <<MSG;
*** PIL2JS needs the Class::Rebless module from CPAN.

MSG
    exit 1;
  }
}
use PIL2JS;
use Getopt::Long;

# Minor hack
INIT {
  if($ENV{PIL2JS_RESOURCE_GUARD}) {
    require BSD::Resource;
    import BSD::Resource;
    setrlimit(RLIMIT_CPU(), 63, 67) or die "Couldn't setrlimit: $!\n";
    warn "*** Limited CPU resources.\n";
  }
}

sub pwd { File::Spec->catfile($FindBin::Bin, @_) }

my (@runjs_args, @pugs_args);
{
  while(@ARGV) {
    my $arg = shift @ARGV;

    # Ignore -B JS and -BJS
    if(uc $arg eq "-B") {
      shift @ARGV;
      next;
    } elsif(uc $arg eq "-BJS") {
      next;
    } elsif($arg eq "--") {
      push @pugs_args, splice @ARGV;
    } elsif($arg =~ /^--/) {  # treat all --options as belonging to runjs
      push @runjs_args, $arg;
    } else {
      push @pugs_args, $arg;
    }
  }

  @ARGV = @runjs_args;
}

GetOptions(
  "js=s"          => \$PIL2JS::cfg{js},
  "pugs=s"        => \$PIL2JS::cfg{pugs},
  "pil2js=s"      => \$PIL2JS::cfg{pil2js},
  "p6preludepc=s" => \$PIL2JS::cfg{preludepc},
  "p6prelude=s"   => \$PIL2JS::cfg{prelude},
  "testpc=s"      => \$PIL2JS::cfg{testpc},
  "metamodel-base=s" => \$PIL2JS::cfg{metamodel_base},
  "compile-only"     => \my $compile_only,
  "precompile-only"  => \my $precompile_only,
  "help"             => \&usage,
) and @pugs_args or usage();

unless(-e $PIL2JS::cfg{preludepc} and -s $PIL2JS::cfg{preludepc}) {
  warn << '.';
*** Precompiled Prelude doesn't exist yet; precompiling...
    (You can safely ignore the 'useless use of constant' warnings.)
.
  my $js = precomp_module_to_mini_js "-I", PIL2JS::pwd("lib6"), "-MPrelude::JS";
  write_file($js => $PIL2JS::cfg{preludepc});
}

unless(-e $PIL2JS::cfg{testpc} and -s $PIL2JS::cfg{testpc}) {
  warn << '.';
*** Precompiled Test.pm doesn't exist yet; precompiling...
    (You can safely ignore the 'useless use of constant' warnings.)
.
  my $js = precomp_module_to_mini_js "-MTest";
  write_file($js => $PIL2JS::cfg{testpc});
}

exit if $precompile_only;

my $js = jsbin_hack(compile_perl6_to_standalone_js(
  # "-I" . PIL2JS::pwd("lib6"),
  # "-MMarkTestPMAsLoaded",
  # "-e", 'BEGIN { %*INC{"Test.pm"}++ }',
  @pugs_args
));
print($js), exit if $compile_only;
run_js($js);
#run_js_on_jssm($js);

sub write_file {
  my ($contents, $file) = @_;

  open my $fh, ">", $file or die "Couldn't open \"$file\" for writing: $!\n";
  print $fh $contents     or die "Couldn't write to \"$file\": $!\n";
  close $fh               or die "Couldn't close \"$file\": $!\n";
}

sub usage { print STDERR <<EOF; exit }
$0 -- Compiles Perl 6 to JavaScript and runs it.
Usage: $0 [options] regular_pugs_options

Available options are:
  --js=/path/to/js/interpreter
  --pugs=/path/to/pugs
  --pil2js=/path/to/pil2js.pl              (usually in perl5/PIL2JS/)
  --p6prelude=/path/to/lib6/Prelude/JS.pm  (usually in perl5/PIL2JS/lib6/)
  --metamodel-base=...                     (usually perl5/PIL2JS/libjs/)
  --p6preludepc=/path/to/preludepc.js      (automatically created)
  --testpc=/path/to/test.js                (automatically created)
  --compile-only                           (outputs the resulting JS to STDOUT)
  --precompile-only
  --help
EOF
