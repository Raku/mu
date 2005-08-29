#!/usr/bin/perl

use warnings;
use strict;

use FindBin;
use File::Spec;
use lib File::Spec->catfile($FindBin::Bin, "lib");
use PIL2JS;
use Getopt::Long;

# Minor hack
INIT {
  if($ENV{PIL2JS_RESOURCE_GUARD}) {
    require BSD::Resource;
    import BSD::Resource;
    setrlimit(RLIMIT_CPU(), 43, 45) or die "Couldn't setrlimit: $!\n";
    warn "*** Limited CPU resources.\n";
  }
}

sub pwd { File::Spec->catfile($FindBin::Bin, @_) }

my (@runjs_args, @pugs_args);
while (@ARGV) {
    $ARGV[0] =~ /^--/ or last;
    push @runjs_args, shift(@ARGV);
}
@pugs_args = @ARGV;
@ARGV = @runjs_args;

GetOptions(
  "js=s"          => \$PIL2JS::cfg{js},
  "pugs=s"        => \$PIL2JS::cfg{pugs},
  "pil2js=s"      => \$PIL2JS::cfg{pil2js},
  "p6preludepc=s" => \$PIL2JS::cfg{preludepc},
  "p6prelude=s"   => \$PIL2JS::cfg{prelude},
  "testpc=s"      => \$PIL2JS::cfg{testpc},
  "help"          => \&usage,
) and @pugs_args or usage();

unless(-e $PIL2JS::cfg{preludepc}) {
  warn "*** Precompiled Prelude doesn't exist yet; precompiling...\n";
  my $js = precomp_module_to_mini_js "-I", PIL2JS::pwd("lib6"), "-MPrelude::JS";
  write_file($js => $PIL2JS::cfg{preludepc});
}

unless(-e $PIL2JS::cfg{testpc}) {
  warn "*** Precompiled Test.pm doesn't exist yet; precompiling...\n";
  my $js = precomp_module_to_mini_js "-MTest";
  write_file($js => $PIL2JS::cfg{testpc});
}

my $js = jsbin_hack(compile_perl6_to_standalone_js(
  # "-I" . PIL2JS::pwd("lib6"),
  # "-MMarkTestPMAsLoaded",
  # "-e", 'BEGIN { %*INC{"Test.pm"}++ }',
  @pugs_args
));
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
  --pil2js=/path/to/pil2js.pl               (usually in perl5/PIL2JS/)
  --p6prelude=/path/to/lib6/Prelude/JS.pm   (usually in perl5/PIL2JS/lib6/)
  --p6preludepc=/path/to/preludepc.js       (compile using
                                            perl5/PIL2JS/jspugs.pl)
  --help
EOF
