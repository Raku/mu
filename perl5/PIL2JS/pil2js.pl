#!/usr/bin/perl
# This is the frontend for the modules PIL::Parser and PIL::Nodes.
# See sub &usage at the bottom for usage information.

use warnings;
use strict;
use lib "lib";

use Getopt::Long;
use PIL::Parser;
use PIL::Nodes;

local ($/, $_);

sub slurp($) {
  open my $fh, "<", $_[0] or die "Couldn't open \"$_[0]\" for reading: $!\n";
  local $/;
  return <$fh>;
}

sub guess_jsprelude_path {
  my $pathsep = $^O eq "MSWin32" ? "\\" : "/";

  my $path =  $INC{"PIL${pathsep}Nodes.pm"};
  $path    =~ s[PIL${pathsep}Nodes\.pm][..${pathsep}libjs${pathsep}PIL2JS.js];

  return -f $path ? $path : undef;
}

my %jsprelude = (mode => "inline", path => guess_jsprelude_path());
my %p6prelude = (mode => "link");

GetOptions(
  "verbose"            => \my $verbose,
  "html"               => \my $html,
  "jsprelude-mode=s"   => \$jsprelude{mode},
  "p6preludepc-mode=s" => \$p6prelude{mode},
  "jsprelude-path=s"   => \$jsprelude{path},
  "p6preludepc-path=s" => \$p6prelude{path},
  "yaml-dump"          => \my $yaml_dump,
  "help"               => \&usage,
) or usage();

usage() unless
  $jsprelude{mode} =~ /^(?:no|inline|link)$/ and
  $p6prelude{mode} =~ /^(?:no|inline|link)$/;

unless($yaml_dump) {
  die <<ERR if not $html and ($jsprelude{mode} eq "link" or $p6prelude{mode} eq "link");
*** Can't link to Prelude if --html option not given!
ERR

  die <<ERR if not $p6prelude{path} and ($p6prelude{mode} =~ /^(?:inline|link)$/);
*** Can't inline or link to the precompiled Prelude,
    as no path to the Prelude was given.
ERR

  die <<ERR if not $jsprelude{path} and ($jsprelude{mode} =~ /^(?:inline|link)$/);
*** Can't inline or link to the JavaScript Prelude (PIL2JS.js),
    as no path to the Prelude was given.
ERR
}

warn "*** Reading PIL...\n" if $verbose;

my $pil  = <>;
my $tree = PIL::Parser->parse($pil);

if($yaml_dump) {
  require YAML;
  print YAML::Dump($tree);
  exit;
}

warn "*** Compiling input PIL to JavaScript...\n" if $verbose;
my $load_check = <<EOF;
try { PIL2JS } catch(err) {
  var error = new Error("PIL2JS.js not loaded; aborting.");
  alert(error);
  throw(error);
}
EOF

my $program_js = join "\n",
  $load_check,
  map { $_->as_js } @{ $tree->{"pilGlob"} }, $tree->{pilMain};

warn "*** Inlining the JavaScript Prelude (PIL2JS.pl)...\n" if $verbose;
my $jsprelude_js = $jsprelude{mode} eq "inline" ? slurp $jsprelude{path} : undef;
warn "*** Inlining the Perl 6 Prelude...\n" if $verbose;
my $p6prelude_js = $p6prelude{mode} eq "inline" ? slurp $p6prelude{path} : undef;
$p6prelude_js =
  "// This is the part of the Prelude written in Perl 6.\n$p6prelude_js"
    if $p6prelude_js;

my $js;
$js .= "$jsprelude_js\n" if $jsprelude{mode} eq "inline" and not $html;
$js .= "$p6prelude_js\n" if $p6prelude{mode} eq "inline";
$js .= $program_js;

unless($html) {
  print $js;
} else {
  # Output a standard HTML skeleton.
  my $indent = sub { join "\n", map { " " x 6 . $_ } split "\n", shift };
  my $link   = sub { "<script type=\"text/javascript\" src=\"$_[0]\"></script>" };

  print <<EOF;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>PIL2JS</title>
  </head>

  <body>
    <pre id="__pil2js_tty"></pre>

EOF

  print <<EOF if $jsprelude{mode} eq "inline";
    <script type="text/javascript">//<![CDATA[
@{[$indent->($jsprelude_js)]}
      //]]
    </script>
EOF

  print <<EOF;
    @{[ $jsprelude{mode} eq "link" ? $link->($jsprelude{path}) : "" ]}
    @{[ $p6prelude{mode} eq "link" ? $link->($p6prelude{path}) : "" ]}
    <script type="text/javascript">//<![CDATA[
@{[$indent->($js)]}
      //]]
    </script>
  </body>
</html>
EOF
}

sub usage { print STDERR <<USAGE; exit }
pil2js.pl compiles PIL code given in STDIN to JavaScript code, outputted to
STDOUT.

Available options (options may be abbreviated to uniqueness):
  --verbose               Be verbose.
  --html                  Output the JavaScript packed in an HTML page.

  --jsprelude-mode=no|inline|link
  --p6preludepc-mode=no|inline|link
                          Don't include, inline, or link to the specified part
                          of the Prelude.

  --jsprelude-path=...
  --p6preludepc-path=...
                          Sets the path to the specified part of the Prelude.

  --yaml-dump             Only output the PIL as YAML; don't compile anything.

Recommended usage:
  \$ cd perl5/PIL2JS
  \$ pugs -CPIL -Ilib6 -MPrelude::JS -we 'say 2 + 3' | \
    ./pil2js.pl --html --p6preludepc-mode=no > test.js
  # or (*much* faster)
  \$ pugs -CPIL -Ilib6 -MPrelude::JS -we '' | \
    ./pil2js.pl --jsprelude-mode=no --p6preludepc-mode=no > preludepc.js
  \$ pugs -CPIL -we 'say 2 + 3' | \
    ./pil2js.pl --html --p6preludepc="http://.../preludepc.js" > test.js

USAGE
