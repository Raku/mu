#!/usr/bin/perl

use warnings;
use strict;

use Term::ReadLine;

my $term = Term::ReadLine->new("jspugs");

my $prompt = "jspugs> ";
my $OUT = $term->OUT || \*STDOUT;

print $OUT <<EOF;
Welcome to JSPugs -- Perl6 User's Golfing System
Type :h for help.

EOF

my %cfg = (
  pugs      => $^O eq "MSWin32" ? "pugs.exe" : "pugs",
  pil2js    => "pil2js.pl",
  preludepc => "preludepc.js",
  lib6      => "lib6",
  output    => "output.html",
);

# "Autodetection".
$cfg{pugs} = ($^O eq "MSWin32" ? "..\\..\\" : "../../") . $cfg{pugs}
  unless -e $cfg{pugs};
$cfg{pil2js} = ($^O eq "MSWin32" ? ".\\" : "./") . $cfg{pil2js}
  if -e $cfg{pil2js};
$cfg{lib6} = ($^O eq "MSWin32" ? "perl5\\PIL2JS\\lib6" : "perl5/PIL2JS/lib6")
  unless -e $cfg{lib6};

command_conf(pugs      => $cfg{pugs});
command_conf(pil2js    => $cfg{pil2js});
command_conf(preludepc => $cfg{preludepc});
command_conf(lib6      => $cfg{lib6});
command_conf(output    => $cfg{output});

while(defined($_ = $term->readline($prompt))) {
  $term->addhistory($_) if /\S/;

  if(my ($cmd, $arg) = /^:([hq]|pil(?:\.yaml)|conf|precomp)\s*(.*?)\s*$/) {
    no strict "refs";
    $cmd =~ s/\./_/g;
    &{"command_$cmd"}(split /\s+/, $arg);
  } else {
    command_compile($_);
  }
}

sub command_q { exit }

sub command_h { print $OUT <<USAGE }
Commands available from the prompt:
:h                      = show this help message
:q                      = quit
:conf thing [new_value] = set the path to thing
                          (pugs|pil2js|preludepc|lib6|output)
:precomp                = precompile the Prelude
:pil <exp>              = show PIL of expression
:pil.yaml <exp>         = show PIL of expression dumped as YAML
<exp>                   = compile expression
USAGE

sub command_conf {
  my ($what, $new) = @_;

  my %human = (
    pugs      => "pugs",
    pil2js    => "pil2js",
    preludepc => "the precompiled Prelude",
    lib6      => "PIL2JS's lib6/",
    output    => "the JavaScript output",
  );

  unless($what and $what =~ /^(?:pugs|pil2js|preludepc|lib6|output)$/) {
    print $OUT "Usage: :conf pugs|pil2js|preludepc|lib6|output [new_value]\n";
    return;
  }

  $cfg{$what} = $new if $new;

  my $descr = "$human{$what}:" . " " x (30 - length $human{$what});
  my $path  = $cfg{$what}      . " " x (20 - length $cfg{$what});
  print $OUT
    "* Path to $descr $path [" .
    (-d $cfg{$what} || (-f $cfg{$what} && -s $cfg{$what}) ? "OK" : "NOT FOUND") .
    "]\n";
}

sub command_precomp {
  my $pil = pugs("-CPIL", "-I$cfg{lib6}", "-MPrelude::JS", "-e", "''");
  print $OUT "Error: Couldn't compile Prelude::JS to PIL!\n" and return if not defined $pil;
  pil2js($pil, $cfg{preludepc}, "--no-jsprelude") or
    print $OUT "Error: Couldn't compile Prelude::JS to JavaScript!\n";
  command_conf("preludepc");
}

sub command_pil {
  my $pil = pugs("-CPIL", "-I$cfg{lib6}", "-e", $_[0]);
  print $OUT "Error: Couldn't compile to PIL!\n" and return if not defined $pil;
  print $OUT $pil;
}

sub command_pil_yaml {
  my $pil = pugs("-CPIL", "-I$cfg{lib6}", "-e", $_[0]);
  print $OUT "Error: Couldn't compile to PIL!\n" and return if not defined $pil;
  pil2js($pil, undef, "--yaml-dump") or
    print $OUT "Error: Couldn't dump PIL as YAML!\n";
}

sub command_compile {
  unless(-e $cfg{preludepc} and -s $cfg{preludepc}) {
    print $OUT "* Warning: Precompiled Prelude (\"$cfg{preludepc}\") does not exist,\n";
    print $OUT "           use the command \":precomp\" to compile the Prelude.\n";
  }

  my $pil = pugs("-CPIL", "-Ilib6", "-e", $_[0]);
  print $OUT "Error: Couldn't compile to PIL!\n" and return if not defined $pil;
  pil2js($pil, $cfg{output}, "--html", "--preludepc", $cfg{preludepc}) or
    print $OUT "Error: Couldn't compile to JavaScript!\n" and return;
  command_conf("output");
}

sub pugs {
  my @args = @_;

  diag("$cfg{pugs} @args");
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

  my $cmd = "$cfg{pil2js} @args" . (defined $to ? " > $to" : "");
  diag($cmd);
  open my $fh, "|-", "$cmd" or
    warn "Couldn't open pipe to \"$cmd\": $!\n" and return;
  print $fh $pil or
    warn "Couldn't write into pipe to \"$cmd\": $!\n" and return;
  close $fh or
    warn "Couldn't close pipe to \"$cmd\"!\n" and return;
}

sub diag { print $OUT "# $_[0]\n" }
