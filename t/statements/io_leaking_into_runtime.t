#!/usr/bin/pugs

use v6;
use Test;

sub nonce () { return (".$*PID." ~ int rand 1000) }

my $ignore_errors = q[
  BEGIN {
    say $*ERR:
      "Please ignore any errors this test outputs regarding BEGIN and CHECK blocks.\n" ~
      "The error messages are expected and tested for.";
  }
];

my $tmpfile = "temp-test" ~ nonce();
my @tests = (
    # Test that "open 'README'" works
    'my $fh = open "README"; print($fh ?? "ok" !! "nok")',
    { $^a eq "ok" },
    
    # Test that BEGIN { open 'README' } does *not* work
    $ignore_errors ~ 'my $fh = BEGIN { open "README" }; print "_nok_"',
    { $^a !~ rx:P5/_nok_/ },

    # Test that CHECK { open 'README' } does *not* work
    $ignore_errors ~ 'my $fh = CHECK { open "README" }; print "_nok_"',
    { $^a !~ rx:P5/_nok_/ },

    # Test that INIT { open 'README' } *does* work
    'my $fh = INIT { open "README" }; print($fh ?? "ok" !! "nok")',
    { $^a eq "ok" },
);

plan +@tests / 2;

diag "Running under $*OS";

my ($pugs,$redir) = ("./pugs", ">");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
  $redir = '>';
};

for @tests -> $code_to_run, $condition {
  state $i; $i++;

  {
      my $fh = open("$tmpfile-src", :w);
      say $fh: $code_to_run;
      close $fh;
  }

  my $command = "$pugs $tmpfile-src $redir $tmpfile-out";
  diag "Code to be run:\n  $code_to_run";
  diag "Pugs will be started using:\n  $command";
  system $command;

  my $got     = slurp "$tmpfile-out";
  unlink map { "$tmpfile-$_" } <src out opened>;
  diag "The code wrote to STDOUT:\n  $got";

  ok $condition($got), "IO handles created at compile-time may not leak into runtime ($i)";
}
