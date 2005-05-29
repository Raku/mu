#!/usr/bin/pugs

use v6;
use Test;

=pod

Test that the safemode is really safe.

=cut

sub nonce () { return (".$*PID." ~ int rand 1000) }

my $tmpfile = "temp-test" ~ nonce();
my @tests = (
    # Test that open() doesn't work.
    'my $fh = eval \'open "> ' ~ $tmpfile ~ '-opened"\'; eval \'close $fh\'',
    { $^a; not(-e "$tmpfile-opened") },
    
    # %*ENV, %?CONFIG, and $*OS should be hidden, too.
    'Pugs::Safe::safe_print("[%*ENV{}] [%?CONFIG{}] [$*OS]")',
    { $^a eq "[] [] []" },

    # The filetest operators shouldn't work, either.
    'Pugs::Safe::safe_print(eval("-d \'.\'").perl)',
    { $^a eq "undef" },
);

plan +@tests / 2;

diag "Running under $*OS";

my ($pugs,$redir) = ("./pugs", ">");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
  $redir = '>';
};

# Run our pugs childs in safemode
%*ENV<PUGS_SAFEMODE> = "true";

for @tests -> $code_to_run, $condition {
  state $i; $i++;

  {
      my $fh = open("> $tmpfile-src");
      say $fh: $code_to_run;
      close $fh;
  }

  my $command = "$pugs $tmpfile-src $redir $tmpfile-out";
  diag "Code to be run under safemode: $code_to_run";
  diag "Pugs will be started using:    $command";
  system $command;

  my $got     = slurp "$tmpfile-out";
  unlink map { "$tmpfile-$_" } <src out opened>;
  diag "The code wrote to STDOUT:      $got";

  ok $condition($got), "safemode works ($i)";
}
