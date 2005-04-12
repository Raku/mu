#!/usr/bin/pugs

use v6;
require Test;

=pod

Test that the C<--version> command in its various incantations
works.

=cut

my @tests = any< -v --version >;
@tests = map { $_.values }
         map { $_, "-w $_", "$_ -w", "-w $_ -w" }
         @tests;

plan +@tests;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");

if($?OS eq any<MSWin32 mingw cygwin>) {
  $pugs = 'pugs.exe';
};

for @tests -> $ex {
  my $command = "$pugs $ex $redir temp-ex-output";
  diag $command;
  system $command;

  my $got = slurp "temp-ex-output";
  unlink "temp-ex-output";

  like($got, rx:perl5/Version: 6\.0\.\d+/, "'$ex' displays version");
};
