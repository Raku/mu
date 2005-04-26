#!/usr/bin/pugs

use v6;
use Test;

=pod

Test that %ENV can be modified, and that
spawned processes see it.

=cut

push @INC, <  blib6/lib >; # ext/File-Spec/lib
require File::Spec;
plan 3;

my ($pugs) = "./pugs";
if($*OS eq any<MSWin32 mingw msys cygwin>) {
    $pugs = 'pugs.exe';
    skip 3, "setEnv is not implemented for Win32"; 
    exit;
};

sub run_pugs (Str $c) {
  my $tempname = "pugs-temp-output";
  # my $tempfile = catfile( tempdir(), $tempname ));
  my $tempfile = $tempname;

  my $command = qq!$pugs $c > "$tempfile"!;
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

my $key = "MODIFIEDENV";
my $val = "Test";

try {
  %*ENV{$key} = $val;
};

is($!, "", 'Modification of %*ENV raises no error');
is(%*ENV{$key}, $val, 'Modification of %*ENV works');

# Now check for the child process:

my $res = run_pugs( '-e "print %ENV{\'' ~$key ~ '\'} // \'undefined\'"');
is($res, $val, "Child processes see the new value");
