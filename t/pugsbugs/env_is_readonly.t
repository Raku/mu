#!/usr/bin/pugs

use v6;
require Test;

=pod

Test that %ENV can be modified, and that
spawned processes see it.

=cut

push @INC, <  blib6/lib >; # ext/File-Spec/lib
require File::Spec;
plan 3;

my ($pugs) = "./pugs";
if($?OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
  
  skip 3, "setEnv is not implemented for Win32"; # unTODOme
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

todo_is($!, "", 'Modification of %*ENV raises no error');
todo_is(%*ENV{$key}, $val, 'Modification of %*ENV works');

# Now check for the child process:

my $res = run_pugs( '-e "say %ENV{\'' ~$key ~ '\'} // \'undefined\'"');
todo_is($res, $val, "Child processes see the new value");
