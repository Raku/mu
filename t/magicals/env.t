#!/usr/bin/pugs

# Tests for magic variables

use v6;
use Test;

plan 11;

=kwid

= DESCRIPTION

Tests for %*ENV

Tests that C<%*ENV> can be read and written to and that
child processes see the modified C<%*ENV>.

=cut

# It must not be empty at startup.
ok +%*ENV.keys, '%*ENV has keys';

# %*ENV should be able to get copied into another variable.
my %vars = %*ENV;
is +%vars.keys, +%*ENV.keys, '%*ENV was successfully copied into another variable';

# XXX: Should modifying %vars affect the environment? I don't think so, but, of
# course, feel free to change the following test if I'm wrong.
%vars<PATH> = "42";
ok %*ENV<PATH> ne "42",
  'modifying a copy of %*ENV didn\'t affect the environment';

# Similarily, I don't think creating a new entry in %vars should affect the
# environment:
diag '%*ENV<PUGS_ROCKS>=' ~ %*ENV<PUGS_ROCKS>;
ok !defined(%*ENV<PUGS_ROCKS>), "there's no env variable 'PUGS_ROCKS'";
%vars<PUGS_ROCKS> = "42";
diag '%*ENV<PUGS_ROCKS>=' ~ %*ENV<PUGS_ROCKS>;
ok !defined(%*ENV<PUGS_ROCKS>), "there's still no env variable 'PUGS_ROCKS'";

my ($pugs,$redir,$squo) = ("./pugs", ">", "'");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
    $pugs = 'pugs.exe';
};

my $expected = 'Hello from subprocess';
%*ENV<PUGS_ROCKS> = $expected;
# Note that the "?" preceeding the "(" is necessary, because we need a Bool,
# not a junction of Bools.
is %*ENV<PUGS_ROCKS>, $expected,'%*ENV is rw';

my $tempfile = "temp-ex-output." ~ $*PID ~ "." ~ rand 1000;

my $command = qq!$pugs -e "\%*ENV.perl.say" $redir $tempfile!;
diag $command;
system $command;

my $child_env = slurp $tempfile;
my %child_env = eval $child_env;
unlink $tempfile;

my $err = 0;
for %*ENV.kv -> $k,$v {
  if (%child_env{$k} !~ $v) {
    if (! $err) {
      fail("Environment gets propagated to child.");
      $err++;
    };
    diag "Expected: $k=$v";
    diag "Got:      $k=%child_env{$k}";
  } else {
    # diag "$k=$v";
  };
};
if (! $err) {
  ok(1,"Environment gets propagated to child.");
};

%*ENV.delete('PUGS_ROCKS');
is(%*ENV<PUGS_ROCKS>,undef,'We can remove keys from %*ENV');

my $command = qq!$pugs -e "\%*ENV.perl.say" $redir $tempfile!;
diag $command;
system $command;

my $child_env = slurp $tempfile;
my %child_env = eval $child_env;
unlink $tempfile;

is(%child_env<PUGS_ROCKS>,undef,'The child did not see %*ENV<PUGS_ROCKS>');

my $err = 0;
for %*ENV.kv -> $k,$v {
  if (%child_env{$k} !~ $v) {
    if (! $err) {
      fail("Environment gets propagated to child.");
      $err++;
    };
    diag "Expected: $k=$v";
    diag "Got:      $k=%child_env{$k}";
  } else {
    # diag "$k=$v";
  };
};
if (! $err) {
  ok(1,"Environment gets propagated to child.");
};

ok !%*ENV.exists("does_not_exist"), "exists() returns false on a not defined env var";
