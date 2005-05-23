#!/usr/bin/pugs

use v6;
use Test;

=kwid

= DESCRIPTION

Test that C< $*PID > in this process is different from
C< $*PID > in the child process.

=cut

plan 1;

my ($pugs,$redir,$squo) = ("./pugs", ">", "'");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
    $pugs = 'pugs.exe';
};

sub nonce () { return (".$*PID." ~ int rand 1000) }
my $tempfile = "temp-ex-output" ~ nonce;

my $command = qq!$pugs -e "say eval chr(36)~'PID'" $redir $tempfile!;
diag $command;
system $command;

my $child_pid = slurp $tempfile;
chomp $child_pid;
unlink $tempfile;

ok $*PID ne $child_pid, "My PID differs from the child pid ($*PID != $child_pid)";
