#!/usr/bin/pugs

use v6;
use Test;

=head1 DESCRIPTION

This test tests the various filetest operators.

=cut

plan 34;

#if($*OS eq any<MSWin32 mingw msys cygwin>) {
#    skip 30, "file tests not fully available on win32";
#    exit;
#};

# Basic tests
ok -d 't',    "-d returns true on directories";
ok -f $*PROGRAM_NAME, "-f returns true on files";
ok -e $*PROGRAM_NAME, "-e returns true on files";
ok -e 't',    "-e returns true on directories";
ok -r $*PROGRAM_NAME, "-r returns true on readable files";
ok -w $*PROGRAM_NAME, "-w returns true on writable files";

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  skip 2, "win32 doesn't have -x";
} else {
  ok -x 'pugs', "-x returns true on executable files";
  ok -x 't',    "-x returns true on cwd()able directories";
}

ok !-f "t", "-f returns false on directories";
ok -r "t",  "-r returns true on a readable directory";

skip 2, "/etc/shadow tests skipped";
#if($*OS eq any<MSWin32 mingw msys cygwin>) {
#  skip 2, "win32 doesn't have /etc/shadow";
#} else {
#  ok !-r "/etc/shadow", "-r returns false on unreadable files";
#  ok !-w "/etc/shadow", "-w returns false on unwritable files";
#}

ok !-d 'doesnotexist', "-d returns false on non existant directories";
ok !-r 'doesnotexist', "-r returns false on non existant directories";
ok !-w 'doesnotexist', "-w returns false on non existant directories";
ok !-x 'doesnotexist', "-x returns false on non existant directories";
ok !-f 'doesnotexist', "-f returns false on non existant directories";

ok !-f 'doesnotexist.t', "-f returns false on non existant files";
ok !-r 'doesnotexist.t', "-r returns false on non existant files";
ok !-w 'doesnotexist.t', "-w returns false on non existant files";
ok !-x 'doesnotexist.t', "-x returns false on non existant files";
ok !-f 'doesnotexist.t', "-f returns false on non existant files";

#if($*OS eq any<MSWin32 mingw msys cygwin>) {
#  skip 1, "-s is not working on Win32 yet"
#}
#else {
  ok  -s $*PROGRAM_NAME > 42,   "-s returns size on existant files";
#}
ok !-s "doesnotexist.t", "-s returns undef on non existant files";

ok !-z $*PROGRAM_NAME,   "-z returns false on existant files";
ok !-z "doesnotexist.t", "-z returns false on non existant files";
ok !-z "t",              "-z returns false on directories";

my $fh = open("empty_file", :w);
close $fh;
#if($*OS eq any<MSWin32 mingw msys cygwin>) {
#  skip 1, "-z is not working on Win32 yet"
#}
#else {
  ok -z "empty_file",      "-z returns true for an empty file";
#}
unlink "empty_file";

# Stacked filetests
# L<A03/"RFC 320: Allow grouping of -X file tests and add filetest builtin">
ok -e -d -r "t",               "stacking of filetest operators (1)";
ok -e -f -r -w $*PROGRAM_NAME,         "stacking of filetest operators (2)";
ok !-e -f -r "doesnotexist.t", "stacking of filetest operators (3)";
# This one should return false *all the time* (-f and -d are mutually
# exclusive):
ok !-e -f -d "t",              "stacking of filetest operators (4-1)";
ok !-e -f -d "doesnotexist.t", "stacking of filetest operators (4-2)";
ok !-e -f -d "pugs",           "stacking of filetest operators (4-3)";
