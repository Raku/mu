#!/usr/bin/pugs
use v6;
use Test;

plan 3;

if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

my $pugs = "./pugs";
if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
};

my $res;

$res = system($pugs,'-e1');
ok($res,"system() to an existing program does not die (and returns something true)");

$res = system("program_that_does_not_exist_ignore_this_error_please.exe");
ok(!$res, "system() to a nonexisting program does not die (and returns something false)");

if $*OS ~~ any<cygwin MSWin32 msys> {
    skip 1, "skip crashing test on win32";
} else {
    $res = system("program_that_does_not_exist_ignore_errors_please.exe","a","b");
    ok(!$res, "system() to a nonexisting program with an argument list does not die (and returns something false)");
}
