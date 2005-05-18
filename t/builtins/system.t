#!/usr/bin/pugs
use v6;
use Test;

plan 3;

my $pugs = "./pugs";
if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
};

my $res;

$res = system($pugs,'-e1');
ok($res,"system() to an existing program does not die (and returns something true)");

$res = system("program_that_does_not_exist.exe");
ok(!$res, "system() to a nonexisting program does not die (and returns something false)");

$res = system("program_that_does_not_exist.exe","a","b");
ok(!$res, "system() to a nonexisting program with an argument list does not die (and returns something false)");
