#!/usr/bin/pugs

use v6;
use Test;

plan 3;

if ($*OS eq any<MSWin32 mingw msys cygwin browser>) {
    skip_rest;
    exit;
}

=pod

opendir/readdir support

=cut

my $dir = opendir('.');
isa_ok($dir, 'IO::Dir', "opendir worked");

my @files = readdir($dir);
ok(@files, "seems readdir worked too");

ok(closedir($dir), "as does closedir");
