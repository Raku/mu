#!/usr/bin/perl

use FindBin;
use Config;
use File::Spec;

chdir (File::Spec->catdir($FindBin::Bin, File::Spec->updir));
my $pugs = File::Spec->catfile(File::Spec->curdir, "pugs$Config{_exe}");

system($pugs, -e => '"1..2\nok 1 # Welcome to Pugs!\n"');

open PUGS, "| $pugs" or die "Cannot pipe out to $pugs: $!";
print PUGS q("ok 2 # We've got fun and games!\n");
close PUGS;
