#!/usr/bin/perl

use FindBin;
use Config;
use File::Spec;

chdir (File::Spec->catdir($FindBin::Bin, File::Spec->updir));
my $pugs = File::Spec->catfile(File::Spec->curdir, "pugs$Config{_exe}");

system($pugs, -e => '"1..2\nok 1 # Welcome to Pugs!\n"');

open PUGS, "| $pugs" or die "Cannot pipe out to $pugs: $!";
print PUGS << '.';
    sub fine { "ok " ~ $_ };
    sub toys { "fun and games!\n" };
    sub cool { fine($_) ~ " # We've got " ~ toys };
    cool 2
.
close PUGS;
