#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;

my $base = shift || Cwd::cwd();
my $version_h = "$base/src/pugs_version.h";
my $svn_entries = ".svn/entries";

open OUT, "> $version_h" or die $!;
print OUT "#undef PUGS_SVN_REVISION\n";
if (-r $svn_entries) {
    open FH, $svn_entries or die $!;
    while (<FH>) {
        /^ *committed-rev=.(\d+)./ or next;
        print OUT "#define PUGS_SVN_REVISION $1\n";
        # rebuild Help.hs to show new revision number
        unlink "$base/src/Help.hi"; 
        exit;
    }
}
print OUT "#define PUGS_SVN_REVISION 0\n";
close OUT;
