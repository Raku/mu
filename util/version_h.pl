#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;

my $version_h = shift || Cwd::cwd() . "src/Pugs/pugs_version.h";
my $base = shift || Cwd::cwd();
my $svn_entries = "$base/.svn/entries";

print "Writing version from $svn_entries to $version_h\n";

open OUT, "> $version_h" or die $!;
print OUT "#undef PUGS_SVN_REVISION\n";
if (-r $svn_entries) {
    open FH, $svn_entries or die $!;
    while (<FH>) {
        /^ *committed-rev=.(\d+)./ or next;
        print OUT "#define PUGS_SVN_REVISION $1\n";
        # rebuild Help.hs to show new revision number
        unlink "$base/src/Pugs/Help.hi";
        unlink "$base/src/Pugs/Help.o";
        exit;
    }
}
print OUT "#define PUGS_SVN_REVISION 0\n";
close OUT;
