#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;

my $version_h = shift || Cwd::cwd() . "src/Pugs/pugs_version.h";
my $base = shift || Cwd::cwd();
my $svn_entries = "$base/.svn/entries";

print "Writing version from $svn_entries to $version_h\n";

my $revision = 0;
open OUT, "> $version_h" or die $!;
print OUT "#undef PUGS_SVN_REVISION\n";
if (-r $svn_entries) {
    open FH, $svn_entries or die $!;
    while (<FH>) {
        /^ *committed-rev=.(\d+)./ or next;
	$revision = $1;
    }
    close FH;
} elsif (my @info = qx/svk info/ and $? == 0) {
    my ($line) = grep /Mirrored From/, @info;
    ($revision) = $line =~ /Rev\. (\d+)/;
}
print OUT "#define PUGS_SVN_REVISION $revision\n";
close OUT;

if ($revision != 0) {
    # rebuild Help.hs to show new revision number
    unlink "$base/src/Pugs/Help.hi";
    unlink "$base/src/Pugs/Help.o";
    exit;
}
