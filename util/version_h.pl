#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;

my $version_h = shift || Cwd::cwd() . "/src/Pugs/pugs_version.h";
my $base = shift || Cwd::cwd();
my $svn_entries = "$base/.svn/entries";

my $revision = 0;
open OUT, "> $version_h" or die $!;
print OUT "#undef PUGS_SVN_REVISION\n";
if (-r $svn_entries) {
    print "Writing version from $svn_entries to $version_h\n";
    open FH, $svn_entries or die $!;
    while (<FH>) {
        /^ *committed-rev=.(\d+)./ or next;
	$revision = $1;
	last;
    }
    close FH;
} elsif (my @info = qx/svk info/ and $? == 0) {
    print "Writing version from `svk info` to $version_h\n";
    my ($line) = grep /(?:file|svn|https?)\b/, @info;
    ($revision) = $line =~ / (\d+)$/;
}
$revision ||= 0;
print OUT "#define PUGS_SVN_REVISION $revision\n";
close OUT;

if ($revision != 0) {
    # rebuild Help.hs to show new revision number
    unlink "$base/src/Pugs/Help.hi";
    unlink "$base/src/Pugs/Help.o";
    exit;
}
