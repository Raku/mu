#!/usr/bin/perl -w

# This program determines the revision (number) that has been checked out.

use strict;
use warnings;

my $output = `svn info 2>&1`;
my $revision;
if ($output =~ /^Revision: (\d+)$/m) {
    $revision = $1;
} else {
    $output = `git show 2>&1`;
    if ($output =~ /^\s*git-svn-id:.*?@(\d+)/m) {
        $revision = $1;
    } else {
        die "$0: Can't obtain pugs repo revision :(\n";
    }
}

print $revision, "\n";
