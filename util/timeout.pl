#!/usr/bin/env perl

# This script serves a hacky timeout daemon for auto-smoke
# Please run util/t/02-timeout.t after updating this file.

use strict;
use warnings;

#use Smart::Comments;
use List::Util qw( first );
use Getopt::Std;
use POSIX qw( getuid getpid );

my %opts;
getopts('r:u:t:h', \%opts) or die help();
if ($opts{h}) { print help(); exit 0; }

my $regex = $opts{r};
die "No command line pattern specified. See -h.\n" if !defined $regex;
my $threshold = $opts{t};
die "No cpu-time threshold specified. See -h.\n" if !defined $threshold;

sub help {
    print <<_EOC_;
Usage:
    $0 -r <cmd-regex> -t <cputime-threshold> -u <user>
Examples:
    $0 -r '^\\./pugs\\s+' -t '00:05:00'
    $0 -r '\\bsvk\\b' -t '01:00:00' -u audreyt
_EOC_
}

my $user = $opts{user} || getuid();
### $regex
## $user

my @lines = split /\n/, `ps -u $user -o pid -o cputime -o cmd`;
## @lines
my $my_pid = getpid();
for my $line (@lines) {
    my ($pid, $time, $cmd) = ($line =~ /^\s*(\d+)\s+(\S+)\s+(.+)$/);
    next if !defined $pid;
    if ($cmd =~ m/$regex/o) {
        ## $pid
        ### $time
        ### $cmd
        if ($time ge $threshold) {
            if ($pid == $my_pid) {
                warn "Skipping myself...\n";
                next;
            }
            warn "Killing $pid ($cmd)...\n";
            kill(15, $pid);
        }
   }
}

