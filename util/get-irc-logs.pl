#!/usr/bin/env perl

# this script downloads log files from colabti.de.

use strict;
use warnings;

use Getopt::Long;
use LWP::UserAgent;
#use LWP::UserAgent::Cached;  # Debug purpose

my ($all, $help, $channel);
GetOptions(
    'channel=s'   => \$channel,
    'help'        => \$help,
    'all'         => \$all,
) or help();

my $out_dir = shift || '.';

if ($help) {
    help();
}

$out_dir ||= '.';
mkdir $out_dir if !-e $out_dir;

my $last_log;
if (!$all) {
    my @existing_files = sort glob "$out_dir/*.log";
    $last_log = pop @existing_files;
}

$channel ||= 'perl6';
$channel =~ s/^\#//;

my $ua = LWP::UserAgent->new;
#my $ua = LWP::UserAgent::Cached->new;  # Debug purpose
$ua->env_proxy;

my $base_url = "http://colabti.org/irclogger/irclogger_logs";
warn "  info: getting $base_url/$channel...\n";
my $res = $ua->get("$base_url/$channel");
#warn "Got!";
if ($res->is_success) {
    my %links = extract_links($res->content);
    while (my ($name, $url) = each %links) {
        my $local_file = "$out_dir/$name";
        if (!$all and -e $local_file and index($last_log, $name) == -1) {
            warn "  $local_file already exists, skipped.\n";
            next;
        } else {
            warn "generating $local_file...\n";
            $ua->mirror("http://colabti.de/$url", $local_file);
        }
    }
}
else {
    die $res->status_line;
}

sub help {
    print <<_EOC_;
Usage:
  $0
  $0 <out-dir>
  $0 --all tmp

Options:
  --channel <name> Specify the IRC channel. Defaults to #perl6.
  --all            Download very log file even if there's one in out-dir.
  --help           Show this help.
_EOC_
    exit(0);
}

sub extract_links {
    my $html = shift;
    my %links;
    while ($html =~
           m{"/irclogger/irclogger_log/$channel\?date=(\d+-\d+-\d+),\w+;raw=on"}g) {
        my ($name, $url) = ("$channel-$1.log", $&);
        $url =~ s/^"|"$//g;
        $links{$name} = $url;
    }
    %links;
}
