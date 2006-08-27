#!/usr/bin/env perl

# this script downloads log files from colabti.de.

use strict;
use warnings;

use Getopt::Long;
#use LWP::UserAgent;
use LWP::UserAgent::Cached;

my ($all, $out_dir, $help, $channel);
GetOptions(
    'out-dir=s'   => \$out_dir,
    'channel'     => \$channel,
    'help'        => \$help,
    'all'         => \$all,
);

if ($help || @ARGV) {
    help();
}

$out_dir ||= '.';
mkdir $out_dir if !-e $out_dir;

$channel ||= 'perl6';
$channel =~ s/^\#//;

#my $ua = LWP::UserAgent->new;
my $ua = LWP::UserAgent::Cached->new; 
$ua->env_proxy;

my $base_url = "http://colabti.de/irclogger/irclogger_logs";
warn "  info: getting $base_url...\n";
my $res = $ua->get("$base_url/$channel");
#warn "Got!";
if ($res->is_success) {
    my %links = extract_links($res->content);
    while (my ($name, $url) = each %links) {
        my $local_file = "$out_dir/$name";
        if (!$all and -e $local_file) {
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
  $0 --out-dir=logs
  $0 --all --out-dir=.

Options:
  --out-dir <dir>  Specify the output directory for IRC log files.
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
