#!/usr/bin/perl

use 5.10.0;
use strict;
use warnings;
use utf8;

use JSON;
use File::Slurp;
use POSIX qw(strftime);
use List::Util qw(sum);
use GD::Graph::bars;
use DBI;

my $dbh = DBI->connect('dbi:SQLite:time_stats.sql');

my $stats = from_json scalar read_file 'rakudo/docs/test_summary.times';
my $date = $stats->{test_history}[-1][0];

my $last_run = ($dbh->selectrow_array('select max(run_time) from report') or '0000-00-00 00:00:00');

if ($last_run lt $date) {
    $dbh->do('insert into report (run_time) values (?)', undef, $date);

    my $last_report = $dbh->selectall_arrayref('select * from test_run where report = ?', {Slice => {}}, $last_run);
    my %last_report = map { $_->{file} => $_ } @$last_report;

    while (my ($file, $run) = each %{ $stats->{test_microseconds} }) {
        my $mtime = strftime("%F %T", localtime((stat "rakudo/t/spec/$file")[9]));

        my $elapsed = sum(map { $_->[1][-1] } @$run);
        my $relative = ($last_report and exists $last_report{$file} and $last_report{$file}{elapsed} and $last_report{$file}{report} gt $mtime) ? $elapsed / $last_report{$file}{elapsed} : 1;

        #warn "insert into test_run (report, file, elapsed, relative) values ($date, $file, $elapsed, $relative)";
        $dbh->do('insert into test_run (report, file, elapsed, relative) values (?, ?, ?, ?)', undef, $date, $file, $elapsed, $relative);
    }
}
