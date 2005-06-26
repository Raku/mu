#!/usr/bin/perl
# Creates a statistic of Pugs' development.
# Usage: svn log | util/svnlog2graph.pl > /tmp/graph.png   --or
#        svn log > /tmp/log; util/svnlog2graph /tmp/log > /tmp/graph.png

use warnings;
use strict;

use GD::Graph::lines;
use Time::Piece;
use Time::Seconds;

local $_;
my $num_commits;
my @commits;
my @developers;
my %devs_seen;

print STDERR <<INFO unless @ARGV;
Pipe output of "svn log" or save "svn log" output in a file and
specify this file as parameter.
Output will go to STDOUT in PNG format.
INFO

# Read the logfile
while(<>) {
  # Only process headlines
  next unless m/^r/ and m/lines?$/ and m/\|/;
  my ($dev, $date) = (split / \| /)[1, 2];
  $date =~ s/ [+-]\d+ \(.*$//;

  # Example: $date is now "2005-02-06 17:52:06"
  my $time = Time::Piece->strptime($date, "%Y-%m-%d %H:%M:%S");
  push @commits,    $time;
  push @developers, $time unless $devs_seen{$dev}++;
  $num_commits++;
}

# $commits[0] should be first commit, not last
@commits    = reverse @commits;
@developers = reverse @developers;

# Collect commits in days
# E.g. $commits_till_day[42] = 1500 (1500 commits from day 1 to day 42)
my @commits_till_day = dayify(@commits);
my @devs_till_day    = dayify(@developers);
# @devs_till_day should have the same length as @commits_till_day --
# fill if needed
push @devs_till_day, $devs_till_day[-1] while
  @devs_till_day < @commits_till_day;

# Create the graph.
my $graph = GD::Graph::lines->new(500, 350);
$graph->set(
  title        => "Pugs development",
  x_label      => "Days",
  y_label      => "Commits/Developers",
  x_label_skip => 10,
  y_max_value  => 6000,
) or die $graph->error;

my @data = (
  [ 0..@commits_till_day ],              # Day#
  [ 0, @commits_till_day ],              # Commits
  [ 0, map { 50 * $_ } @devs_till_day ], # Developers (scaled)
);

my $gd = $graph->plot(\@data) or die $graph->error;
binmode STDOUT;
print $gd->png;

# Input:  (2, 5, 9, 86400+17, 2*86400+35, 4*86400+50)
# Output: (3,       4,        5,          5, 6)
sub dayify {
  my @till_day;

  for(@_) {
    my $cur_day = int(($_ - $_[0]) / ONE_DAY);

    if($cur_day != $#till_day) {
      push @till_day, $till_day[-1] || 0
	while $#till_day < $cur_day;
    } else {
      $till_day[-1]++;
    }
  }

  return @till_day;
}
