#!/usr/bin/perl

use 5.10.0;
use strict;
use warnings;
use utf8;

use File::Slurp;
use POSIX qw(strftime);
use List::Util qw(sum);
use GD::Graph::bars;
use DBI;

my $dbh = DBI->connect('dbi:SQLite:time_stats.sql');

my $reports = $dbh->selectall_arrayref('select report, avg(relative) as relative from test_run group by report order by report', {Slice => {}});

my $graph = GD::Graph::bars->new(500, 300);
$graph->set(
    bgclr             => 'white',
    transparent       => 0,
    long_ticks        => 1,
    x_labels_vertical => 1,
    x_label_skip      => 10,
    bargroup_spacing  => 0,
    show_values       => 0,
    values_vertical   => 1,
    legend_placement  => 'BL',
    y_label           => 'relative time',
    overwrite         => 1,
    cumulate          => 1,
);
$graph->set_x_axis_font('/usr/X11R6/lib/X11/fonts/truetype/arial.ttf', 9);
$graph->set_y_axis_font('/usr/X11R6/lib/X11/fonts/truetype/arial.ttf', 9);
$graph->set_legend_font('/usr/X11R6/lib/X11/fonts/truetype/arial.ttf', 14);

my $relative = 1;
my $plot = $graph->plot([
    [map { $_->{report} }                @$reports],
    [map { $relative *= $_->{relative} } @$reports],
]);
die $graph->error unless $plot;

write_file('benchmarks.png', $plot->png);
