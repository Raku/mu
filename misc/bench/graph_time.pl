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

my $graph = GD::Graph::bars->new(600, 400);
$graph->set(
    bgclr             => 'white',
    transparent       => 0,
    long_ticks        => 1,
    x_labels_vertical => 1,
    x_label_skip      => 0,
    bargroup_spacing  => 0,
    show_values       => 1,
    values_vertical   => 1,
    legend_placement  => 'BL',
    y_label           => 'relative time (%)',
    overwrite         => 1,
    cumulate          => 1,
    dclrs             => [ qw(lred lgreen ) ],
);
$graph->set_x_axis_font('/usr/share/fonts/truetype/DejaVuSans.ttf', 8);
$graph->set_y_axis_font('/usr/share/fonts/truetype/DejaVuSans.ttf', 8);
$graph->set_values_font('/usr/share/fonts/truetype/DejaVuSans.ttf', 8);
$graph->set_legend_font('/usr/share/fonts/truetype/DejaVuSans.ttf', 14);

my $relative = 1;
my @relatives = map { ( $relative *= $_->{relative} ) - 1 } @$reports;
my $plot = $graph->plot([
    [map { $_->{report} }                @$reports],
    [map { $_ >  0 ? sprintf('%.2f', $_ * 100) : undef } @relatives],
    [map { $_ <= 0 ? sprintf('%.2f', $_ * 100) : undef } @relatives],
]);
die $graph->error unless $plot;

write_file('benchmarks.png', $plot->png);
