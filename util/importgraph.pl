#!/usr/bin/env perl
# usage: util/importgraph.pl [--layout=neato]
# produces importgraph.ps in the current directory
# it expects to run from the top pugs directory with src present
# arrows point from importer to importee

use strict;
use warnings;
use File::Find;
use GraphViz;
use Getopt::Long;

my $layout = "neato";
my $overlap = "false";
my @ignore;
GetOptions(
  "layout=s" => \$layout,
  "overlap=s" => \$overlap,
  "ignore=s" => \@ignore,
) or die "invalid command line";

my $g = GraphViz->new(
  layout => $layout,
  directed => 1,
  rankdir => 1,
  overlap => $overlap,
);

my %modules;
find sub {
  return unless /\.hs$/;

  my ($module_name, $exports, %imports);

  open my $fh, '<', $_
    or die "couldn't open $File::Find::name: $!\n";

  while(<$fh>) {
    if (/^module \s+ ([\w.]+) (\s* \()?/x) {
      $module_name = $1;
      $exports = defined $2;
    } elsif (/^import \s+ (qualified \s+)? ([\w.]+) (\s* \()?/x) {
      $imports{$2} = [defined($1), defined($3)];
    } elsif (/^import|^module/) {
      warn "Unrecognised import|module: $_";
    }
  }

  close $fh;

  unless (defined $module_name) {
    warn "couldn't find a module in $File::Find::name\n";
    return;
  }

  $modules{$module_name} = [\%imports, $exports];
}, 'src';

delete $modules{$_} for @ignore;

my ($nodes, $edges) = (0, 0);
while (my ($name, $module) = each %modules) {
  $g->add_node($name, color => ($module->[1] ? 'green' : 'black'));
  $nodes++;

  while(my ($k, $edge) = each %{$module->[0]}) {
    next unless exists $modules{$k}; # only pugs modules
    my $color = $edge->[1] ? 'green' : $edge->[0] ? "blue" : "black";
    $g->add_edge($name, $k, color => $color);
    $edges++;
  }
}
print "$nodes nodes and $edges edges\n";

#$g->as_canon("importgraph.dot");
#$g->as_png("importgraph.png");
$g->as_ps("importgraph.ps");
