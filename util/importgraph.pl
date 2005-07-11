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
GetOptions(
  "layout=s" => \$layout,
) or die "invalid command line";

my $g = GraphViz->new(
  layout => $layout,
  directed => 1,
  rankdir => 1,
  overlap => "false",
);

my %modules;
find sub {
  return unless /\.hs$/;

  my ($module_name, %imports);

  open my $fh, '<', $_
    or die "couldn't open $File::Find::name: $!\n";

  while(<$fh>) {
    if (/^module \s+ ([\w.]+)/x) {
      $module_name = $1;
    } elsif (/^import \s+ (?:qualified \s+)? ([\w.]+)/x) {
      $imports{$1} = 1;
    } elsif (/^import|^module/) {
      warn "Unrecognised import|module: $_";
    }
  }

  close $fh;

  unless (defined $module_name) {
    warn "couldn't find a module in $File::Find::name\n";
    return;
  }

  $modules{$module_name} = \%imports;
}, 'src';

my ($nodes, $edges) = (0, 0);
while (my ($name, $imports) = each %modules) {
  $g->add_node($name);
  $nodes++;

  while(my ($k, undef) = each %$imports) {
    next unless exists $modules{$k}; # only pugs modules
    $g->add_edge($name, $k);
    $edges++;
  }
}
print "$nodes nodes and $edges edges\n";

#$g->as_canon("importgraph.dot");
#$g->as_png("importgraph.png");
$g->as_ps("importgraph.ps");
