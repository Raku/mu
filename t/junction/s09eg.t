#!/usr/bin/pugs

use v6;
require Test;

=kwid

Tests autothreading junction example from S09

=cut

plan 3;

# test auto-threading

my $c = 0;

# FIXME - implement substr() in core
sub substr($str, $i, $j) {
    $c++;
    my @c = split "", $str;
    join("", @c[$i..($i+$j-1)]);
}

is (substr("camel", 0, 2),  "ca", "substr()");

$c = 0;
my $j = substr("camel", 0|1, 2&3);

is($c, 4, "substr() called 4 times");

# all(any("am","ca"),any("ame","cam"));

ok( ($j == all("am","ame")) &&
    ($j == all("ca","cam")),
    "Junctions as arg inputs work as expected" );

