#!/usr/bin/pugs

use v6;
use Test;

=kwid

Tests autothreading junction example from Synopsis 09 

L<S09/"Junctions">

=cut

plan 3;

# test auto-threading

my $c = 0;

is (substr("camel", 0, 2),  "ca", "substr()");

$c = 0;
sub my_substr ($str, $i, $j) {
    $c++;
    my @c = split "", $str;
    join("", @c[$i..($i+$j-1)]);
}

my $j = my_substr("camel", 0|1, 2&3);

# L<S09/"Junctions" /Each of the resulting set of calls is then recursively autothreaded/>
is($c, 4, "substr() called 4 times");

# all(any("am","ca"),any("ame","cam"));

ok( ($j == all("am","ame")) &&
    ($j == all("ca","cam")),
    "Junctions as arg inputs work as expected" );

