use v6-alpha;

use Test;

plan 4;

my $d = [];

is( +$d, 0, "no elems in array" );
ok( !($d.elems() > 0), "left side of && is false"); 
ok( !( ($d.elems() > 0) && ( $d.[0] == 1 ) ), "whole && expr is false"); 
is( +$d, 0, "right side of expression did not evaluate" );

