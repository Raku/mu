#!/usr/bin/pugs

use v6;
use Test;

plan 1;

# This is first attempt at rationalizing the := form into a Siglist method call.
{
    my ($x, $y, $z);
    my $siglist = eval ':($x,$y,$z)';
    try { $siglist.infix:<:=>(1,2,3) };
    is("$x $y $z", "1 2 3", "siglist bindings works", :todo<feature>);
}
