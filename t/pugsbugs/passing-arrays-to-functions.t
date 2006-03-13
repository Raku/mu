#!/usr/bin/pugs

use v6;
use Test;

plan 1;

sub test(@a,@b)
{
    return @a[0] + @b[0];
}

# TEST
is(test([100,5],[20,300]), 120, 
   "Passing array references to functions accepting arrays works.");
