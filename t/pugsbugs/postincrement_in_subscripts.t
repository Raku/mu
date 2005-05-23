#!/usr/bin/pugs

use v6;
use Test;

# Test case courtesy of Limbic_Region

plan 1;

{
    my $curr  = 4;
    my @array = 1..5;
    @array[ --$curr ]++;

    is $curr, 3, "postincrements in array subscripts work", :todo<bug>;
}
