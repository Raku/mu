#!/usr/bin/pugs

use v6;
use Test;

=kwid 

Isa tests

=cut

plan 3;

{   
    my @arr = <1 2 3 4>;
    isa_ok(@arr, 'Array');
    isa_ok(@arr, 'List');
}

{   my $arr = <1 2 3 4>;
    is $arr.ref, 'Array', '$array is an Array, not a List';
}
