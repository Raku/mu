#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Isa tests

=cut

plan 2;

{   my @arr = <1 2 3 4>;
    ok @arr.isa("List"), 'arrays descend from lists';
}

{   my $arr = <1 2 3 4>;
    is $arr.ref, 'Array', '$array is an Array, not a List';
}
