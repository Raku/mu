#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Isa tests

=cut

plan 1;

{   my %hash = <1 2 3 4>;
    ok %hash.isa("List"), 'hashes descend from lists';
}
