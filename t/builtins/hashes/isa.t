use v6-pugs;

use Test;

=kwid 

Isa tests

=cut

plan 2;

{   my %hash = <1 2 3 4>;
    isa_ok(%hash, 'Hash');
    isa_ok(%hash, 'List');
}
