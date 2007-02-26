use v6-alpha;

use Test;

plan 1;

=pod

L<S02/"One-pass parsing">

=cut

ok(eval('regex foo { <[ } > ]> }; 1'),
    "can parse non-backslashed curly and right bracket in cclass");
