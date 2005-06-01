#!/usr/bin/pugs

use v6;
use Test;

plan 1;

=pod

$0 should not be defined.

Pcre is doing the right thing:
  $ pcretest
...
    re> /a|(b)/
  data> a
   0: a
  data>
so it looks like a pugs-pcre interface bug.

=cut

"a" ~~ rx:perl5/a|(b)/;
is($0, undef, 'An unmatched capture should be false.');
