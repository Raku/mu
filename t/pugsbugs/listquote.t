#!/usr/bin/pugs

use v6;
require Test;

=kwid

= DESCRIPTION

This file tests a parse failure between expressions
and list quotes <>:

The C<< <3 >> is seen as the start of a list that
extends into the commented line. The expression
should be parsed as restricted to the one C<ok()>
line of course.

The following expressions also contend for the
same problem:

Two-way comparison:

  1 < EXPR > 2

Hash access:

  HASHEXPR<KEY>

= TODO

Add relevant Sxx and/or Axx references, that
describe the conflicting cases.

=cut

plan 1;

# L<S02/Literals/>
# L<S03/Chained comparisons/>

ok( (1 | 3)<3 , 'No parsefail');
#>

1;