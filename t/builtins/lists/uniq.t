#!/usr/bin/pugs

use Test;
use v6;

plan 7;

=head1 DESCRIPTION

This test tests the C<uniq> builtin.

Reference:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org>

See the thread "[S29] uniq" on p6l, too.

=cut

{
  my @array = <a b b c d e b b b b f b>;
  is ~@array, "a b b c d e b b b b f b",  "basic sanity";
  is eval('~@array.uniq'), "a b c d e f", "method form of uniq works", :todo;
  is eval('~uniq @array'), "a b c d e f", "subroutine form of uniq works", :todo;
  ok eval('@array.=uniq'),                "inplace form of uniq works (1)", :todo;
  is      ~@array,         "a b c d e f", "inplace form of uniq works (2)", :todo;
}

# With a userspecified criterion
{
  my @array = <a b A c b d>;
  # Semantics w/o junctions
  is eval('~@array.uniq:{ lc $^a eq lc $^b }'), "a b c d", "method form of uniq with own comparator works", :todo;
  is eval('~uniq { lc $^a eq lc $^b } @array'), "a b c d", "subroutine form of uniq with own comparator works", :todo;

  # Semantics w/ junctions
  # is eval('~@array.uniq:{ lc $^a eq lc $^b }.values.sort'),
  #   "A b c d a b c d", :todo;
}
