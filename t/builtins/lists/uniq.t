#!/usr/bin/pugs

require Test;
use v6;

plan 5;

=head1 DESCRIPTION

This test tests the C<uniq> builtin.

Reference:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org>

=cut

my @array = <a b b c d e b b b b f b>;
is ~@array, "a b b c d e b b b b f b",          "basic sanity";
eval_is '~@array.uniq', "a b c d e b f b", "method form of uniq works", :todo(1);
eval_is '~uniq @array', "a b c d e b f b", "subroutine form of uniq works", :todo(1);
eval_ok '@array.=uniq',                    "inplace form of uniq works (1)", :todo(1);
is      ~@array,        "a b c d e b f b", "inplace form of uniq works (2)", :todo(1);
