#!/usr/bin/pugs

use v6;
require Test;

=kwid

goto tests

=cut

plan 2;

# VERY WEIRD PARSEFAIL.
# *delete*, don't comment, the last line here, and it will pass.
#
# elmex points out that there is no parsefail if you change it
# from pass("string") to something else.

sub test1_ok () { pass("&sub.goto does"); };

sub test1 () {
	&test1_ok.goto();
	fail("&sub.goto doesn't");
}
test1();
pass("ok");

