#!/usr/bin/pugs

use v6;
require Test;

# L<news:20050320062104.61811.qmail@web50805.mail.yahoo.com>

plan 1;

close $*ERR;

todo_fail("end block reached");

die "punt";

END { pass("end block reached") }


