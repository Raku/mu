#!/usr/bin/pugs

use v6;
require Test;

# L<news:20050320062104.61811.qmail@web50805.mail.yahoo.com>

plan 1;

close $*ERR;

# I am not sure this will even work because I am not 100% sure
# of the order in which the END {} blocks run. Will the Test.pm 
# END {} run first? Or will this one? And how will that affect 
# the test?

skip(1, 'END {} blocks are not printing???');

die "punt";
END { pass("end block reached") }


