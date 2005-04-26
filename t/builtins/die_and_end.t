#!/usr/bin/pugs

use v6;
use Test;

# L<news:20050320062104.61811.qmail@web50805.mail.yahoo.com>

plan 1;

close $*ERR;

# I am not sure this will even work because I am not 100% sure
# of the order in which the END {} blocks run. Will the Test.pm 
# END {} run first? Or will this one? And how will that affect 
# the test?

END { pass("end block reached") }

exit; # same as die() -- but Harness doesn't like a failed exit code

fail("exit did not work");


