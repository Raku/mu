#!/usr/bin/pugs

use v6;
require Test;

=kwid

Config Tests

=cut

plan 2;

# If this test fails because your osname is not listed here, please add it.
# But don't add other osnames just because you know of them. That way we can
# get a list of osnames that have actually passed tests.

ok($?OS, "We have an OS name: $?OS");

my $osnames = 'darwin' | 'linux' | 'MSWin32' | 'FreeBSD';
if ($?OS eq $osnames) {
    pass("...and we know of this OS")
}
else {
    todo_fail("We do not know of this OS -- please report to the pugs team")
}

