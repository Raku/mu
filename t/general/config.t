#!/usr/bin/pugs

use v6;
require Test;

=kwid

Config Tests

If this test fails because your osname is not listed here, please add it.
But don't add other osnames just because you know of them. That way we can
get a list of osnames that have actually passed tests.

=cut

plan 3;

# $?OS is the OS we were compiled in.
ok $?OS, "We have an OS name: $?OS";

# my $osnames = 'darwin' | 'linux' | 'MSWin32' | 'FreeBSD';
my $osnames = any<darwin linux FreeBSD MSWin32 mingw cygwin>;
if ($?OS eq $osnames) {
    pass("...and we know of this OS")
} else {
    todo_fail("We do not know of this OS -- please report to the pugs team")
}

# $*OS is the OS we are running currently. Those two don't need to be the same
# (think of compiling to bytecode on computer1 and running the bytecode on
# computer2).
todo_eval_ok '$*OS', '$*OS works';
