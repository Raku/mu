#!/usr/bin/pugs

use v6;
require Test;

=pod

Basic "if" tests.

=cut

plan 6;

my $x = 'test';
if ($x eq $x) { pass(); } else { fail(); }
if ($x ne $x) { fail(); } else { pass(); }
if (1) { pass(); } else { fail(); }
if (0) { fail(); } else { pass(); }
if (undef) { fail(); } else { pass(); }

# die called in the condition part of an if statement should die immediately
# rather than being evaluated as true
my $foo = 1;
eval 'if (die "should die") { $foo = 3 } else { $foo = 2; }';
#say '# $foo = ' ~ $foo;
is $foo, 1, "die should stop execution immediately.";
