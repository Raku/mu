#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/prior.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 31;

ok(!eval { "A" ~~ m/<?prior>/ }, 'No prior successful match');
ok($!, 'Error');

ok("A" ~~ m/<[A-Z]>/, 'Successful match');

ok("B" ~~ m/<?prior>/, 'Prior successful match');
ok(not "!" ~~ m/<?prior>/, 'Prior successful non-match');

ok(not "A" ~~ m/B/, 'Unsuccessful match');

ok("B" ~~ m/<?prior>/, 'Still prior successful match');
ok("B" ~~ m/<?prior>/, 'And still prior successful match');

ok("AB" ~~ m/A <?prior>/, 'Nested prior successful match');
ok(not "A" ~~ m/A <?prior>/, 'Nested prior successful non-match');
ok("B" ~~ m/<?prior>/, 'And even now prior successful match');

ok("!" ~~ m/<-[A-Z]>/, 'New successful match');

ok(not "B" ~~ m/<?prior>/, 'New prior successful non-match');
ok("!" ~~ m/<?prior>/, 'New prior successful match');

ok(not "A" ~~ m/B/, 'New unsuccessful match');

ok("%" ~~ m/<?prior>/, 'New still prior successful match');
ok("@" ~~ m/<?prior>/, 'New and still prior successful match');

ok("A!" ~~ m/A <?prior>/, 'New nested prior successful match');
ok(not "A" ~~ m/A <?prior>/, 'New nested prior successful non-match');
ok("^" ~~ m/<?prior>/, 'New and even now prior successful match');


ok("A" ~~ m/<[A-Z]>/, 'Another successful match');
ok("AA" ~~ m/^ <?prior>+ $/, 'Repeated prior');
is($/, "AA", 'Matched fully');

ok("A" ~~ m/^ <prior> $/, 'Captured prior');
is($/<prior>, "A", 'Captured correctly');

ok("AAAA" ~~ m/^ <prior>+ $/, 'Repeatedly captured prior');
is($/<prior>[0], 'A', 'Capture 0');
is($/<prior>[1], 'A', 'Capture 1');
is($/<prior>[2], 'A', 'Capture 2');
is($/<prior>[3], 'A', 'Capture 3');
ok(! defined $/<prior>[4], 'Capture 4');
