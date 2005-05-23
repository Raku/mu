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

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

ok(!eval(' "A" ~~ m/<?prior>/ '), 'No prior successful match');
ok($!, 'Error', :todo<feature> );

ok("A" ~~ m/<[A-Z]>/, 'Successful match', :todo<feature> );

ok("B" ~~ m/<?prior>/, 'Prior successful match', :todo<feature> );
ok(!( "!" ~~ m/<?prior>/ ), 'Prior successful non-match');

ok(!( "A" ~~ m/B/ ), 'Unsuccessful match');

ok("B" ~~ m/<?prior>/, 'Still prior successful match', :todo<feature> );
ok("B" ~~ m/<?prior>/, 'And still prior successful match', :todo<feature> );

ok("AB" ~~ m/A <?prior>/, 'Nested prior successful match', :todo<feature> );
ok(!( "A" ~~ m/A <?prior>/ ), 'Nested prior successful non-match');
ok("B" ~~ m/<?prior>/, 'And even now prior successful match', :todo<feature> );

ok("!" ~~ m/<-[A-Z]>/, 'New successful match', :todo<feature> );

ok(!( "B" ~~ m/<?prior>/ ), 'New prior successful non-match');
ok("!" ~~ m/<?prior>/, 'New prior successful match', :todo<feature> );

ok(!( "A" ~~ m/B/ ), 'New unsuccessful match');

ok("%" ~~ m/<?prior>/, 'New still prior successful match', :todo<feature> );
ok("@" ~~ m/<?prior>/, 'New and still prior successful match', :todo<feature> );

ok("A!" ~~ m/A <?prior>/, 'New nested prior successful match', :todo<feature> );
ok(!( "A" ~~ m/A <?prior>/ ), 'New nested prior successful non-match');
ok("^" ~~ m/<?prior>/, 'New and even now prior successful match', :todo<feature> );


ok("A" ~~ m/<[A-Z]>/, 'Another successful match', :todo<feature> );
ok("AA" ~~ m/^ <?prior>+ $/, 'Repeated prior', :todo<feature> );
is($/, "AA", 'Matched fully', :todo<feature> );

ok("A" ~~ m/^ <prior> $/, 'Captured prior', :todo<feature> );
is($/<prior>, "A", 'Captured correctly', :todo<feature> );

ok("AAAA" ~~ m/^ <prior>+ $/, 'Repeatedly captured prior', :todo<feature> );
is(try { $/<prior>[0] }, 'A', 'Capture 0', :todo<feature> );
is(try { $/<prior>[1] }, 'A', 'Capture 1', :todo<feature> );
is(try { $/<prior>[2] }, 'A', 'Capture 2', :todo<feature> );
is(try { $/<prior>[3] }, 'A', 'Capture 3', :todo<feature> );
ok(try {! defined($/<prior>[4]) }, 'Capture 4', :todo<feature> );

}

