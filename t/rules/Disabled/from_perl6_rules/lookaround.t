#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/lookaround.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 10;

rule bc { b?c }

ok("a cdef" ~~ m/<after a <?sp> c> def/, 'Lookbehind');
ok(!( "acdef" ~~ m/<after a <?sp> c> def/ ), 'Lookbehind failure');
ok(!( "a cdef" ~~ m/<!after a <?sp> c> def/ ), 'Negative lookbehind failure');
ok("acdef" ~~ m/<!after a <?sp> c> def/, 'Negative lookbehind');

ok("abcd f" ~~ m/abc <before d <?sp> f> (.)/, 'Lookahead');
is($0, 'd', 'Verify lookahead');
ok(!( "abcdef" ~~ m/abc <before d <?sp> f>/ ), 'Lookahead failure');
ok(!( "abcd f" ~~ m/abc <!before d <?sp> f>/ ), 'Negative lookahead failure');
ok("abcdef" ~~ m/abc <!before d <?sp> f> (.)/, 'Negative lookahead');
is($0, 'd', 'Verify negative lookahead');
