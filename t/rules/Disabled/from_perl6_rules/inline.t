#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/inline.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 2;

ok("abcDEFghi" ~~ m/abc (:i def) ghi/, 'Match');
ok(not "abcDEFGHI" ~~ m/abc (:i def) ghi/, 'Mismatch');
