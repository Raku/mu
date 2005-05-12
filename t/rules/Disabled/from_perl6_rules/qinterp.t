#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/qinterp.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 4;

ok("ab cd" ~~ m/a <'b c'> d/, 'ab cd 1');
ok(not "abcd" ~~ m/a <'b c'> d/, 'not abcd 1');
ok("ab cd" ~~ m/ab <' '> c d/, 'ab cd 2');
ok("ab/cd" ~~ m/ab <'/'> c d/, 'ab/cd');
