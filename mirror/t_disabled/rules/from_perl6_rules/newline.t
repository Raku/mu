#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/newline.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 15;

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

ok("\n" ~~ m/\n/, '\n');

ok("\015\012" ~~ m/\n/, 'CR/LF');
ok("\012" ~~ m/\n/, 'LF');
ok("a\012" ~~ m/\n/, 'aLF');
ok("\015" ~~ m/\n/, 'CR');
ok("\x85" ~~ m/\n/, 'NEL');
ok("\x2028" ~~ m/\n/, 'LINE SEP');

ok(!( "abc" ~~ m/\n/ ), 'not abc');

ok(!( "\n" ~~ m/\N/ ), 'not \n');

ok(!( "\012" ~~ m/\N/ ), 'not LF');
ok(!( "\015\012" ~~ m/\N/ ), 'not CR/LF');
ok(!( "\015" ~~ m/\N/ ), 'not CR');
ok(!( "\x85" ~~ m/\N/ ), 'not NEL');
ok(!( "\x2028" ~~ m/\N/ ), 'not LINE SEP');

ok("abc" ~~ m/\N/, 'abc');

}

