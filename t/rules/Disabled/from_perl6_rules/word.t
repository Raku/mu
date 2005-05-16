#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/word.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 7;

ok(!( "abc  def" ~~ m/abc  def/ ), 'Literal space nonmatch');
ok("abcdef" ~~ m/abc  def/, 'Nonspace match');
ok("abc  def" ~~ m:w/abc  def/, 'Word space match');
ok("abc\ndef" ~~ m:words/abc  def/, 'Word newline match');
ok(!( "abcdef" ~~ m:words/abc  def/ ), 'Word nonspace nonmatch');
ok(!( "abc  def" ~~ m:words/abc <?sp> def/ ), 'Word explicit space non-match');
ok("abc  def" ~~ m:words/abc <?ws> def/, 'Word explicit space match');
