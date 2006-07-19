use v6-alpha;

use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/word.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 7;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

ok(!( "abc  def" ~~ m/abc  def/ ), 'Literal space nonmatch' );
ok(   "abcdef"   ~~ m/abc  def/, 'Nonspace match' );
ok(   "abc  def" ~~ m:s/abc  def/, 'Word space match' );
ok(   "abc\ndef" ~~ m:sigspace/abc  def/, 'Word newline match' );
ok(!( "abcdef"   ~~ m:sigspace/abc  def/ ), 'Word nonspace nonmatch' );
ok(!( "abc  def" ~~ m:sigspace/abc <?sp> def/ ), 'Word explicit space non-match', :todo<bug> );
ok(   "abc  def" ~~ m:sigspace/abc <?ws> def/, 'Word explicit space match');

}

