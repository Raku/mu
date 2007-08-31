use v6-alpha;

use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/inline.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

# L<S05/Modifiers/and Unicode-level modifiers can be>

=cut

plan 2;

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

ok("abcDEFghi" ~~ m/abc (:i def) ghi/, 'Match');
ok(!( "abcDEFGHI" ~~ m/abc (:i def) ghi/ ), 'Mismatch');

}

