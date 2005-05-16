#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/combchar.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 3;

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

my $unichar = "\c[GREEK CAPITAL LETTER ALPHA]";
my $combchar = "\c[LATIN CAPITAL LETTER A]\c[COMBINING ACUTE ACCENT]";

ok("A" ~~ m/^<.>$/, 'ASCII');
ok($combchar ~~ m/^<.>$/, 'Unicode combining');
ok($unichar ~~ m/^<.>$/, 'Unicode');

}

