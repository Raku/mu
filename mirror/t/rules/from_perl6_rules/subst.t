#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/subst.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 3;

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

my $s = "ZBC"; my @a = ("A", 'ZBC');

my $_ = q{Now I know my abc's};

s:global/Now/Wow/;
is($_, q{Wow I know my abc's}, 'Constant substitution');

s:global/abc/$s/;
is($_, q{Wow I know my ZBC's}, 'Scalar substitution');

s:g/BC/@a[]/;
is($_, q{Wow I know my ZA ZBC's}, 'List substitution');

}

