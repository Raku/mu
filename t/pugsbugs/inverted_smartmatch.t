#!/usr/bin/pugs

use v6;
use Test;

=pod

Currently, pugs treats ~~ as smartmatch, but !~ as negated string match.
This leads to craziness like the following:

    pugs> not(0 !~ rx:Perl5/0/)
    bool::false
    pugs> not(0 ~~ rx:Perl5/0/)
    bool::false

=cut

plan 1;

my $opposites = not(0 ~~ rx:Perl5/0/) xor not(0 !~ rx:Perl5/0/);
ok($opposites, "~~ and !~ are opposites");
