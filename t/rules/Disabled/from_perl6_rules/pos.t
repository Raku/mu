#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/pos.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 9;

if(eval('!("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

my $str = "abrAcadAbbra";

ok($str ~~ m/ a .+ A /, 'Match from start');
ok($/.pos == 0, 'Match pos is 0');

ok($str ~~ m/ A .+ a /, 'Match from 3');
ok($/.pos == 3, 'Match pos is 3');

ok(!( $str ~~ m/ Z .+ a / ), 'No match');
ok(!defined $/.pos, 'Match pos is undef');

rule Aa { A .* a }
ok($str ~~ m/ .*? <Aa> /, 'Subrule match from 3');
ok($/.pos == 0, 'Full match pos is 0');
ok($/<Aa>.pos == 3, 'Subrule match pos is 3');

}

