#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/patvar.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 19;

my $var = rx/a+b/;

my @var = (rx/a/, rx/b/, rx/c/, rx/\w/);

my %var = (a=>rx:w/ 4/, b=>rx:w/ cos/, c=>rx:w/ \d+/);


# SCALARS

ok(!( "a+b" ~~ m/<{$var}>/ ), 'Simple scalar match');
ok(!( "zzzzzza+bzzzzzz" ~~ m/<{$var}>/ ), 'Nested scalar match');
ok("aaaaab" ~~ m/<{$var}>/, 'Rulish scalar match');


# ArrayS

ok("a" ~~ m/@var/, 'Simple array match (a)');
ok("b" ~~ m/@var/, 'Simple array match (b)');
ok("c" ~~ m/@var/, 'Simple array match (c)');
ok("d" ~~ m/@var/, 'Simple array match (d)');
ok(!( "!" ~~ m/@var/ ), 'Simple array match (!)');
ok("!!!!a!!!!!" ~~ m/@var/, 'Nested array match (a)');
ok("!!!!e!!!!!" ~~ m/@var/, 'Nested array match (e)');

ok("abca" ~~ m/^@var+$/, 'Multiple array matching');
ok(!( "abca!" ~~ m/^@var+$/ ), 'Multiple array non-matching');


# HASHES

ok("a 4" ~~ m/%var/, 'Simple hash interpolation (a)');
ok("b cos" ~~ m/%var/, 'Simple hash interpolation (b)');
ok("c 1234" ~~ m/%var/, 'Simple hash interpolation (c)');
ok(!( "d" ~~ m/%var/ ), 'Simple hash interpolation (d)');
ok("====a 4=====" ~~ m/%var/, 'Nested hash interpolation (a)');
ok(!( "abca" ~~ m/^%var$/ ), 'Simple hash non-matching');


ok("a 4 b cos c 99  a 4" ~~ m:w/^[ %var]+$/, 'Simple hash repeated matching');
