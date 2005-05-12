#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/anchors.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 19;

my $str = q{abc
def
ghi};

ok($str ~~ m/^abc/, 'SOS abc');
ok(not $str ~~ m/^bc/, 'SOS bc');
ok($str ~~ m/^^abc/, 'SOL abc');
ok(not $str ~~ m/^^bc/, 'SOL bc');
ok($str ~~ m/abc\n?$$/, 'abc newline EOL');
ok(not $str ~~ m/abc$$/, 'abc EOL');
ok(not $str ~~ m/ab$$/, 'ab EOL');
ok(not $str ~~ m/abc$/, 'abc EOS');
ok(not $str ~~ m/^def/, 'SOS def');
ok($str ~~ m/^^def/, 'SOL def');
ok($str ~~ m/def\n?$$/, 'def newline EOL');
ok(not $str ~~ m/def$$/, 'def newline EOL');
ok(not $str ~~ m/def$/, 'def EOS');
ok(not $str ~~ m/^ghi/, 'SOS ghi');
ok($str ~~ m/^^ghi/, 'SOL ghi');
ok($str ~~ m/ghi\n?$$/, 'ghi newline EOL');
ok($str ~~ m/ghi$$/, 'ghi EOL');
ok($str ~~ m/ghi$/, 'ghi EOS');
ok($str ~~ m/^abc\n$$^^d.*f\n$$^^ghi$/, 'All dot');
