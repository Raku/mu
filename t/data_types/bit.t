#!/usr/bin/pugs

use v6;
require Test;

# Mostly copied from Perl 5.8.4 s t/op/bop.t

plan 17;

# test the bit operators '&', '|', '^', '~', '<<', and '>>'

# numerics
is (0xdead +& 0xbeef, 0x9ead);
is (0xdead +| 0xbeef, 0xfeef);
is (0xdead +^ 0xbeef, 0x6042);
ok (+^0xdead +& 0xbeef == 0x2042);
# ok (+^(0xdead +& 0xbeef) == 0x2042); # works

# shifts
ok(32896 == (257 +< 7));
#ok ((257 +< 7) == 32896); # XXX
ok(257 == (33023 +> 7));
#ok ((33023 +> 7) == 257); # XXX

# signed vs. unsigned
#ok ((+^0 +> 0 && do { use integer; ~0 } == -1));

#my $bits = 0;
#for (my $i = ~0; $i; $i >>= 1) { ++$bits; }
#my $cusp = 1 << ($bits - 1);


#ok (($cusp & -1) > 0 && do { use integer; $cusp & -1 } < 0);
#ok (($cusp | 1) > 0 && do { use integer; $cusp | 1 } < 0);
#ok (($cusp ^ 1) > 0 && do { use integer; $cusp ^ 1 } < 0);
#ok ((1 << ($bits - 1)) == $cusp &&
#    do { use integer; 1 << ($bits - 1) } == -$cusp);
#ok (($cusp >> 1) == ($cusp / 2) &&
#    do { use integer; abs($cusp >> 1) } == ($cusp / 2));

#--
#$Aaz = chr(ord("A") & ord("z"));
#$Aoz = chr(ord("A") | ord("z"));
#$Axz = chr(ord("A") ^ ord("z"));
# instead of $Aaz x 5, literal "@@@@@" is used and thus ascii assumed below
# (for now...)

# short strings
is ("AAAAA" ~& "zzzzz", '@@@@@');
is ("AAAAA" ~| "zzzzz", '{{{{{');
is ("AAAAA" ~^ "zzzzz", ';;;;;');

# long strings
my $foo = "A" x 150;
my $bar = "z" x 75;
my $zap = "A" x 75;
# & truncates
is ($foo ~& $bar, '@' x 75);
# | does not truncate
is ($foo ~| $bar, '{' x 75 ~ $zap);
# ^ does not truncate
is ($foo ~^ $bar, ';' x 75 ~ $zap);


# These ok numbers make absolutely no sense in pugs test suite :)
# 
is("ok \xFF\xFF\n" ~& "ok 19\n", "ok 19\n");
is("ok 20\n" ~| "ok \0\0\n", "ok 20\n");

# currently, pugs recognize octals as "\0o00", not "\o000".
#if ("o\o000 \0" ~ "1\o000" ~^ "\o000k\02\o000\n" eq "ok 21\n") { say "ok 15" } else { say "not ok 15" }

# Pugs does not have \x{}

#
#if ("ok \x{FF}\x{FF}\n" ~& "ok 22\n" eq "ok 22\n") { say "ok 16" } else { say "not ok 16" }
#if ("ok 23\n" ~| "ok \x{0}\x{0}\n" eq "ok 23\n") { say "ok 17" } else { say "not ok 17" }
#if ("o\x{0} \x{0}4\x{0}" ~^ "\x{0}k\x{0}2\x{0}\n" eq "ok 24\n") { say "ok 18" } else { say "not ok 18" }

# Not in Pugs: vstrings, ebcdic, unicode, sprintf

# More variations on 19 and 22
#if ("ok \xFF\x{FF}\n" ~& "ok 41\n" eq "ok 41\n") { say "ok 19" } else { say "not ok 19" }
#if ("ok \x{FF}\xFF\n" ~& "ok 42\n" eq "ok 42\n") { say "ok 20" } else { say "not ok 20" }

# Tests to see if you really can do casts negative floats to unsigned properly
my $neg1 = -1.0;
is(+^ $neg1, 0);
my $neg7 = -7.0;
is(+^ $neg7, 6);
ok(+^ $neg7 == 6); # weird == parsing bug: XXX



