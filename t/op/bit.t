use v6;

# Mostly copied from Perl 5.8.4 s t/op/bop.t

say "1..17";

# test the bit operators '&', '|', '^', '~', '<<', and '>>'

# numerics
if (0xdead +& 0xbeef == 0x9ead) { say "ok 1" } else { say "not ok 1" }
if (0xdead +| 0xbeef == 0xfeef) { say "ok 2" } else { say "not ok 2" }
if (0xdead +^ 0xbeef == 0x6042) { say "ok 3" } else { say "not ok 3" }
if (+^0xdead +& 0xbeef == 0x2042) { say "ok 4" } else { say "not ok 4 # TODO" }

# shifts
if ((257 +< 7) == 32896) { say "ok 5" } else { say "not ok 5" }
if ((33023 +> 7) == 257) { say "ok 6" } else { say "not ok 6" }

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
if ("AAAAA" ~& "zzzzz" eq "@@@@@") { say "ok 7" } else { say "not ok 7" }
if ("AAAAA" ~| "zzzzz" eq "{{{{{") { say "ok 8" } else { say "not ok 8" }
if ("AAAAA" ~^ "zzzzz" eq ";;;;;") { say "ok 9" } else { say "not ok 9" }

# long strings
my $foo = "A" x 150;
my $bar = "z" x 75;
my $zap = "A" x 75;
# & truncates
if ($foo ~& $bar eq "@" x 75) { say "ok 10" } else { say "not ok 10" }
# | does not truncate
if ($foo ~| $bar eq "{" x 75 ~ $zap) { say "ok 11" } else { say "not ok 11 # TODO truncate" }
# ^ does not truncate
if ($foo ~^ $bar eq ";" x 75 ~ $zap) { say "ok 12" } else { say "not ok 12 # TODO truncate" }


# These ok numbers make absolutely no sense in pugs test suite :)
# 
if ("ok \xFF\xFF\n" ~& "ok 19\n" eq "ok 19\n") { say "ok 13" } else { say "not ok 13" }
if ("ok 20\n" ~| "ok \0\0\n" eq "ok 20\n") { say "ok 14" } else { say "not ok 14" }
if ("o\000 \0001\000" ~^ "\000k\0002\000\n" eq "ok 21\n") { say "ok 15" } else { say "not ok 15 # TODO" }

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
if (+^ $neg1 == 0) { say "ok 16" } else { say "not ok 16" }
my $neg7 = -7.0;
if (+^ $neg7 == 6) { say "ok 17" } else { say "not ok 17" }



