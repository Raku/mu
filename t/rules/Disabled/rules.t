#!/usr/bin/pugs

# This file has been generated from re_tests-file (in perl5-sources).
# Original lines from re_tests are in comments

use v6;
use Test;

plan 1581;


# I don't know how to get the equivalent of $-[$n] in perl6,
# so this code uses following (yet-undefined) function for those tests:
#    getpos($/, $n) == $-[$n] from perl5
#
# This is just in case these tests might be of some value. (IMHO they are.)
# Of course if these tests are deemed unwanted, they can be easily removed.
# (by grepping for getpos)


# Tests from re_tests in perl5-source

# --- re_tests ---

# 1: abc	abc	y	$&	abc
# 2: abc	abc	y	$-[0]	0
# 3: abc	abc	y	$+[0]	3 # SKIP
is(eval '"abc" ~~ rx:perl5/abc/ && $0', "abc", 're_tests 1/0 (#1)', :todo);
is(eval '"abc" ~~ rx:perl5/abc/ && getpos($/, 0)', 0, 're_tests 1/0 (#2)', :todo);
is(eval '"abc" ~~ rx/abc/ && $0', "abc", 're_tests 2/0 (#3)', :todo);
is(eval '"abc" ~~ rx/abc/ && getpos($/, 0)', 0, 're_tests 2/0 (#4)', :todo);
# 4: abc	xbc	n	-	-
ok(eval 'not ("xbc" ~~ rx:perl5/abc/)', 're_tests 3  (#5)', :todo);
ok(eval 'not ("xbc" ~~ rx/abc/)', 're_tests 4  (#6)', :todo);
# 5: abc	axc	n	-	-
ok(eval 'not ("axc" ~~ rx:perl5/abc/)', 're_tests 5  (#7)', :todo);
ok(eval 'not ("axc" ~~ rx/abc/)', 're_tests 6  (#8)', :todo);
# 6: abc	abx	n	-	-
ok(eval 'not ("abx" ~~ rx:perl5/abc/)', 're_tests 7  (#9)', :todo);
ok(eval 'not ("abx" ~~ rx/abc/)', 're_tests 8  (#10)', :todo);
# 7: abc	xabcy	y	$&	abc
# 8: abc	xabcy	y	$-[0]	1
# 9: abc	xabcy	y	$+[0]	4 # SKIP
is(eval '"xabcy" ~~ rx:perl5/abc/ && $0', "abc", 're_tests 9/0 (#11)', :todo);
is(eval '"xabcy" ~~ rx:perl5/abc/ && getpos($/, 0)', 1, 're_tests 9/0 (#12)', :todo);
is(eval '"xabcy" ~~ rx/abc/ && $0', "abc", 're_tests 10/0 (#13)', :todo);
is(eval '"xabcy" ~~ rx/abc/ && getpos($/, 0)', 1, 're_tests 10/0 (#14)', :todo);
# 10: abc	ababc	y	$&	abc
# 11: abc	ababc	y	$-[0]	2
# 12: abc	ababc	y	$+[0]	5 # SKIP
is(eval '"ababc" ~~ rx:perl5/abc/ && $0', "abc", 're_tests 11/0 (#15)', :todo);
is(eval '"ababc" ~~ rx:perl5/abc/ && getpos($/, 0)', 2, 're_tests 11/0 (#16)', :todo);
is(eval '"ababc" ~~ rx/abc/ && $0', "abc", 're_tests 12/0 (#17)', :todo);
is(eval '"ababc" ~~ rx/abc/ && getpos($/, 0)', 2, 're_tests 12/0 (#18)', :todo);
# 13: ab*c	abc	y	$&	abc
# 14: ab*c	abc	y	$-[0]	0
# 15: ab*c	abc	y	$+[0]	3 # SKIP
is(eval '"abc" ~~ rx:perl5/ab*c/ && $0', "abc", 're_tests 13/0 (#19)', :todo);
is(eval '"abc" ~~ rx:perl5/ab*c/ && getpos($/, 0)', 0, 're_tests 13/0 (#20)', :todo);
is(eval '"abc" ~~ rx/ab*c/ && $0', "abc", 're_tests 14/0 (#21)', :todo);
is(eval '"abc" ~~ rx/ab*c/ && getpos($/, 0)', 0, 're_tests 14/0 (#22)', :todo);
# 16: ab*bc	abc	y	$&	abc
# 17: ab*bc	abc	y	$-[0]	0
# 18: ab*bc	abc	y	$+[0]	3 # SKIP
is(eval '"abc" ~~ rx:perl5/ab*bc/ && $0', "abc", 're_tests 15/0 (#23)', :todo);
is(eval '"abc" ~~ rx:perl5/ab*bc/ && getpos($/, 0)', 0, 're_tests 15/0 (#24)', :todo);
is(eval '"abc" ~~ rx/ab*bc/ && $0', "abc", 're_tests 16/0 (#25)', :todo);
is(eval '"abc" ~~ rx/ab*bc/ && getpos($/, 0)', 0, 're_tests 16/0 (#26)', :todo);
# 19: ab*bc	abbc	y	$&	abbc
# 20: ab*bc	abbc	y	$-[0]	0
# 21: ab*bc	abbc	y	$+[0]	4 # SKIP
is(eval '"abbc" ~~ rx:perl5/ab*bc/ && $0', "abbc", 're_tests 17/0 (#27)', :todo);
is(eval '"abbc" ~~ rx:perl5/ab*bc/ && getpos($/, 0)', 0, 're_tests 17/0 (#28)', :todo);
is(eval '"abbc" ~~ rx/ab*bc/ && $0', "abbc", 're_tests 18/0 (#29)', :todo);
is(eval '"abbc" ~~ rx/ab*bc/ && getpos($/, 0)', 0, 're_tests 18/0 (#30)', :todo);
# 22: ab*bc	abbbbc	y	$&	abbbbc
# 23: ab*bc	abbbbc	y	$-[0]	0
# 24: ab*bc	abbbbc	y	$+[0]	6 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/ab*bc/ && $0', "abbbbc", 're_tests 19/0 (#31)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/ab*bc/ && getpos($/, 0)', 0, 're_tests 19/0 (#32)', :todo);
is(eval '"abbbbc" ~~ rx/ab*bc/ && $0', "abbbbc", 're_tests 20/0 (#33)', :todo);
is(eval '"abbbbc" ~~ rx/ab*bc/ && getpos($/, 0)', 0, 're_tests 20/0 (#34)', :todo);
# 25: .{1}	abbbbc	y	$&	a
# 26: .{1}	abbbbc	y	$-[0]	0
# 27: .{1}	abbbbc	y	$+[0]	1 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/.{1}/ && $0', "a", 're_tests 21/0 (#35)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/.{1}/ && getpos($/, 0)', 0, 're_tests 21/0 (#36)', :todo);
is(eval '"abbbbc" ~~ rx/\N**{1}/ && $0', "a", 're_tests 22/0 (#37)', :todo);
is(eval '"abbbbc" ~~ rx/\N**{1}/ && getpos($/, 0)', 0, 're_tests 22/0 (#38)', :todo);
# 28: .{3,4}	abbbbc	y	$&	abbb
# 29: .{3,4}	abbbbc	y	$-[0]	0
# 30: .{3,4}	abbbbc	y	$+[0]	4 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/.{3,4}/ && $0', "abbb", 're_tests 23/0 (#39)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/.{3,4}/ && getpos($/, 0)', 0, 're_tests 23/0 (#40)', :todo);
is(eval '"abbbbc" ~~ rx/\N**{3..4}/ && $0', "abbb", 're_tests 24/0 (#41)', :todo);
is(eval '"abbbbc" ~~ rx/\N**{3..4}/ && getpos($/, 0)', 0, 're_tests 24/0 (#42)', :todo);
# 31: ab{0,}bc	abbbbc	y	$&	abbbbc
# 32: ab{0,}bc	abbbbc	y	$-[0]	0
# 33: ab{0,}bc	abbbbc	y	$+[0]	6 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/ab{0,}bc/ && $0', "abbbbc", 're_tests 25/0 (#43)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/ab{0,}bc/ && getpos($/, 0)', 0, 're_tests 25/0 (#44)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{0...}bc/ && $0', "abbbbc", 're_tests 26/0 (#45)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{0...}bc/ && getpos($/, 0)', 0, 're_tests 26/0 (#46)', :todo);
# 34: ab+bc	abbc	y	$&	abbc
# 35: ab+bc	abbc	y	$-[0]	0
# 36: ab+bc	abbc	y	$+[0]	4 # SKIP
is(eval '"abbc" ~~ rx:perl5/ab+bc/ && $0', "abbc", 're_tests 27/0 (#47)', :todo);
is(eval '"abbc" ~~ rx:perl5/ab+bc/ && getpos($/, 0)', 0, 're_tests 27/0 (#48)', :todo);
is(eval '"abbc" ~~ rx/ab+bc/ && $0', "abbc", 're_tests 28/0 (#49)', :todo);
is(eval '"abbc" ~~ rx/ab+bc/ && getpos($/, 0)', 0, 're_tests 28/0 (#50)', :todo);
# 37: ab+bc	abc	n	-	-
ok(eval 'not ("abc" ~~ rx:perl5/ab+bc/)', 're_tests 29  (#51)', :todo);
ok(eval 'not ("abc" ~~ rx/ab+bc/)', 're_tests 30  (#52)', :todo);
# 38: ab+bc	abq	n	-	-
ok(eval 'not ("abq" ~~ rx:perl5/ab+bc/)', 're_tests 31  (#53)', :todo);
ok(eval 'not ("abq" ~~ rx/ab+bc/)', 're_tests 32  (#54)', :todo);
# 39: ab{1,}bc	abq	n	-	-
ok(eval 'not ("abq" ~~ rx:perl5/ab{1,}bc/)', 're_tests 33  (#55)', :todo);
ok(eval 'not ("abq" ~~ rx/ab**{1...}bc/)', 're_tests 34  (#56)', :todo);
# 40: ab+bc	abbbbc	y	$&	abbbbc
# 41: ab+bc	abbbbc	y	$-[0]	0
# 42: ab+bc	abbbbc	y	$+[0]	6 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/ab+bc/ && $0', "abbbbc", 're_tests 35/0 (#57)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/ab+bc/ && getpos($/, 0)', 0, 're_tests 35/0 (#58)', :todo);
is(eval '"abbbbc" ~~ rx/ab+bc/ && $0', "abbbbc", 're_tests 36/0 (#59)', :todo);
is(eval '"abbbbc" ~~ rx/ab+bc/ && getpos($/, 0)', 0, 're_tests 36/0 (#60)', :todo);
# 43: ab{1,}bc	abbbbc	y	$&	abbbbc
# 44: ab{1,}bc	abbbbc	y	$-[0]	0
# 45: ab{1,}bc	abbbbc	y	$+[0]	6 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/ab{1,}bc/ && $0', "abbbbc", 're_tests 37/0 (#61)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/ab{1,}bc/ && getpos($/, 0)', 0, 're_tests 37/0 (#62)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{1...}bc/ && $0', "abbbbc", 're_tests 38/0 (#63)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{1...}bc/ && getpos($/, 0)', 0, 're_tests 38/0 (#64)', :todo);
# 46: ab{1,3}bc	abbbbc	y	$&	abbbbc
# 47: ab{1,3}bc	abbbbc	y	$-[0]	0
# 48: ab{1,3}bc	abbbbc	y	$+[0]	6 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/ab{1,3}bc/ && $0', "abbbbc", 're_tests 39/0 (#65)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/ab{1,3}bc/ && getpos($/, 0)', 0, 're_tests 39/0 (#66)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{1..3}bc/ && $0', "abbbbc", 're_tests 40/0 (#67)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{1..3}bc/ && getpos($/, 0)', 0, 're_tests 40/0 (#68)', :todo);
# 49: ab{3,4}bc	abbbbc	y	$&	abbbbc
# 50: ab{3,4}bc	abbbbc	y	$-[0]	0
# 51: ab{3,4}bc	abbbbc	y	$+[0]	6 # SKIP
is(eval '"abbbbc" ~~ rx:perl5/ab{3,4}bc/ && $0', "abbbbc", 're_tests 41/0 (#69)', :todo);
is(eval '"abbbbc" ~~ rx:perl5/ab{3,4}bc/ && getpos($/, 0)', 0, 're_tests 41/0 (#70)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{3..4}bc/ && $0', "abbbbc", 're_tests 42/0 (#71)', :todo);
is(eval '"abbbbc" ~~ rx/ab**{3..4}bc/ && getpos($/, 0)', 0, 're_tests 42/0 (#72)', :todo);
# 52: ab{4,5}bc	abbbbc	n	-	-
ok(eval 'not ("abbbbc" ~~ rx:perl5/ab{4,5}bc/)', 're_tests 43  (#73)', :todo);
ok(eval 'not ("abbbbc" ~~ rx/ab**{4..5}bc/)', 're_tests 44  (#74)', :todo);
# 53: ab?bc	abbc	y	$&	abbc
is(eval '"abbc" ~~ rx:perl5/ab?bc/ && $0', "abbc", 're_tests 45/0 (#75)', :todo);
is(eval '"abbc" ~~ rx/ab?bc/ && $0', "abbc", 're_tests 46/0 (#76)', :todo);
# 54: ab?bc	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/ab?bc/ && $0', "abc", 're_tests 47/0 (#77)', :todo);
is(eval '"abc" ~~ rx/ab?bc/ && $0', "abc", 're_tests 48/0 (#78)', :todo);
# 55: ab{0,1}bc	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/ab{0,1}bc/ && $0', "abc", 're_tests 49/0 (#79)', :todo);
is(eval '"abc" ~~ rx/ab**{0..1}bc/ && $0', "abc", 're_tests 50/0 (#80)', :todo);
# 56: ab?bc	abbbbc	n	-	-
ok(eval 'not ("abbbbc" ~~ rx:perl5/ab?bc/)', 're_tests 51  (#81)', :todo);
ok(eval 'not ("abbbbc" ~~ rx/ab?bc/)', 're_tests 52  (#82)', :todo);
# 57: ab?c	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/ab?c/ && $0', "abc", 're_tests 53/0 (#83)', :todo);
is(eval '"abc" ~~ rx/ab?c/ && $0', "abc", 're_tests 54/0 (#84)', :todo);
# 58: ab{0,1}c	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/ab{0,1}c/ && $0', "abc", 're_tests 55/0 (#85)', :todo);
is(eval '"abc" ~~ rx/ab**{0..1}c/ && $0', "abc", 're_tests 56/0 (#86)', :todo);
# 59: ^abc$	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/^abc$/ && $0', "abc", 're_tests 57/0 (#87)', :todo);
is(eval '"abc" ~~ rx/^abc$/ && $0', "abc", 're_tests 58/0 (#88)', :todo);
# 60: ^abc$	abcc	n	-	-
ok(eval 'not ("abcc" ~~ rx:perl5/^abc$/)', 're_tests 59  (#89)', :todo);
ok(eval 'not ("abcc" ~~ rx/^abc$/)', 're_tests 60  (#90)', :todo);
# 61: ^abc	abcc	y	$&	abc
is(eval '"abcc" ~~ rx:perl5/^abc/ && $0', "abc", 're_tests 61/0 (#91)', :todo);
is(eval '"abcc" ~~ rx/^abc/ && $0', "abc", 're_tests 62/0 (#92)', :todo);
# 62: ^abc$	aabc	n	-	-
ok(eval 'not ("aabc" ~~ rx:perl5/^abc$/)', 're_tests 63  (#93)', :todo);
ok(eval 'not ("aabc" ~~ rx/^abc$/)', 're_tests 64  (#94)', :todo);
# 63: abc$	aabc	y	$&	abc
is(eval '"aabc" ~~ rx:perl5/abc$/ && $0', "abc", 're_tests 65/0 (#95)', :todo);
is(eval '"aabc" ~~ rx/abc$/ && $0', "abc", 're_tests 66/0 (#96)', :todo);
# 64: abc$	aabcd	n	-	-
ok(eval 'not ("aabcd" ~~ rx:perl5/abc$/)', 're_tests 67  (#97)', :todo);
ok(eval 'not ("aabcd" ~~ rx/abc$/)', 're_tests 68  (#98)', :todo);
# 65: ^	abc	y	$&	
is(eval '"abc" ~~ rx:perl5/^/ && $0', "", 're_tests 69/0 (#99)', :todo);
is(eval '"abc" ~~ rx/^/ && $0', "", 're_tests 70/0 (#100)', :todo);
# 66: $	abc	y	$&	
is(eval '"abc" ~~ rx:perl5/$/ && $0', "", 're_tests 71/0 (#101)', :todo);
is(eval '"abc" ~~ rx/$/ && $0', "", 're_tests 72/0 (#102)', :todo);
# 67: a.c	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/a.c/ && $0', "abc", 're_tests 73/0 (#103)', :todo);
is(eval '"abc" ~~ rx/a\Nc/ && $0', "abc", 're_tests 74/0 (#104)', :todo);
# 68: a.c	axc	y	$&	axc
is(eval '"axc" ~~ rx:perl5/a.c/ && $0', "axc", 're_tests 75/0 (#105)', :todo);
is(eval '"axc" ~~ rx/a\Nc/ && $0', "axc", 're_tests 76/0 (#106)', :todo);
# 69: a.*c	axyzc	y	$&	axyzc
is(eval '"axyzc" ~~ rx:perl5/a.*c/ && $0', "axyzc", 're_tests 77/0 (#107)', :todo);
is(eval '"axyzc" ~~ rx/a\N*c/ && $0', "axyzc", 're_tests 78/0 (#108)', :todo);
# 70: a.*c	axyzd	n	-	-
ok(eval 'not ("axyzd" ~~ rx:perl5/a.*c/)', 're_tests 79  (#109)', :todo);
ok(eval 'not ("axyzd" ~~ rx/a\N*c/)', 're_tests 80  (#110)', :todo);
# 71: a[bc]d	abc	n	-	-
ok(eval 'not ("abc" ~~ rx:perl5/a[bc]d/)', 're_tests 81  (#111)', :todo);
ok(eval 'not ("abc" ~~ rx/a<[bc]>d/)', 're_tests 82  (#112)', :todo);
# 72: a[bc]d	abd	y	$&	abd
is(eval '"abd" ~~ rx:perl5/a[bc]d/ && $0', "abd", 're_tests 83/0 (#113)', :todo);
is(eval '"abd" ~~ rx/a<[bc]>d/ && $0', "abd", 're_tests 84/0 (#114)', :todo);
# 73: a[b-d]e	abd	n	-	-
ok(eval 'not ("abd" ~~ rx:perl5/a[b-d]e/)', 're_tests 85  (#115)', :todo);
ok(eval 'not ("abd" ~~ rx/a<[b-d]>e/)', 're_tests 86  (#116)', :todo);
# 74: a[b-d]e	ace	y	$&	ace
is(eval '"ace" ~~ rx:perl5/a[b-d]e/ && $0', "ace", 're_tests 87/0 (#117)', :todo);
is(eval '"ace" ~~ rx/a<[b-d]>e/ && $0', "ace", 're_tests 88/0 (#118)', :todo);
# 75: a[b-d]	aac	y	$&	ac
is(eval '"aac" ~~ rx:perl5/a[b-d]/ && $0', "ac", 're_tests 89/0 (#119)', :todo);
is(eval '"aac" ~~ rx/a<[b-d]>/ && $0', "ac", 're_tests 90/0 (#120)', :todo);
# 76: a[-b]	a-	y	$&	a-
is(eval '"a-" ~~ rx:perl5/a[-b]/ && $0', "a-", 're_tests 91/0 (#121)', :todo);
is(eval '"a-" ~~ rx/a<[-b]>/ && $0', "a-", 're_tests 92/0 (#122)', :todo);
# 77: a[b-]	a-	y	$&	a-
is(eval '"a-" ~~ rx:perl5/a[b-]/ && $0', "a-", 're_tests 93/0 (#123)', :todo);
is(eval '"a-" ~~ rx/a<[b-]>/ && $0', "a-", 're_tests 94/0 (#124)', :todo);
# 78: a[b-a]	-	c	-	Invalid [] range "b-a"
# -- SKIPPED - TESTS ERROR MESSAGE
# 79: a[]b	-	c	-	Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 80: a[	-	c	-	Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 81: a]	a]	y	$&	a]
is(eval '"a]" ~~ rx:perl5/a]/ && $0', "a]", 're_tests 95/0 (#125)', :todo);
is(eval '"a]" ~~ rx/a]/ && $0', "a]", 're_tests 96/0 (#126)', :todo);
# 82: a[]]b	a]b	y	$&	a]b
is(eval '"a]b" ~~ rx:perl5/a[]]b/ && $0', "a]b", 're_tests 97/0 (#127)', :todo);
is(eval '"a]b" ~~ rx/a<[]>]b/ && $0', "a]b", 're_tests 98/0 (#128)', :todo);
# 83: a[^bc]d	aed	y	$&	aed
is(eval '"aed" ~~ rx:perl5/a[^bc]d/ && $0', "aed", 're_tests 99/0 (#129)', :todo);
is(eval '"aed" ~~ rx/a<[^bc]>d/ && $0', "aed", 're_tests 100/0 (#130)', :todo);
# 84: a[^bc]d	abd	n	-	-
ok(eval 'not ("abd" ~~ rx:perl5/a[^bc]d/)', 're_tests 101  (#131)', :todo);
ok(eval 'not ("abd" ~~ rx/a<[^bc]>d/)', 're_tests 102  (#132)', :todo);
# 85: a[^-b]c	adc	y	$&	adc
is(eval '"adc" ~~ rx:perl5/a[^-b]c/ && $0', "adc", 're_tests 103/0 (#133)', :todo);
is(eval '"adc" ~~ rx/a<[^-b]>c/ && $0', "adc", 're_tests 104/0 (#134)', :todo);
# 86: a[^-b]c	a-c	n	-	-
ok(eval 'not ("a-c" ~~ rx:perl5/a[^-b]c/)', 're_tests 105  (#135)', :todo);
ok(eval 'not ("a-c" ~~ rx/a<[^-b]>c/)', 're_tests 106  (#136)', :todo);
# 87: a[^]b]c	a]c	n	-	-
ok(eval 'not ("a]c" ~~ rx:perl5/a[^]b]c/)', 're_tests 107  (#137)', :todo);
ok(eval 'not ("a]c" ~~ rx/a<[^]>b]c/)', 're_tests 108  (#138)', :todo);
# 88: a[^]b]c	adc	y	$&	adc
is(eval '"adc" ~~ rx:perl5/a[^]b]c/ && $0', "adc", 're_tests 109/0 (#139)', :todo);
is(eval '"adc" ~~ rx/a<[^]>b]c/ && $0', "adc", 're_tests 110/0 (#140)', :todo);
# 89: \ba\b	a-	y	-	-
ok(eval '"a-" ~~ rx:perl5/\ba\b/', 're_tests 111  (#141)', :todo);
ok(eval '"a-" ~~ rx/\ba\b/', 're_tests 112  (#142)', :todo);
# 90: \ba\b	-a	y	-	-
ok(eval '"-a" ~~ rx:perl5/\ba\b/', 're_tests 113  (#143)', :todo);
ok(eval '"-a" ~~ rx/\ba\b/', 're_tests 114  (#144)', :todo);
# 91: \ba\b	-a-	y	-	-
ok(eval '"-a-" ~~ rx:perl5/\ba\b/', 're_tests 115  (#145)', :todo);
ok(eval '"-a-" ~~ rx/\ba\b/', 're_tests 116  (#146)', :todo);
# 92: \by\b	xy	n	-	-
ok(eval 'not ("xy" ~~ rx:perl5/\by\b/)', 're_tests 117  (#147)', :todo);
ok(eval 'not ("xy" ~~ rx/\by\b/)', 're_tests 118  (#148)', :todo);
# 93: \by\b	yz	n	-	-
ok(eval 'not ("yz" ~~ rx:perl5/\by\b/)', 're_tests 119  (#149)', :todo);
ok(eval 'not ("yz" ~~ rx/\by\b/)', 're_tests 120  (#150)', :todo);
# 94: \by\b	xyz	n	-	-
ok(eval 'not ("xyz" ~~ rx:perl5/\by\b/)', 're_tests 121  (#151)', :todo);
ok(eval 'not ("xyz" ~~ rx/\by\b/)', 're_tests 122  (#152)', :todo);
# 95: \Ba\B	a-	n	-	-
ok(eval 'not ("a-" ~~ rx:perl5/\Ba\B/)', 're_tests 123  (#153)', :todo);
ok(eval 'not ("a-" ~~ rx/\Ba\B/)', 're_tests 124  (#154)', :todo);
# 96: \Ba\B	-a	n	-	-
ok(eval 'not ("-a" ~~ rx:perl5/\Ba\B/)', 're_tests 125  (#155)', :todo);
ok(eval 'not ("-a" ~~ rx/\Ba\B/)', 're_tests 126  (#156)', :todo);
# 97: \Ba\B	-a-	n	-	-
ok(eval 'not ("-a-" ~~ rx:perl5/\Ba\B/)', 're_tests 127  (#157)', :todo);
ok(eval 'not ("-a-" ~~ rx/\Ba\B/)', 're_tests 128  (#158)', :todo);
# 98: \By\b	xy	y	-	-
ok(eval '"xy" ~~ rx:perl5/\By\b/', 're_tests 129  (#159)', :todo);
ok(eval '"xy" ~~ rx/\By\b/', 're_tests 130  (#160)', :todo);
# 99: \By\b	xy	y	$-[0]	1
# 100: \By\b	xy	y	$+[0]	2 # SKIP
is(eval '"xy" ~~ rx:perl5/\By\b/ && getpos($/, 0)', 1, 're_tests 131/0 (#161)', :todo);
is(eval '"xy" ~~ rx/\By\b/ && getpos($/, 0)', 1, 're_tests 132/0 (#162)', :todo);
# 101: \By\b	xy	y	-	-
ok(eval '"xy" ~~ rx:perl5/\By\b/', 're_tests 133  (#163)', :todo);
ok(eval '"xy" ~~ rx/\By\b/', 're_tests 134  (#164)', :todo);
# 102: \by\B	yz	y	-	-
ok(eval '"yz" ~~ rx:perl5/\by\B/', 're_tests 135  (#165)', :todo);
ok(eval '"yz" ~~ rx/\by\B/', 're_tests 136  (#166)', :todo);
# 103: \By\B	xyz	y	-	-
ok(eval '"xyz" ~~ rx:perl5/\By\B/', 're_tests 137  (#167)', :todo);
ok(eval '"xyz" ~~ rx/\By\B/', 're_tests 138  (#168)', :todo);
# 104: \w	a	y	-	-
ok(eval '"a" ~~ rx:perl5/\w/', 're_tests 139  (#169)', :todo);
ok(eval '"a" ~~ rx/\w/', 're_tests 140  (#170)', :todo);
# 105: \w	-	n	-	-
ok(eval 'not ("-" ~~ rx:perl5/\w/)', 're_tests 141  (#171)', :todo);
ok(eval 'not ("-" ~~ rx/\w/)', 're_tests 142  (#172)', :todo);
# 106: \W	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/\W/)', 're_tests 143  (#173)', :todo);
ok(eval 'not ("a" ~~ rx/\W/)', 're_tests 144  (#174)', :todo);
# 107: \W	-	y	-	-
ok(eval '"-" ~~ rx:perl5/\W/', 're_tests 145  (#175)', :todo);
ok(eval '"-" ~~ rx/\W/', 're_tests 146  (#176)', :todo);
# 108: a\sb	a b	y	-	-
ok(eval '"a b" ~~ rx:perl5/a\sb/', 're_tests 147  (#177)', :todo);
ok(eval '"a b" ~~ rx/a\sb/', 're_tests 148  (#178)', :todo);
# 109: a\sb	a-b	n	-	-
ok(eval 'not ("a-b" ~~ rx:perl5/a\sb/)', 're_tests 149  (#179)', :todo);
ok(eval 'not ("a-b" ~~ rx/a\sb/)', 're_tests 150  (#180)', :todo);
# 110: a\Sb	a b	n	-	-
ok(eval 'not ("a b" ~~ rx:perl5/a\Sb/)', 're_tests 151  (#181)', :todo);
ok(eval 'not ("a b" ~~ rx/a\Sb/)', 're_tests 152  (#182)', :todo);
# 111: a\Sb	a-b	y	-	-
ok(eval '"a-b" ~~ rx:perl5/a\Sb/', 're_tests 153  (#183)', :todo);
ok(eval '"a-b" ~~ rx/a\Sb/', 're_tests 154  (#184)', :todo);
# 112: \d	1	y	-	-
ok(eval '"1" ~~ rx:perl5/\d/', 're_tests 155  (#185)', :todo);
ok(eval '"1" ~~ rx/\d/', 're_tests 156  (#186)', :todo);
# 113: \d	-	n	-	-
ok(eval 'not ("-" ~~ rx:perl5/\d/)', 're_tests 157  (#187)', :todo);
ok(eval 'not ("-" ~~ rx/\d/)', 're_tests 158  (#188)', :todo);
# 114: \D	1	n	-	-
ok(eval 'not ("1" ~~ rx:perl5/\D/)', 're_tests 159  (#189)', :todo);
ok(eval 'not ("1" ~~ rx/\D/)', 're_tests 160  (#190)', :todo);
# 115: \D	-	y	-	-
ok(eval '"-" ~~ rx:perl5/\D/', 're_tests 161  (#191)', :todo);
ok(eval '"-" ~~ rx/\D/', 're_tests 162  (#192)', :todo);
# 116: [\w]	a	y	-	-
ok(eval '"a" ~~ rx:perl5/[\w]/', 're_tests 163  (#193)', :todo);
ok(eval '"a" ~~ rx/<[\w]>/', 're_tests 164  (#194)', :todo);
# 117: [\w]	-	n	-	-
ok(eval 'not ("-" ~~ rx:perl5/[\w]/)', 're_tests 165  (#195)', :todo);
ok(eval 'not ("-" ~~ rx/<[\w]>/)', 're_tests 166  (#196)', :todo);
# 118: [\W]	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/[\W]/)', 're_tests 167  (#197)', :todo);
ok(eval 'not ("a" ~~ rx/<[\W]>/)', 're_tests 168  (#198)', :todo);
# 119: [\W]	-	y	-	-
ok(eval '"-" ~~ rx:perl5/[\W]/', 're_tests 169  (#199)', :todo);
ok(eval '"-" ~~ rx/<[\W]>/', 're_tests 170  (#200)', :todo);
# 120: a[\s]b	a b	y	-	-
ok(eval '"a b" ~~ rx:perl5/a[\s]b/', 're_tests 171  (#201)', :todo);
ok(eval '"a b" ~~ rx/a<[\s]>b/', 're_tests 172  (#202)', :todo);
# 121: a[\s]b	a-b	n	-	-
ok(eval 'not ("a-b" ~~ rx:perl5/a[\s]b/)', 're_tests 173  (#203)', :todo);
ok(eval 'not ("a-b" ~~ rx/a<[\s]>b/)', 're_tests 174  (#204)', :todo);
# 122: a[\S]b	a b	n	-	-
ok(eval 'not ("a b" ~~ rx:perl5/a[\S]b/)', 're_tests 175  (#205)', :todo);
ok(eval 'not ("a b" ~~ rx/a<[\S]>b/)', 're_tests 176  (#206)', :todo);
# 123: a[\S]b	a-b	y	-	-
ok(eval '"a-b" ~~ rx:perl5/a[\S]b/', 're_tests 177  (#207)', :todo);
ok(eval '"a-b" ~~ rx/a<[\S]>b/', 're_tests 178  (#208)', :todo);
# 124: [\d]	1	y	-	-
ok(eval '"1" ~~ rx:perl5/[\d]/', 're_tests 179  (#209)', :todo);
ok(eval '"1" ~~ rx/<[\d]>/', 're_tests 180  (#210)', :todo);
# 125: [\d]	-	n	-	-
ok(eval 'not ("-" ~~ rx:perl5/[\d]/)', 're_tests 181  (#211)', :todo);
ok(eval 'not ("-" ~~ rx/<[\d]>/)', 're_tests 182  (#212)', :todo);
# 126: [\D]	1	n	-	-
ok(eval 'not ("1" ~~ rx:perl5/[\D]/)', 're_tests 183  (#213)', :todo);
ok(eval 'not ("1" ~~ rx/<[\D]>/)', 're_tests 184  (#214)', :todo);
# 127: [\D]	-	y	-	-
ok(eval '"-" ~~ rx:perl5/[\D]/', 're_tests 185  (#215)', :todo);
ok(eval '"-" ~~ rx/<[\D]>/', 're_tests 186  (#216)', :todo);
# 128: ab|cd	abc	y	$&	ab
is(eval '"abc" ~~ rx:perl5/ab|cd/ && $0', "ab", 're_tests 187/0 (#217)', :todo);
is(eval '"abc" ~~ rx/ab|cd/ && $0', "ab", 're_tests 188/0 (#218)', :todo);
# 129: ab|cd	abcd	y	$&	ab
is(eval '"abcd" ~~ rx:perl5/ab|cd/ && $0', "ab", 're_tests 189/0 (#219)', :todo);
is(eval '"abcd" ~~ rx/ab|cd/ && $0', "ab", 're_tests 190/0 (#220)', :todo);
# 130: ()ef	def	y	$&-$1	ef-
is(eval '"def" ~~ rx:perl5/()ef/ && $0', "ef", 're_tests 191/0 (#221)', :todo);
is(eval '"def" ~~ rx:perl5/()ef/ && $1', "", 're_tests 191/1 (#222)', :todo);
is(eval '"def" ~~ rx/()ef/ && $0', "ef", 're_tests 192/0 (#223)', :todo);
is(eval '"def" ~~ rx/()ef/ && $1', "", 're_tests 192/1 (#224)', :todo);
# 131: ()ef	def	y	$-[0]	1
# 132: ()ef	def	y	$+[0]	3 # SKIP
is(eval '"def" ~~ rx:perl5/()ef/ && getpos($/, 0)', 1, 're_tests 193/0 (#225)', :todo);
is(eval '"def" ~~ rx/()ef/ && getpos($/, 0)', 1, 're_tests 194/0 (#226)', :todo);
# 133: ()ef	def	y	$-[1]	1
# 134: ()ef	def	y	$+[1]	1 # SKIP
is(eval '"def" ~~ rx:perl5/()ef/ && getpos($/, 1)', 1, 're_tests 195/1 (#227)', :todo);
is(eval '"def" ~~ rx/()ef/ && getpos($/, 1)', 1, 're_tests 196/1 (#228)', :todo);
# 135: *a	-	c	-	Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 136: (*)b	-	c	-	Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 137: $b	b	n	-	-
ok(eval 'not ("b" ~~ rx:perl5/$b/)', 're_tests 197  (#229)', :todo);
ok(eval 'not ("b" ~~ rx/$b/)', 're_tests 198  (#230)', :todo);
# 138: a\	-	c	-	Search pattern not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
# 139: a\(b	a(b	y	$&-$1	a(b-
is(eval '"a(b" ~~ rx:perl5/a\(b/ && $0', "a(b", 're_tests 199/0 (#231)', :todo);
is(eval '"a(b" ~~ rx:perl5/a\(b/ && $1', "", 're_tests 199/1 (#232)', :todo);
is(eval '"a(b" ~~ rx/a\(b/ && $0', "a(b", 're_tests 200/0 (#233)', :todo);
is(eval '"a(b" ~~ rx/a\(b/ && $1', "", 're_tests 200/1 (#234)', :todo);
# 140: a\(*b	ab	y	$&	ab
is(eval '"ab" ~~ rx:perl5/a\(*b/ && $0', "ab", 're_tests 201/0 (#235)', :todo);
is(eval '"ab" ~~ rx/a\(*b/ && $0', "ab", 're_tests 202/0 (#236)', :todo);
# 141: a\(*b	a((b	y	$&	a((b
is(eval '"a((b" ~~ rx:perl5/a\(*b/ && $0', "a((b", 're_tests 203/0 (#237)', :todo);
is(eval '"a((b" ~~ rx/a\(*b/ && $0', "a((b", 're_tests 204/0 (#238)', :todo);
# 142: a\\b	a\b	y	$&	a\b
is(eval '"a\b" ~~ rx:perl5/a\\b/ && $0', "a\b", 're_tests 205/0 (#239)', :todo);
is(eval '"a\b" ~~ rx/a\\b/ && $0', "a\b", 're_tests 206/0 (#240)', :todo);
# 143: abc)	-	c	-	Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 144: (abc	-	c	-	Unmatched (
# -- SKIPPED - TESTS ERROR MESSAGE
# 145: ((a))	abc	y	$&-$1-$2	a-a-a
is(eval '"abc" ~~ rx:perl5/((a))/ && $0', "a", 're_tests 207/0 (#241)', :todo);
is(eval '"abc" ~~ rx:perl5/((a))/ && $1', "a", 're_tests 207/1 (#242)', :todo);
is(eval '"abc" ~~ rx:perl5/((a))/ && $2', "a", 're_tests 207/2 (#243)', :todo);
is(eval '"abc" ~~ rx/((a))/ && $0', "a", 're_tests 208/0 (#244)', :todo);
is(eval '"abc" ~~ rx/((a))/ && $1', "a", 're_tests 208/1 (#245)', :todo);
is(eval '"abc" ~~ rx/((a))/ && $2', "a", 're_tests 208/2 (#246)', :todo);
# 146: ((a))	abc	y	$-[0]-$-[1]-$-[2]	0-0-0
# 147: ((a))	abc	y	$+[0]-$+[1]-$+[2]	1-1-1 # SKIP
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# 148: ((a))	abc	b	@-	0 0 0
# SKIPPED: script doesn't understand `@-' yet
# SKIPPED: script doesn't understand `@-' yet
# 149: ((a))	abc	b	@+	1 1 1
# SKIPPED: script doesn't understand `@+' yet
# SKIPPED: script doesn't understand `@+' yet
# 150: (a)b(c)	abc	y	$&-$1-$2	abc-a-c
is(eval '"abc" ~~ rx:perl5/(a)b(c)/ && $0', "abc", 're_tests 209/0 (#247)', :todo);
is(eval '"abc" ~~ rx:perl5/(a)b(c)/ && $1', "a", 're_tests 209/1 (#248)', :todo);
is(eval '"abc" ~~ rx:perl5/(a)b(c)/ && $2', "c", 're_tests 209/2 (#249)', :todo);
is(eval '"abc" ~~ rx/(a)b(c)/ && $0', "abc", 're_tests 210/0 (#250)', :todo);
is(eval '"abc" ~~ rx/(a)b(c)/ && $1', "a", 're_tests 210/1 (#251)', :todo);
is(eval '"abc" ~~ rx/(a)b(c)/ && $2', "c", 're_tests 210/2 (#252)', :todo);
# 151: (a)b(c)	abc	y	$-[0]-$-[1]-$-[2]	0-0-2
# 152: (a)b(c)	abc	y	$+[0]-$+[1]-$+[2]	3-1-3 # SKIP
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# 153: a+b+c	aabbabc	y	$&	abc
is(eval '"aabbabc" ~~ rx:perl5/a+b+c/ && $0', "abc", 're_tests 211/0 (#253)', :todo);
is(eval '"aabbabc" ~~ rx/a+b+c/ && $0', "abc", 're_tests 212/0 (#254)', :todo);
# 154: a{1,}b{1,}c	aabbabc	y	$&	abc
is(eval '"aabbabc" ~~ rx:perl5/a{1,}b{1,}c/ && $0', "abc", 're_tests 213/0 (#255)', :todo);
is(eval '"aabbabc" ~~ rx/a**{1...}b**{1...}c/ && $0', "abc", 're_tests 214/0 (#256)', :todo);
# 155: a**	-	c	-	Nested quantifiers
# -- SKIPPED - TESTS ERROR MESSAGE
# 156: a.+?c	abcabc	y	$&	abc
is(eval '"abcabc" ~~ rx:perl5/a.+?c/ && $0', "abc", 're_tests 215/0 (#257)', :todo);
is(eval '"abcabc" ~~ rx/a\N+?c/ && $0', "abc", 're_tests 216/0 (#258)', :todo);
# 157: (a+|b)*	ab	y	$&-$1	ab-b
is(eval '"ab" ~~ rx:perl5/(a+|b)*/ && $0', "ab", 're_tests 217/0 (#259)', :todo);
is(eval '"ab" ~~ rx:perl5/(a+|b)*/ && $1', "b", 're_tests 217/1 (#260)', :todo);
is(eval '"ab" ~~ rx/(a+|b)*/ && $0', "ab", 're_tests 218/0 (#261)', :todo);
is(eval '"ab" ~~ rx/(a+|b)*/ && $1', "b", 're_tests 218/1 (#262)', :todo);
# 158: (a+|b)*	ab	y	$-[0]	0
# 159: (a+|b)*	ab	y	$+[0]	2 # SKIP
is(eval '"ab" ~~ rx:perl5/(a+|b)*/ && getpos($/, 0)', 0, 're_tests 219/0 (#263)', :todo);
is(eval '"ab" ~~ rx/(a+|b)*/ && getpos($/, 0)', 0, 're_tests 220/0 (#264)', :todo);
# 160: (a+|b)*	ab	y	$-[1]	1
# 161: (a+|b)*	ab	y	$+[1]	2 # SKIP
is(eval '"ab" ~~ rx:perl5/(a+|b)*/ && getpos($/, 1)', 1, 're_tests 221/1 (#265)', :todo);
is(eval '"ab" ~~ rx/(a+|b)*/ && getpos($/, 1)', 1, 're_tests 222/1 (#266)', :todo);
# 162: (a+|b){0,}	ab	y	$&-$1	ab-b
is(eval '"ab" ~~ rx:perl5/(a+|b){0,}/ && $0', "ab", 're_tests 223/0 (#267)', :todo);
is(eval '"ab" ~~ rx:perl5/(a+|b){0,}/ && $1', "b", 're_tests 223/1 (#268)', :todo);
is(eval '"ab" ~~ rx/(a+|b)**{0...}/ && $0', "ab", 're_tests 224/0 (#269)', :todo);
is(eval '"ab" ~~ rx/(a+|b)**{0...}/ && $1', "b", 're_tests 224/1 (#270)', :todo);
# 163: (a+|b)+	ab	y	$&-$1	ab-b
is(eval '"ab" ~~ rx:perl5/(a+|b)+/ && $0', "ab", 're_tests 225/0 (#271)', :todo);
is(eval '"ab" ~~ rx:perl5/(a+|b)+/ && $1', "b", 're_tests 225/1 (#272)', :todo);
is(eval '"ab" ~~ rx/(a+|b)+/ && $0', "ab", 're_tests 226/0 (#273)', :todo);
is(eval '"ab" ~~ rx/(a+|b)+/ && $1', "b", 're_tests 226/1 (#274)', :todo);
# 164: (a+|b){1,}	ab	y	$&-$1	ab-b
is(eval '"ab" ~~ rx:perl5/(a+|b){1,}/ && $0', "ab", 're_tests 227/0 (#275)', :todo);
is(eval '"ab" ~~ rx:perl5/(a+|b){1,}/ && $1', "b", 're_tests 227/1 (#276)', :todo);
is(eval '"ab" ~~ rx/(a+|b)**{1...}/ && $0', "ab", 're_tests 228/0 (#277)', :todo);
is(eval '"ab" ~~ rx/(a+|b)**{1...}/ && $1', "b", 're_tests 228/1 (#278)', :todo);
# 165: (a+|b)?	ab	y	$&-$1	a-a
is(eval '"ab" ~~ rx:perl5/(a+|b)?/ && $0', "a", 're_tests 229/0 (#279)', :todo);
is(eval '"ab" ~~ rx:perl5/(a+|b)?/ && $1', "a", 're_tests 229/1 (#280)', :todo);
is(eval '"ab" ~~ rx/(a+|b)?/ && $0', "a", 're_tests 230/0 (#281)', :todo);
is(eval '"ab" ~~ rx/(a+|b)?/ && $1', "a", 're_tests 230/1 (#282)', :todo);
# 166: (a+|b){0,1}	ab	y	$&-$1	a-a
is(eval '"ab" ~~ rx:perl5/(a+|b){0,1}/ && $0', "a", 're_tests 231/0 (#283)', :todo);
is(eval '"ab" ~~ rx:perl5/(a+|b){0,1}/ && $1', "a", 're_tests 231/1 (#284)', :todo);
is(eval '"ab" ~~ rx/(a+|b)**{0..1}/ && $0', "a", 're_tests 232/0 (#285)', :todo);
is(eval '"ab" ~~ rx/(a+|b)**{0..1}/ && $1', "a", 're_tests 232/1 (#286)', :todo);
# 167: )(	-	c	-	Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 168: [^ab]*	cde	y	$&	cde
is(eval '"cde" ~~ rx:perl5/[^ab]*/ && $0', "cde", 're_tests 233/0 (#287)', :todo);
is(eval '"cde" ~~ rx/<[^ab]>*/ && $0', "cde", 're_tests 234/0 (#288)', :todo);
# 169: abc		n	-	-
ok(eval 'not ("" ~~ rx:perl5/abc/)', 're_tests 235  (#289)', :todo);
ok(eval 'not ("" ~~ rx/abc/)', 're_tests 236  (#290)', :todo);
# 170: a*		y	$&	
is(eval '"" ~~ rx:perl5/a*/ && $0', "", 're_tests 237/0 (#291)', :todo);
is(eval '"" ~~ rx/a*/ && $0', "", 're_tests 238/0 (#292)', :todo);
# 171: ([abc])*d	abbbcd	y	$&-$1	abbbcd-c
is(eval '"abbbcd" ~~ rx:perl5/([abc])*d/ && $0', "abbbcd", 're_tests 239/0 (#293)', :todo);
is(eval '"abbbcd" ~~ rx:perl5/([abc])*d/ && $1', "c", 're_tests 239/1 (#294)', :todo);
is(eval '"abbbcd" ~~ rx/(<[abc]>)*d/ && $0', "abbbcd", 're_tests 240/0 (#295)', :todo);
is(eval '"abbbcd" ~~ rx/(<[abc]>)*d/ && $1', "c", 're_tests 240/1 (#296)', :todo);
# 172: ([abc])*bcd	abcd	y	$&-$1	abcd-a
is(eval '"abcd" ~~ rx:perl5/([abc])*bcd/ && $0', "abcd", 're_tests 241/0 (#297)', :todo);
is(eval '"abcd" ~~ rx:perl5/([abc])*bcd/ && $1', "a", 're_tests 241/1 (#298)', :todo);
is(eval '"abcd" ~~ rx/(<[abc]>)*bcd/ && $0', "abcd", 're_tests 242/0 (#299)', :todo);
is(eval '"abcd" ~~ rx/(<[abc]>)*bcd/ && $1', "a", 're_tests 242/1 (#300)', :todo);
# 173: a|b|c|d|e	e	y	$&	e
is(eval '"e" ~~ rx:perl5/a|b|c|d|e/ && $0', "e", 're_tests 243/0 (#301)', :todo);
is(eval '"e" ~~ rx/a|b|c|d|e/ && $0', "e", 're_tests 244/0 (#302)', :todo);
# 174: (a|b|c|d|e)f	ef	y	$&-$1	ef-e
is(eval '"ef" ~~ rx:perl5/(a|b|c|d|e)f/ && $0', "ef", 're_tests 245/0 (#303)', :todo);
is(eval '"ef" ~~ rx:perl5/(a|b|c|d|e)f/ && $1', "e", 're_tests 245/1 (#304)', :todo);
is(eval '"ef" ~~ rx/(a|b|c|d|e)f/ && $0', "ef", 're_tests 246/0 (#305)', :todo);
is(eval '"ef" ~~ rx/(a|b|c|d|e)f/ && $1', "e", 're_tests 246/1 (#306)', :todo);
# 175: (a|b|c|d|e)f	ef	y	$-[0]	0
# 176: (a|b|c|d|e)f	ef	y	$+[0]	2 # SKIP
is(eval '"ef" ~~ rx:perl5/(a|b|c|d|e)f/ && getpos($/, 0)', 0, 're_tests 247/0 (#307)', :todo);
is(eval '"ef" ~~ rx/(a|b|c|d|e)f/ && getpos($/, 0)', 0, 're_tests 248/0 (#308)', :todo);
# 177: (a|b|c|d|e)f	ef	y	$-[1]	0
# 178: (a|b|c|d|e)f	ef	y	$+[1]	1 # SKIP
is(eval '"ef" ~~ rx:perl5/(a|b|c|d|e)f/ && getpos($/, 1)', 0, 're_tests 249/1 (#309)', :todo);
is(eval '"ef" ~~ rx/(a|b|c|d|e)f/ && getpos($/, 1)', 0, 're_tests 250/1 (#310)', :todo);
# 179: abcd*efg	abcdefg	y	$&	abcdefg
is(eval '"abcdefg" ~~ rx:perl5/abcd*efg/ && $0', "abcdefg", 're_tests 251/0 (#311)', :todo);
is(eval '"abcdefg" ~~ rx/abcd*efg/ && $0', "abcdefg", 're_tests 252/0 (#312)', :todo);
# 180: ab*	xabyabbbz	y	$&	ab
is(eval '"xabyabbbz" ~~ rx:perl5/ab*/ && $0', "ab", 're_tests 253/0 (#313)', :todo);
is(eval '"xabyabbbz" ~~ rx/ab*/ && $0', "ab", 're_tests 254/0 (#314)', :todo);
# 181: ab*	xayabbbz	y	$&	a
is(eval '"xayabbbz" ~~ rx:perl5/ab*/ && $0', "a", 're_tests 255/0 (#315)', :todo);
is(eval '"xayabbbz" ~~ rx/ab*/ && $0', "a", 're_tests 256/0 (#316)', :todo);
# 182: (ab|cd)e	abcde	y	$&-$1	cde-cd
is(eval '"abcde" ~~ rx:perl5/(ab|cd)e/ && $0', "cde", 're_tests 257/0 (#317)', :todo);
is(eval '"abcde" ~~ rx:perl5/(ab|cd)e/ && $1', "cd", 're_tests 257/1 (#318)', :todo);
is(eval '"abcde" ~~ rx/(ab|cd)e/ && $0', "cde", 're_tests 258/0 (#319)', :todo);
is(eval '"abcde" ~~ rx/(ab|cd)e/ && $1', "cd", 're_tests 258/1 (#320)', :todo);
# 183: [abhgefdc]ij	hij	y	$&	hij
is(eval '"hij" ~~ rx:perl5/[abhgefdc]ij/ && $0', "hij", 're_tests 259/0 (#321)', :todo);
is(eval '"hij" ~~ rx/<[abhgefdc]>ij/ && $0', "hij", 're_tests 260/0 (#322)', :todo);
# 184: ^(ab|cd)e	abcde	n	x$1y	xy
# SKIPPED: script doesn't understand `x$1y' yet
# SKIPPED: script doesn't understand `x$1y' yet
# 185: (abc|)ef	abcdef	y	$&-$1	ef-
is(eval '"abcdef" ~~ rx:perl5/(abc|)ef/ && $0', "ef", 're_tests 261/0 (#323)', :todo);
is(eval '"abcdef" ~~ rx:perl5/(abc|)ef/ && $1', "", 're_tests 261/1 (#324)', :todo);
is(eval '"abcdef" ~~ rx/(abc|)ef/ && $0', "ef", 're_tests 262/0 (#325)', :todo);
is(eval '"abcdef" ~~ rx/(abc|)ef/ && $1', "", 're_tests 262/1 (#326)', :todo);
# 186: (a|b)c*d	abcd	y	$&-$1	bcd-b
is(eval '"abcd" ~~ rx:perl5/(a|b)c*d/ && $0', "bcd", 're_tests 263/0 (#327)', :todo);
is(eval '"abcd" ~~ rx:perl5/(a|b)c*d/ && $1', "b", 're_tests 263/1 (#328)', :todo);
is(eval '"abcd" ~~ rx/(a|b)c*d/ && $0', "bcd", 're_tests 264/0 (#329)', :todo);
is(eval '"abcd" ~~ rx/(a|b)c*d/ && $1', "b", 're_tests 264/1 (#330)', :todo);
# 187: (ab|ab*)bc	abc	y	$&-$1	abc-a
is(eval '"abc" ~~ rx:perl5/(ab|ab*)bc/ && $0', "abc", 're_tests 265/0 (#331)', :todo);
is(eval '"abc" ~~ rx:perl5/(ab|ab*)bc/ && $1', "a", 're_tests 265/1 (#332)', :todo);
is(eval '"abc" ~~ rx/(ab|ab*)bc/ && $0', "abc", 're_tests 266/0 (#333)', :todo);
is(eval '"abc" ~~ rx/(ab|ab*)bc/ && $1', "a", 're_tests 266/1 (#334)', :todo);
# 188: a([bc]*)c*	abc	y	$&-$1	abc-bc
is(eval '"abc" ~~ rx:perl5/a([bc]*)c*/ && $0', "abc", 're_tests 267/0 (#335)', :todo);
is(eval '"abc" ~~ rx:perl5/a([bc]*)c*/ && $1', "bc", 're_tests 267/1 (#336)', :todo);
is(eval '"abc" ~~ rx/a(<[bc]>*)c*/ && $0', "abc", 're_tests 268/0 (#337)', :todo);
is(eval '"abc" ~~ rx/a(<[bc]>*)c*/ && $1', "bc", 're_tests 268/1 (#338)', :todo);
# 189: a([bc]*)(c*d)	abcd	y	$&-$1-$2	abcd-bc-d
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c*d)/ && $0', "abcd", 're_tests 269/0 (#339)', :todo);
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c*d)/ && $1', "bc", 're_tests 269/1 (#340)', :todo);
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c*d)/ && $2', "d", 're_tests 269/2 (#341)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c*d)/ && $0', "abcd", 're_tests 270/0 (#342)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c*d)/ && $1', "bc", 're_tests 270/1 (#343)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c*d)/ && $2', "d", 're_tests 270/2 (#344)', :todo);
# 190: a([bc]*)(c*d)	abcd	y	$-[0]	0
# 191: a([bc]*)(c*d)	abcd	y	$+[0]	4 # SKIP
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c*d)/ && getpos($/, 0)', 0, 're_tests 271/0 (#345)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c*d)/ && getpos($/, 0)', 0, 're_tests 272/0 (#346)', :todo);
# 192: a([bc]*)(c*d)	abcd	y	$-[1]	1
# 193: a([bc]*)(c*d)	abcd	y	$+[1]	3 # SKIP
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c*d)/ && getpos($/, 1)', 1, 're_tests 273/1 (#347)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c*d)/ && getpos($/, 1)', 1, 're_tests 274/1 (#348)', :todo);
# 194: a([bc]*)(c*d)	abcd	y	$-[2]	3
# 195: a([bc]*)(c*d)	abcd	y	$+[2]	4 # SKIP
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c*d)/ && getpos($/, 2)', 3, 're_tests 275/2 (#349)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c*d)/ && getpos($/, 2)', 3, 're_tests 276/2 (#350)', :todo);
# 196: a([bc]+)(c*d)	abcd	y	$&-$1-$2	abcd-bc-d
is(eval '"abcd" ~~ rx:perl5/a([bc]+)(c*d)/ && $0', "abcd", 're_tests 277/0 (#351)', :todo);
is(eval '"abcd" ~~ rx:perl5/a([bc]+)(c*d)/ && $1', "bc", 're_tests 277/1 (#352)', :todo);
is(eval '"abcd" ~~ rx:perl5/a([bc]+)(c*d)/ && $2', "d", 're_tests 277/2 (#353)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>+)(c*d)/ && $0', "abcd", 're_tests 278/0 (#354)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>+)(c*d)/ && $1', "bc", 're_tests 278/1 (#355)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>+)(c*d)/ && $2', "d", 're_tests 278/2 (#356)', :todo);
# 197: a([bc]*)(c+d)	abcd	y	$&-$1-$2	abcd-b-cd
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c+d)/ && $0', "abcd", 're_tests 279/0 (#357)', :todo);
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c+d)/ && $1', "b", 're_tests 279/1 (#358)', :todo);
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c+d)/ && $2', "cd", 're_tests 279/2 (#359)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c+d)/ && $0', "abcd", 're_tests 280/0 (#360)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c+d)/ && $1', "b", 're_tests 280/1 (#361)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c+d)/ && $2', "cd", 're_tests 280/2 (#362)', :todo);
# 198: a([bc]*)(c+d)	abcd	y	$-[0]	0
# 199: a([bc]*)(c+d)	abcd	y	$+[0]	4 # SKIP
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c+d)/ && getpos($/, 0)', 0, 're_tests 281/0 (#363)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c+d)/ && getpos($/, 0)', 0, 're_tests 282/0 (#364)', :todo);
# 200: a([bc]*)(c+d)	abcd	y	$-[1]	1
# 201: a([bc]*)(c+d)	abcd	y	$+[1]	2 # SKIP
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c+d)/ && getpos($/, 1)', 1, 're_tests 283/1 (#365)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c+d)/ && getpos($/, 1)', 1, 're_tests 284/1 (#366)', :todo);
# 202: a([bc]*)(c+d)	abcd	y	$-[2]	2
# 203: a([bc]*)(c+d)	abcd	y	$+[2]	4 # SKIP
is(eval '"abcd" ~~ rx:perl5/a([bc]*)(c+d)/ && getpos($/, 2)', 2, 're_tests 285/2 (#367)', :todo);
is(eval '"abcd" ~~ rx/a(<[bc]>*)(c+d)/ && getpos($/, 2)', 2, 're_tests 286/2 (#368)', :todo);
# 204: a[bcd]*dcdcde	adcdcde	y	$&	adcdcde
is(eval '"adcdcde" ~~ rx:perl5/a[bcd]*dcdcde/ && $0', "adcdcde", 're_tests 287/0 (#369)', :todo);
is(eval '"adcdcde" ~~ rx/a<[bcd]>*dcdcde/ && $0', "adcdcde", 're_tests 288/0 (#370)', :todo);
# 205: a[bcd]+dcdcde	adcdcde	n	-	-
ok(eval 'not ("adcdcde" ~~ rx:perl5/a[bcd]+dcdcde/)', 're_tests 289  (#371)', :todo);
ok(eval 'not ("adcdcde" ~~ rx/a<[bcd]>+dcdcde/)', 're_tests 290  (#372)', :todo);
# 206: (ab|a)b*c	abc	y	$&-$1	abc-ab
is(eval '"abc" ~~ rx:perl5/(ab|a)b*c/ && $0', "abc", 're_tests 291/0 (#373)', :todo);
is(eval '"abc" ~~ rx:perl5/(ab|a)b*c/ && $1', "ab", 're_tests 291/1 (#374)', :todo);
is(eval '"abc" ~~ rx/(ab|a)b*c/ && $0', "abc", 're_tests 292/0 (#375)', :todo);
is(eval '"abc" ~~ rx/(ab|a)b*c/ && $1', "ab", 're_tests 292/1 (#376)', :todo);
# 207: (ab|a)b*c	abc	y	$-[0]	0
# 208: (ab|a)b*c	abc	y	$+[0]	3 # SKIP
is(eval '"abc" ~~ rx:perl5/(ab|a)b*c/ && getpos($/, 0)', 0, 're_tests 293/0 (#377)', :todo);
is(eval '"abc" ~~ rx/(ab|a)b*c/ && getpos($/, 0)', 0, 're_tests 294/0 (#378)', :todo);
# 209: (ab|a)b*c	abc	y	$-[1]	0
# 210: (ab|a)b*c	abc	y	$+[1]	2 # SKIP
is(eval '"abc" ~~ rx:perl5/(ab|a)b*c/ && getpos($/, 1)', 0, 're_tests 295/1 (#379)', :todo);
is(eval '"abc" ~~ rx/(ab|a)b*c/ && getpos($/, 1)', 0, 're_tests 296/1 (#380)', :todo);
# 211: ((a)(b)c)(d)	abcd	y	$1-$2-$3-$4	abc-a-b-d
# SKIPPED: script doesn't understand `$1-$2-$3-$4' yet
# SKIPPED: script doesn't understand `$1-$2-$3-$4' yet
# 212: ((a)(b)c)(d)	abcd	y	$-[0]	0
# 213: ((a)(b)c)(d)	abcd	y	$+[0]	4 # SKIP
is(eval '"abcd" ~~ rx:perl5/((a)(b)c)(d)/ && getpos($/, 0)', 0, 're_tests 297/0 (#381)', :todo);
is(eval '"abcd" ~~ rx/((a)(b)c)(d)/ && getpos($/, 0)', 0, 're_tests 298/0 (#382)', :todo);
# 214: ((a)(b)c)(d)	abcd	y	$-[1]	0
# 215: ((a)(b)c)(d)	abcd	y	$+[1]	3 # SKIP
is(eval '"abcd" ~~ rx:perl5/((a)(b)c)(d)/ && getpos($/, 1)', 0, 're_tests 299/1 (#383)', :todo);
is(eval '"abcd" ~~ rx/((a)(b)c)(d)/ && getpos($/, 1)', 0, 're_tests 300/1 (#384)', :todo);
# 216: ((a)(b)c)(d)	abcd	y	$-[2]	0
# 217: ((a)(b)c)(d)	abcd	y	$+[2]	1 # SKIP
is(eval '"abcd" ~~ rx:perl5/((a)(b)c)(d)/ && getpos($/, 2)', 0, 're_tests 301/2 (#385)', :todo);
is(eval '"abcd" ~~ rx/((a)(b)c)(d)/ && getpos($/, 2)', 0, 're_tests 302/2 (#386)', :todo);
# 218: ((a)(b)c)(d)	abcd	y	$-[3]	1
# 219: ((a)(b)c)(d)	abcd	y	$+[3]	2 # SKIP
is(eval '"abcd" ~~ rx:perl5/((a)(b)c)(d)/ && getpos($/, 3)', 1, 're_tests 303/3 (#387)', :todo);
is(eval '"abcd" ~~ rx/((a)(b)c)(d)/ && getpos($/, 3)', 1, 're_tests 304/3 (#388)', :todo);
# 220: ((a)(b)c)(d)	abcd	y	$-[4]	3
# 221: ((a)(b)c)(d)	abcd	y	$+[4]	4 # SKIP
is(eval '"abcd" ~~ rx:perl5/((a)(b)c)(d)/ && getpos($/, 4)', 3, 're_tests 305/4 (#389)', :todo);
is(eval '"abcd" ~~ rx/((a)(b)c)(d)/ && getpos($/, 4)', 3, 're_tests 306/4 (#390)', :todo);
# 222: [a-zA-Z_][a-zA-Z0-9_]*	alpha	y	$&	alpha
is(eval '"alpha" ~~ rx:perl5/[a-zA-Z_][a-zA-Z0-9_]*/ && $0', "alpha", 're_tests 307/0 (#391)', :todo);
is(eval '"alpha" ~~ rx/<[a-zA-Z_]><[a-zA-Z0-9_]>*/ && $0', "alpha", 're_tests 308/0 (#392)', :todo);
# 223: ^a(bc+|b[eh])g|.h$	abh	y	$&-$1	bh-
is(eval '"abh" ~~ rx:perl5/^a(bc+|b[eh])g|.h$/ && $0', "bh", 're_tests 309/0 (#393)', :todo);
is(eval '"abh" ~~ rx:perl5/^a(bc+|b[eh])g|.h$/ && $1', "", 're_tests 309/1 (#394)', :todo);
is(eval '"abh" ~~ rx/^a(bc+|b<[eh]>)g|\Nh$/ && $0', "bh", 're_tests 310/0 (#395)', :todo);
is(eval '"abh" ~~ rx/^a(bc+|b<[eh]>)g|\Nh$/ && $1', "", 're_tests 310/1 (#396)', :todo);
# 224: (bc+d$|ef*g.|h?i(j|k))	effgz	y	$&-$1-$2	effgz-effgz-
is(eval '"effgz" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $0', "effgz", 're_tests 311/0 (#397)', :todo);
is(eval '"effgz" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $1', "effgz", 're_tests 311/1 (#398)', :todo);
is(eval '"effgz" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $2', "", 're_tests 311/2 (#399)', :todo);
is(eval '"effgz" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $0', "effgz", 're_tests 312/0 (#400)', :todo);
is(eval '"effgz" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $1', "effgz", 're_tests 312/1 (#401)', :todo);
is(eval '"effgz" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $2', "", 're_tests 312/2 (#402)', :todo);
# 225: (bc+d$|ef*g.|h?i(j|k))	ij	y	$&-$1-$2	ij-ij-j
is(eval '"ij" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $0', "ij", 're_tests 313/0 (#403)', :todo);
is(eval '"ij" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $1', "ij", 're_tests 313/1 (#404)', :todo);
is(eval '"ij" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $2', "j", 're_tests 313/2 (#405)', :todo);
is(eval '"ij" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $0', "ij", 're_tests 314/0 (#406)', :todo);
is(eval '"ij" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $1', "ij", 're_tests 314/1 (#407)', :todo);
is(eval '"ij" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $2', "j", 're_tests 314/2 (#408)', :todo);
# 226: (bc+d$|ef*g.|h?i(j|k))	effg	n	-	-
ok(eval 'not ("effg" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/)', 're_tests 315  (#409)', :todo);
ok(eval 'not ("effg" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/)', 're_tests 316  (#410)', :todo);
# 227: (bc+d$|ef*g.|h?i(j|k))	bcdd	n	-	-
ok(eval 'not ("bcdd" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/)', 're_tests 317  (#411)', :todo);
ok(eval 'not ("bcdd" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/)', 're_tests 318  (#412)', :todo);
# 228: (bc+d$|ef*g.|h?i(j|k))	reffgz	y	$&-$1-$2	effgz-effgz-
is(eval '"reffgz" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $0', "effgz", 're_tests 319/0 (#413)', :todo);
is(eval '"reffgz" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $1', "effgz", 're_tests 319/1 (#414)', :todo);
is(eval '"reffgz" ~~ rx:perl5/(bc+d$|ef*g.|h?i(j|k))/ && $2', "", 're_tests 319/2 (#415)', :todo);
is(eval '"reffgz" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $0', "effgz", 're_tests 320/0 (#416)', :todo);
is(eval '"reffgz" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $1', "effgz", 're_tests 320/1 (#417)', :todo);
is(eval '"reffgz" ~~ rx/(bc+d$|ef*g\N|h?i(j|k))/ && $2', "", 're_tests 320/2 (#418)', :todo);
# 229: ((((((((((a))))))))))	a	y	$10	a
is(eval '"a" ~~ rx:perl5/((((((((((a))))))))))/ && $10', "a", 're_tests 321/10 (#419)', :todo);
is(eval '"a" ~~ rx/((((((((((a))))))))))/ && $10', "a", 're_tests 322/10 (#420)', :todo);
# 230: ((((((((((a))))))))))	a	y	$-[0]	0
# 231: ((((((((((a))))))))))	a	y	$+[0]	1 # SKIP
is(eval '"a" ~~ rx:perl5/((((((((((a))))))))))/ && getpos($/, 0)', 0, 're_tests 323/0 (#421)', :todo);
is(eval '"a" ~~ rx/((((((((((a))))))))))/ && getpos($/, 0)', 0, 're_tests 324/0 (#422)', :todo);
# 232: ((((((((((a))))))))))	a	y	$-[10]	0
# 233: ((((((((((a))))))))))	a	y	$+[10]	1 # SKIP
is(eval '"a" ~~ rx:perl5/((((((((((a))))))))))/ && getpos($/, 10)', 0, 're_tests 325/10 (#423)', :todo);
is(eval '"a" ~~ rx/((((((((((a))))))))))/ && getpos($/, 10)', 0, 're_tests 326/10 (#424)', :todo);
# 234: ((((((((((a))))))))))\10	aa	y	$&	aa
is(eval '"aa" ~~ rx:perl5/((((((((((a))))))))))\10/ && $0', "aa", 're_tests 327/0 (#425)', :todo);
is(eval '"aa" ~~ rx/((((((((((a))))))))))$10/ && $0', "aa", 're_tests 328/0 (#426)', :todo);
# 235: ((((((((((a))))))))))${bang}	aa	n	-	-
ok(eval 'not ("aa" ~~ rx:perl5/((((((((((a))))))))))${bang}/)', 're_tests 329  (#427)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 236: ((((((((((a))))))))))${bang}	a!	y	$&	a!
is(eval '"a!" ~~ rx:perl5/((((((((((a))))))))))${bang}/ && $0', "a!", 're_tests 330/0 (#428)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 237: (((((((((a)))))))))	a	y	$&	a
is(eval '"a" ~~ rx:perl5/(((((((((a)))))))))/ && $0', "a", 're_tests 331/0 (#429)', :todo);
is(eval '"a" ~~ rx/(((((((((a)))))))))/ && $0', "a", 're_tests 332/0 (#430)', :todo);
# 238: multiple words of text	uh-uh	n	-	-
ok(eval 'not ("uh-uh" ~~ rx:perl5/multiple words of text/)', 're_tests 333  (#431)', :todo);
ok(eval 'not ("uh-uh" ~~ rx/multiple<sp>words<sp>of<sp>text/)', 're_tests 334  (#432)', :todo);
# 239: multiple words	multiple words, yeah	y	$&	multiple words
is(eval '"multiple words, yeah" ~~ rx:perl5/multiple words/ && $0', "multiple words", 're_tests 335/0 (#433)', :todo);
is(eval '"multiple words, yeah" ~~ rx/multiple<sp>words/ && $0', "multiple words", 're_tests 336/0 (#434)', :todo);
# 240: (.*)c(.*)	abcde	y	$&-$1-$2	abcde-ab-de
is(eval '"abcde" ~~ rx:perl5/(.*)c(.*)/ && $0', "abcde", 're_tests 337/0 (#435)', :todo);
is(eval '"abcde" ~~ rx:perl5/(.*)c(.*)/ && $1', "ab", 're_tests 337/1 (#436)', :todo);
is(eval '"abcde" ~~ rx:perl5/(.*)c(.*)/ && $2', "de", 're_tests 337/2 (#437)', :todo);
is(eval '"abcde" ~~ rx/(\N*)c(\N*)/ && $0', "abcde", 're_tests 338/0 (#438)', :todo);
is(eval '"abcde" ~~ rx/(\N*)c(\N*)/ && $1', "ab", 're_tests 338/1 (#439)', :todo);
is(eval '"abcde" ~~ rx/(\N*)c(\N*)/ && $2', "de", 're_tests 338/2 (#440)', :todo);
# 241: \((.*), (.*)\)	(a, b)	y	($2, $1)	(b, a)
# SKIPPED: script doesn't understand `($2, $1)' yet
# SKIPPED: script doesn't understand `($2, $1)' yet
# 242: [k]	ab	n	-	-
ok(eval 'not ("ab" ~~ rx:perl5/[k]/)', 're_tests 339  (#441)', :todo);
ok(eval 'not ("ab" ~~ rx/<[k]>/)', 're_tests 340  (#442)', :todo);
# 243: abcd	abcd	y	$&-\$&-\\$&	abcd-$&-\abcd
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# 244: a(bc)d	abcd	y	$1-\$1-\\$1	bc-$1-\bc
# SKIPPED: script doesn't understand `$1-\$1-\\$1' yet
# SKIPPED: script doesn't understand `$1-\$1-\\$1' yet
# 245: a[-]?c	ac	y	$&	ac
is(eval '"ac" ~~ rx:perl5/a[-]?c/ && $0', "ac", 're_tests 341/0 (#443)', :todo);
is(eval '"ac" ~~ rx/a<[-]>?c/ && $0', "ac", 're_tests 342/0 (#444)', :todo);
# 246: (abc)\1	abcabc	y	$1	abc
is(eval '"abcabc" ~~ rx:perl5/(abc)\1/ && $1', "abc", 're_tests 343/1 (#445)', :todo);
is(eval '"abcabc" ~~ rx/(abc)$1/ && $1', "abc", 're_tests 344/1 (#446)', :todo);
# 247: ([a-c]*)\1	abcabc	y	$1	abc
is(eval '"abcabc" ~~ rx:perl5/([a-c]*)\1/ && $1', "abc", 're_tests 345/1 (#447)', :todo);
is(eval '"abcabc" ~~ rx/(<[a-c]>*)$1/ && $1', "abc", 're_tests 346/1 (#448)', :todo);
# 248: \1	-	c	-	Reference to nonexistent group
# -- SKIPPED - TESTS ERROR MESSAGE
# 249: \2	-	c	-	Reference to nonexistent group
# -- SKIPPED - TESTS ERROR MESSAGE
# 250: (a)|\1	a	y	-	-
ok(eval '"a" ~~ rx:perl5/(a)|\1/', 're_tests 347  (#449)', :todo);
ok(eval '"a" ~~ rx/(a)|$1/', 're_tests 348  (#450)', :todo);
# 251: (a)|\1	x	n	-	-
ok(eval 'not ("x" ~~ rx:perl5/(a)|\1/)', 're_tests 349  (#451)', :todo);
ok(eval 'not ("x" ~~ rx/(a)|$1/)', 're_tests 350  (#452)', :todo);
# 252: (a)|\2	-	c	-	Reference to nonexistent group
# -- SKIPPED - TESTS ERROR MESSAGE
# 253: (([a-c])b*?\2)*	ababbbcbc	y	$&-$1-$2	ababb-bb-b
is(eval '"ababbbcbc" ~~ rx:perl5/(([a-c])b*?\2)*/ && $0', "ababb", 're_tests 351/0 (#453)', :todo);
is(eval '"ababbbcbc" ~~ rx:perl5/(([a-c])b*?\2)*/ && $1', "bb", 're_tests 351/1 (#454)', :todo);
is(eval '"ababbbcbc" ~~ rx:perl5/(([a-c])b*?\2)*/ && $2', "b", 're_tests 351/2 (#455)', :todo);
is(eval '"ababbbcbc" ~~ rx/((<[a-c]>)b*?$2)*/ && $0', "ababb", 're_tests 352/0 (#456)', :todo);
is(eval '"ababbbcbc" ~~ rx/((<[a-c]>)b*?$2)*/ && $1', "bb", 're_tests 352/1 (#457)', :todo);
is(eval '"ababbbcbc" ~~ rx/((<[a-c]>)b*?$2)*/ && $2', "b", 're_tests 352/2 (#458)', :todo);
# 254: (([a-c])b*?\2){3}	ababbbcbc	y	$&-$1-$2	ababbbcbc-cbc-c
is(eval '"ababbbcbc" ~~ rx:perl5/(([a-c])b*?\2){3}/ && $0', "ababbbcbc", 're_tests 353/0 (#459)', :todo);
is(eval '"ababbbcbc" ~~ rx:perl5/(([a-c])b*?\2){3}/ && $1', "cbc", 're_tests 353/1 (#460)', :todo);
is(eval '"ababbbcbc" ~~ rx:perl5/(([a-c])b*?\2){3}/ && $2', "c", 're_tests 353/2 (#461)', :todo);
is(eval '"ababbbcbc" ~~ rx/((<[a-c]>)b*?$2)**{3}/ && $0', "ababbbcbc", 're_tests 354/0 (#462)', :todo);
is(eval '"ababbbcbc" ~~ rx/((<[a-c]>)b*?$2)**{3}/ && $1', "cbc", 're_tests 354/1 (#463)', :todo);
is(eval '"ababbbcbc" ~~ rx/((<[a-c]>)b*?$2)**{3}/ && $2', "c", 're_tests 354/2 (#464)', :todo);
# 255: ((\3|b)\2(a)x)+	aaxabxbaxbbx	n	-	-
ok(eval 'not ("aaxabxbaxbbx" ~~ rx:perl5/((\3|b)\2(a)x)+/)', 're_tests 355  (#465)', :todo);
ok(eval 'not ("aaxabxbaxbbx" ~~ rx/(($3|b)$2(a)x)+/)', 're_tests 356  (#466)', :todo);
# 256: ((\3|b)\2(a)x)+	aaaxabaxbaaxbbax	y	$&-$1-$2-$3	bbax-bbax-b-a
# SKIPPED: script doesn't understand `$&-$1-$2-$3' yet
# SKIPPED: script doesn't understand `$&-$1-$2-$3' yet
# 257: ((\3|b)\2(a)){2,}	bbaababbabaaaaabbaaaabba	y	$&-$1-$2-$3	bbaaaabba-bba-b-a
# SKIPPED: script doesn't understand `$&-$1-$2-$3' yet
# SKIPPED: script doesn't understand `$&-$1-$2-$3' yet
# 258: (a)|(b)	b	y	$-[0]	0
# 259: (a)|(b)	b	y	$+[0]	1 # SKIP
is(eval '"b" ~~ rx:perl5/(a)|(b)/ && getpos($/, 0)', 0, 're_tests 357/0 (#467)', :todo);
is(eval '"b" ~~ rx/(a)|(b)/ && getpos($/, 0)', 0, 're_tests 358/0 (#468)', :todo);
# 260: (a)|(b)	b	y	x$-[1]	x
# 261: (a)|(b)	b	y	x$+[1]	x # SKIP
# SKIPPED: script doesn't understand `x$-[1]' yet
# SKIPPED: script doesn't understand `x$-[1]' yet
# 262: (a)|(b)	b	y	$-[2]	0
# 263: (a)|(b)	b	y	$+[2]	1 # SKIP
is(eval '"b" ~~ rx:perl5/(a)|(b)/ && getpos($/, 2)', 0, 're_tests 359/2 (#469)', :todo);
is(eval '"b" ~~ rx/(a)|(b)/ && getpos($/, 2)', 0, 're_tests 360/2 (#470)', :todo);
# 264: 'abc'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)abc/ && $0', "ABC", 're_tests 361/0 (#471)', :todo);
is(eval '"ABC" ~~ rx:i/abc/ && $0', "ABC", 're_tests 362/0 (#472)', :todo);
# 265: 'abc'i	XBC	n	-	-
ok(eval 'not ("XBC" ~~ rx:perl5/(?i)abc/)', 're_tests 363  (#473)', :todo);
ok(eval 'not ("XBC" ~~ rx:i/abc/)', 're_tests 364  (#474)', :todo);
# 266: 'abc'i	AXC	n	-	-
ok(eval 'not ("AXC" ~~ rx:perl5/(?i)abc/)', 're_tests 365  (#475)', :todo);
ok(eval 'not ("AXC" ~~ rx:i/abc/)', 're_tests 366  (#476)', :todo);
# 267: 'abc'i	ABX	n	-	-
ok(eval 'not ("ABX" ~~ rx:perl5/(?i)abc/)', 're_tests 367  (#477)', :todo);
ok(eval 'not ("ABX" ~~ rx:i/abc/)', 're_tests 368  (#478)', :todo);
# 268: 'abc'i	XABCY	y	$&	ABC
is(eval '"XABCY" ~~ rx:perl5/(?i)abc/ && $0', "ABC", 're_tests 369/0 (#479)', :todo);
is(eval '"XABCY" ~~ rx:i/abc/ && $0', "ABC", 're_tests 370/0 (#480)', :todo);
# 269: 'abc'i	ABABC	y	$&	ABC
is(eval '"ABABC" ~~ rx:perl5/(?i)abc/ && $0', "ABC", 're_tests 371/0 (#481)', :todo);
is(eval '"ABABC" ~~ rx:i/abc/ && $0', "ABC", 're_tests 372/0 (#482)', :todo);
# 270: 'ab*c'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)ab*c/ && $0', "ABC", 're_tests 373/0 (#483)', :todo);
is(eval '"ABC" ~~ rx:i/ab*c/ && $0', "ABC", 're_tests 374/0 (#484)', :todo);
# 271: 'ab*bc'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)ab*bc/ && $0', "ABC", 're_tests 375/0 (#485)', :todo);
is(eval '"ABC" ~~ rx:i/ab*bc/ && $0', "ABC", 're_tests 376/0 (#486)', :todo);
# 272: 'ab*bc'i	ABBC	y	$&	ABBC
is(eval '"ABBC" ~~ rx:perl5/(?i)ab*bc/ && $0', "ABBC", 're_tests 377/0 (#487)', :todo);
is(eval '"ABBC" ~~ rx:i/ab*bc/ && $0', "ABBC", 're_tests 378/0 (#488)', :todo);
# 273: 'ab*?bc'i	ABBBBC	y	$&	ABBBBC
is(eval '"ABBBBC" ~~ rx:perl5/(?i)ab*?bc/ && $0', "ABBBBC", 're_tests 379/0 (#489)', :todo);
is(eval '"ABBBBC" ~~ rx:i/ab*?bc/ && $0', "ABBBBC", 're_tests 380/0 (#490)', :todo);
# 274: 'ab{0,}?bc'i	ABBBBC	y	$&	ABBBBC
is(eval '"ABBBBC" ~~ rx:perl5/(?i)ab{0,}?bc/ && $0', "ABBBBC", 're_tests 381/0 (#491)', :todo);
is(eval '"ABBBBC" ~~ rx:i/ab**{0...}?bc/ && $0', "ABBBBC", 're_tests 382/0 (#492)', :todo);
# 275: 'ab+?bc'i	ABBC	y	$&	ABBC
is(eval '"ABBC" ~~ rx:perl5/(?i)ab+?bc/ && $0', "ABBC", 're_tests 383/0 (#493)', :todo);
is(eval '"ABBC" ~~ rx:i/ab+?bc/ && $0', "ABBC", 're_tests 384/0 (#494)', :todo);
# 276: 'ab+bc'i	ABC	n	-	-
ok(eval 'not ("ABC" ~~ rx:perl5/(?i)ab+bc/)', 're_tests 385  (#495)', :todo);
ok(eval 'not ("ABC" ~~ rx:i/ab+bc/)', 're_tests 386  (#496)', :todo);
# 277: 'ab+bc'i	ABQ	n	-	-
ok(eval 'not ("ABQ" ~~ rx:perl5/(?i)ab+bc/)', 're_tests 387  (#497)', :todo);
ok(eval 'not ("ABQ" ~~ rx:i/ab+bc/)', 're_tests 388  (#498)', :todo);
# 278: 'ab{1,}bc'i	ABQ	n	-	-
ok(eval 'not ("ABQ" ~~ rx:perl5/(?i)ab{1,}bc/)', 're_tests 389  (#499)', :todo);
ok(eval 'not ("ABQ" ~~ rx:i/ab**{1...}bc/)', 're_tests 390  (#500)', :todo);
# 279: 'ab+bc'i	ABBBBC	y	$&	ABBBBC
is(eval '"ABBBBC" ~~ rx:perl5/(?i)ab+bc/ && $0', "ABBBBC", 're_tests 391/0 (#501)', :todo);
is(eval '"ABBBBC" ~~ rx:i/ab+bc/ && $0', "ABBBBC", 're_tests 392/0 (#502)', :todo);
# 280: 'ab{1,}?bc'i	ABBBBC	y	$&	ABBBBC
is(eval '"ABBBBC" ~~ rx:perl5/(?i)ab{1,}?bc/ && $0', "ABBBBC", 're_tests 393/0 (#503)', :todo);
is(eval '"ABBBBC" ~~ rx:i/ab**{1...}?bc/ && $0', "ABBBBC", 're_tests 394/0 (#504)', :todo);
# 281: 'ab{1,3}?bc'i	ABBBBC	y	$&	ABBBBC
is(eval '"ABBBBC" ~~ rx:perl5/(?i)ab{1,3}?bc/ && $0', "ABBBBC", 're_tests 395/0 (#505)', :todo);
is(eval '"ABBBBC" ~~ rx:i/ab**{1..3}?bc/ && $0', "ABBBBC", 're_tests 396/0 (#506)', :todo);
# 282: 'ab{3,4}?bc'i	ABBBBC	y	$&	ABBBBC
is(eval '"ABBBBC" ~~ rx:perl5/(?i)ab{3,4}?bc/ && $0', "ABBBBC", 're_tests 397/0 (#507)', :todo);
is(eval '"ABBBBC" ~~ rx:i/ab**{3..4}?bc/ && $0', "ABBBBC", 're_tests 398/0 (#508)', :todo);
# 283: 'ab{4,5}?bc'i	ABBBBC	n	-	-
ok(eval 'not ("ABBBBC" ~~ rx:perl5/(?i)ab{4,5}?bc/)', 're_tests 399  (#509)', :todo);
ok(eval 'not ("ABBBBC" ~~ rx:i/ab**{4..5}?bc/)', 're_tests 400  (#510)', :todo);
# 284: 'ab??bc'i	ABBC	y	$&	ABBC
is(eval '"ABBC" ~~ rx:perl5/(?i)ab??bc/ && $0', "ABBC", 're_tests 401/0 (#511)', :todo);
is(eval '"ABBC" ~~ rx:i/ab??bc/ && $0', "ABBC", 're_tests 402/0 (#512)', :todo);
# 285: 'ab??bc'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)ab??bc/ && $0', "ABC", 're_tests 403/0 (#513)', :todo);
is(eval '"ABC" ~~ rx:i/ab??bc/ && $0', "ABC", 're_tests 404/0 (#514)', :todo);
# 286: 'ab{0,1}?bc'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)ab{0,1}?bc/ && $0', "ABC", 're_tests 405/0 (#515)', :todo);
is(eval '"ABC" ~~ rx:i/ab**{0..1}?bc/ && $0', "ABC", 're_tests 406/0 (#516)', :todo);
# 287: 'ab??bc'i	ABBBBC	n	-	-
ok(eval 'not ("ABBBBC" ~~ rx:perl5/(?i)ab??bc/)', 're_tests 407  (#517)', :todo);
ok(eval 'not ("ABBBBC" ~~ rx:i/ab??bc/)', 're_tests 408  (#518)', :todo);
# 288: 'ab??c'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)ab??c/ && $0', "ABC", 're_tests 409/0 (#519)', :todo);
is(eval '"ABC" ~~ rx:i/ab??c/ && $0', "ABC", 're_tests 410/0 (#520)', :todo);
# 289: 'ab{0,1}?c'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)ab{0,1}?c/ && $0', "ABC", 're_tests 411/0 (#521)', :todo);
is(eval '"ABC" ~~ rx:i/ab**{0..1}?c/ && $0', "ABC", 're_tests 412/0 (#522)', :todo);
# 290: '^abc$'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)^abc$/ && $0', "ABC", 're_tests 413/0 (#523)', :todo);
is(eval '"ABC" ~~ rx:i/^abc$/ && $0', "ABC", 're_tests 414/0 (#524)', :todo);
# 291: '^abc$'i	ABCC	n	-	-
ok(eval 'not ("ABCC" ~~ rx:perl5/(?i)^abc$/)', 're_tests 415  (#525)', :todo);
ok(eval 'not ("ABCC" ~~ rx:i/^abc$/)', 're_tests 416  (#526)', :todo);
# 292: '^abc'i	ABCC	y	$&	ABC
is(eval '"ABCC" ~~ rx:perl5/(?i)^abc/ && $0', "ABC", 're_tests 417/0 (#527)', :todo);
is(eval '"ABCC" ~~ rx:i/^abc/ && $0', "ABC", 're_tests 418/0 (#528)', :todo);
# 293: '^abc$'i	AABC	n	-	-
ok(eval 'not ("AABC" ~~ rx:perl5/(?i)^abc$/)', 're_tests 419  (#529)', :todo);
ok(eval 'not ("AABC" ~~ rx:i/^abc$/)', 're_tests 420  (#530)', :todo);
# 294: 'abc$'i	AABC	y	$&	ABC
is(eval '"AABC" ~~ rx:perl5/(?i)abc$/ && $0', "ABC", 're_tests 421/0 (#531)', :todo);
is(eval '"AABC" ~~ rx:i/abc$/ && $0', "ABC", 're_tests 422/0 (#532)', :todo);
# 295: '^'i	ABC	y	$&	
is(eval '"ABC" ~~ rx:perl5/(?i)^/ && $0', "", 're_tests 423/0 (#533)', :todo);
is(eval '"ABC" ~~ rx:i/^/ && $0', "", 're_tests 424/0 (#534)', :todo);
# 296: '$'i	ABC	y	$&	
is(eval '"ABC" ~~ rx:perl5/(?i)$/ && $0', "", 're_tests 425/0 (#535)', :todo);
is(eval '"ABC" ~~ rx:i/$/ && $0', "", 're_tests 426/0 (#536)', :todo);
# 297: 'a.c'i	ABC	y	$&	ABC
is(eval '"ABC" ~~ rx:perl5/(?i)a.c/ && $0', "ABC", 're_tests 427/0 (#537)', :todo);
is(eval '"ABC" ~~ rx:i/a\Nc/ && $0', "ABC", 're_tests 428/0 (#538)', :todo);
# 298: 'a.c'i	AXC	y	$&	AXC
is(eval '"AXC" ~~ rx:perl5/(?i)a.c/ && $0', "AXC", 're_tests 429/0 (#539)', :todo);
is(eval '"AXC" ~~ rx:i/a\Nc/ && $0', "AXC", 're_tests 430/0 (#540)', :todo);
# 299: 'a.*?c'i	AXYZC	y	$&	AXYZC
is(eval '"AXYZC" ~~ rx:perl5/(?i)a.*?c/ && $0', "AXYZC", 're_tests 431/0 (#541)', :todo);
is(eval '"AXYZC" ~~ rx:i/a\N*?c/ && $0', "AXYZC", 're_tests 432/0 (#542)', :todo);
# 300: 'a.*c'i	AXYZD	n	-	-
ok(eval 'not ("AXYZD" ~~ rx:perl5/(?i)a.*c/)', 're_tests 433  (#543)', :todo);
ok(eval 'not ("AXYZD" ~~ rx:i/a\N*c/)', 're_tests 434  (#544)', :todo);
# 301: 'a[bc]d'i	ABC	n	-	-
ok(eval 'not ("ABC" ~~ rx:perl5/(?i)a[bc]d/)', 're_tests 435  (#545)', :todo);
ok(eval 'not ("ABC" ~~ rx:i/a<[bc]>d/)', 're_tests 436  (#546)', :todo);
# 302: 'a[bc]d'i	ABD	y	$&	ABD
is(eval '"ABD" ~~ rx:perl5/(?i)a[bc]d/ && $0', "ABD", 're_tests 437/0 (#547)', :todo);
is(eval '"ABD" ~~ rx:i/a<[bc]>d/ && $0', "ABD", 're_tests 438/0 (#548)', :todo);
# 303: 'a[b-d]e'i	ABD	n	-	-
ok(eval 'not ("ABD" ~~ rx:perl5/(?i)a[b-d]e/)', 're_tests 439  (#549)', :todo);
ok(eval 'not ("ABD" ~~ rx:i/a<[b-d]>e/)', 're_tests 440  (#550)', :todo);
# 304: 'a[b-d]e'i	ACE	y	$&	ACE
is(eval '"ACE" ~~ rx:perl5/(?i)a[b-d]e/ && $0', "ACE", 're_tests 441/0 (#551)', :todo);
is(eval '"ACE" ~~ rx:i/a<[b-d]>e/ && $0', "ACE", 're_tests 442/0 (#552)', :todo);
# 305: 'a[b-d]'i	AAC	y	$&	AC
is(eval '"AAC" ~~ rx:perl5/(?i)a[b-d]/ && $0', "AC", 're_tests 443/0 (#553)', :todo);
is(eval '"AAC" ~~ rx:i/a<[b-d]>/ && $0', "AC", 're_tests 444/0 (#554)', :todo);
# 306: 'a[-b]'i	A-	y	$&	A-
is(eval '"A-" ~~ rx:perl5/(?i)a[-b]/ && $0', "A-", 're_tests 445/0 (#555)', :todo);
is(eval '"A-" ~~ rx:i/a<[-b]>/ && $0', "A-", 're_tests 446/0 (#556)', :todo);
# 307: 'a[b-]'i	A-	y	$&	A-
is(eval '"A-" ~~ rx:perl5/(?i)a[b-]/ && $0', "A-", 're_tests 447/0 (#557)', :todo);
is(eval '"A-" ~~ rx:i/a<[b-]>/ && $0', "A-", 're_tests 448/0 (#558)', :todo);
# 308: 'a[b-a]'i	-	c	-	Invalid [] range "b-a"
# -- SKIPPED - TESTS ERROR MESSAGE
# 309: 'a[]b'i	-	c	-	Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 310: 'a['i	-	c	-	Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 311: 'a]'i	A]	y	$&	A]
is(eval '"A]" ~~ rx:perl5/(?i)a]/ && $0', "A]", 're_tests 449/0 (#559)', :todo);
is(eval '"A]" ~~ rx:i/a]/ && $0', "A]", 're_tests 450/0 (#560)', :todo);
# 312: 'a[]]b'i	A]B	y	$&	A]B
is(eval '"A]B" ~~ rx:perl5/(?i)a[]]b/ && $0', "A]B", 're_tests 451/0 (#561)', :todo);
is(eval '"A]B" ~~ rx:i/a<[]>]b/ && $0', "A]B", 're_tests 452/0 (#562)', :todo);
# 313: 'a[^bc]d'i	AED	y	$&	AED
is(eval '"AED" ~~ rx:perl5/(?i)a[^bc]d/ && $0', "AED", 're_tests 453/0 (#563)', :todo);
is(eval '"AED" ~~ rx:i/a<[^bc]>d/ && $0', "AED", 're_tests 454/0 (#564)', :todo);
# 314: 'a[^bc]d'i	ABD	n	-	-
ok(eval 'not ("ABD" ~~ rx:perl5/(?i)a[^bc]d/)', 're_tests 455  (#565)', :todo);
ok(eval 'not ("ABD" ~~ rx:i/a<[^bc]>d/)', 're_tests 456  (#566)', :todo);
# 315: 'a[^-b]c'i	ADC	y	$&	ADC
is(eval '"ADC" ~~ rx:perl5/(?i)a[^-b]c/ && $0', "ADC", 're_tests 457/0 (#567)', :todo);
is(eval '"ADC" ~~ rx:i/a<[^-b]>c/ && $0', "ADC", 're_tests 458/0 (#568)', :todo);
# 316: 'a[^-b]c'i	A-C	n	-	-
ok(eval 'not ("A-C" ~~ rx:perl5/(?i)a[^-b]c/)', 're_tests 459  (#569)', :todo);
ok(eval 'not ("A-C" ~~ rx:i/a<[^-b]>c/)', 're_tests 460  (#570)', :todo);
# 317: 'a[^]b]c'i	A]C	n	-	-
ok(eval 'not ("A]C" ~~ rx:perl5/(?i)a[^]b]c/)', 're_tests 461  (#571)', :todo);
ok(eval 'not ("A]C" ~~ rx:i/a<[^]>b]c/)', 're_tests 462  (#572)', :todo);
# 318: 'a[^]b]c'i	ADC	y	$&	ADC
is(eval '"ADC" ~~ rx:perl5/(?i)a[^]b]c/ && $0', "ADC", 're_tests 463/0 (#573)', :todo);
is(eval '"ADC" ~~ rx:i/a<[^]>b]c/ && $0', "ADC", 're_tests 464/0 (#574)', :todo);
# 319: 'ab|cd'i	ABC	y	$&	AB
is(eval '"ABC" ~~ rx:perl5/(?i)ab|cd/ && $0', "AB", 're_tests 465/0 (#575)', :todo);
is(eval '"ABC" ~~ rx:i/ab|cd/ && $0', "AB", 're_tests 466/0 (#576)', :todo);
# 320: 'ab|cd'i	ABCD	y	$&	AB
is(eval '"ABCD" ~~ rx:perl5/(?i)ab|cd/ && $0', "AB", 're_tests 467/0 (#577)', :todo);
is(eval '"ABCD" ~~ rx:i/ab|cd/ && $0', "AB", 're_tests 468/0 (#578)', :todo);
# 321: '()ef'i	DEF	y	$&-$1	EF-
is(eval '"DEF" ~~ rx:perl5/(?i)()ef/ && $0', "EF", 're_tests 469/0 (#579)', :todo);
is(eval '"DEF" ~~ rx:perl5/(?i)()ef/ && $1', "", 're_tests 469/1 (#580)', :todo);
is(eval '"DEF" ~~ rx:i/()ef/ && $0', "EF", 're_tests 470/0 (#581)', :todo);
is(eval '"DEF" ~~ rx:i/()ef/ && $1', "", 're_tests 470/1 (#582)', :todo);
# 322: '*a'i	-	c	-	Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 323: '(*)b'i	-	c	-	Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 324: '$b'i	B	n	-	-
ok(eval 'not ("B" ~~ rx:perl5/(?i)$b/)', 're_tests 471  (#583)', :todo);
ok(eval 'not ("B" ~~ rx:i/$b/)', 're_tests 472  (#584)', :todo);
# 325: 'a\'i	-	c	-	Search pattern not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
# 326: 'a\(b'i	A(B	y	$&-$1	A(B-
is(eval '"A(B" ~~ rx:perl5/(?i)a\(b/ && $0', "A(B", 're_tests 473/0 (#585)', :todo);
is(eval '"A(B" ~~ rx:perl5/(?i)a\(b/ && $1', "", 're_tests 473/1 (#586)', :todo);
is(eval '"A(B" ~~ rx:i/a\(b/ && $0', "A(B", 're_tests 474/0 (#587)', :todo);
is(eval '"A(B" ~~ rx:i/a\(b/ && $1', "", 're_tests 474/1 (#588)', :todo);
# 327: 'a\(*b'i	AB	y	$&	AB
is(eval '"AB" ~~ rx:perl5/(?i)a\(*b/ && $0', "AB", 're_tests 475/0 (#589)', :todo);
is(eval '"AB" ~~ rx:i/a\(*b/ && $0', "AB", 're_tests 476/0 (#590)', :todo);
# 328: 'a\(*b'i	A((B	y	$&	A((B
is(eval '"A((B" ~~ rx:perl5/(?i)a\(*b/ && $0', "A((B", 're_tests 477/0 (#591)', :todo);
is(eval '"A((B" ~~ rx:i/a\(*b/ && $0', "A((B", 're_tests 478/0 (#592)', :todo);
# 329: 'a\\b'i	A\B	y	$&	A\B
is(eval '"A\B" ~~ rx:perl5/(?i)a\\b/ && $0', "A\B", 're_tests 479/0 (#593)', :todo);
is(eval '"A\B" ~~ rx:i/a\\b/ && $0', "A\B", 're_tests 480/0 (#594)', :todo);
# 330: 'abc)'i	-	c	-	Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 331: '(abc'i	-	c	-	Unmatched (
# -- SKIPPED - TESTS ERROR MESSAGE
# 332: '((a))'i	ABC	y	$&-$1-$2	A-A-A
is(eval '"ABC" ~~ rx:perl5/(?i)((a))/ && $0', "A", 're_tests 481/0 (#595)', :todo);
is(eval '"ABC" ~~ rx:perl5/(?i)((a))/ && $1', "A", 're_tests 481/1 (#596)', :todo);
is(eval '"ABC" ~~ rx:perl5/(?i)((a))/ && $2', "A", 're_tests 481/2 (#597)', :todo);
is(eval '"ABC" ~~ rx:i/((a))/ && $0', "A", 're_tests 482/0 (#598)', :todo);
is(eval '"ABC" ~~ rx:i/((a))/ && $1', "A", 're_tests 482/1 (#599)', :todo);
is(eval '"ABC" ~~ rx:i/((a))/ && $2', "A", 're_tests 482/2 (#600)', :todo);
# 333: '(a)b(c)'i	ABC	y	$&-$1-$2	ABC-A-C
is(eval '"ABC" ~~ rx:perl5/(?i)(a)b(c)/ && $0', "ABC", 're_tests 483/0 (#601)', :todo);
is(eval '"ABC" ~~ rx:perl5/(?i)(a)b(c)/ && $1', "A", 're_tests 483/1 (#602)', :todo);
is(eval '"ABC" ~~ rx:perl5/(?i)(a)b(c)/ && $2', "C", 're_tests 483/2 (#603)', :todo);
is(eval '"ABC" ~~ rx:i/(a)b(c)/ && $0', "ABC", 're_tests 484/0 (#604)', :todo);
is(eval '"ABC" ~~ rx:i/(a)b(c)/ && $1', "A", 're_tests 484/1 (#605)', :todo);
is(eval '"ABC" ~~ rx:i/(a)b(c)/ && $2', "C", 're_tests 484/2 (#606)', :todo);
# 334: 'a+b+c'i	AABBABC	y	$&	ABC
is(eval '"AABBABC" ~~ rx:perl5/(?i)a+b+c/ && $0', "ABC", 're_tests 485/0 (#607)', :todo);
is(eval '"AABBABC" ~~ rx:i/a+b+c/ && $0', "ABC", 're_tests 486/0 (#608)', :todo);
# 335: 'a{1,}b{1,}c'i	AABBABC	y	$&	ABC
is(eval '"AABBABC" ~~ rx:perl5/(?i)a{1,}b{1,}c/ && $0', "ABC", 're_tests 487/0 (#609)', :todo);
is(eval '"AABBABC" ~~ rx:i/a**{1...}b**{1...}c/ && $0', "ABC", 're_tests 488/0 (#610)', :todo);
# 336: 'a**'i	-	c	-	Nested quantifiers
# -- SKIPPED - TESTS ERROR MESSAGE
# 337: 'a.+?c'i	ABCABC	y	$&	ABC
is(eval '"ABCABC" ~~ rx:perl5/(?i)a.+?c/ && $0', "ABC", 're_tests 489/0 (#611)', :todo);
is(eval '"ABCABC" ~~ rx:i/a\N+?c/ && $0', "ABC", 're_tests 490/0 (#612)', :todo);
# 338: 'a.*?c'i	ABCABC	y	$&	ABC
is(eval '"ABCABC" ~~ rx:perl5/(?i)a.*?c/ && $0', "ABC", 're_tests 491/0 (#613)', :todo);
is(eval '"ABCABC" ~~ rx:i/a\N*?c/ && $0', "ABC", 're_tests 492/0 (#614)', :todo);
# 339: 'a.{0,5}?c'i	ABCABC	y	$&	ABC
is(eval '"ABCABC" ~~ rx:perl5/(?i)a.{0,5}?c/ && $0', "ABC", 're_tests 493/0 (#615)', :todo);
is(eval '"ABCABC" ~~ rx:i/a\N**{0..5}?c/ && $0', "ABC", 're_tests 494/0 (#616)', :todo);
# 340: '(a+|b)*'i	AB	y	$&-$1	AB-B
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b)*/ && $0', "AB", 're_tests 495/0 (#617)', :todo);
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b)*/ && $1', "B", 're_tests 495/1 (#618)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)*/ && $0', "AB", 're_tests 496/0 (#619)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)*/ && $1', "B", 're_tests 496/1 (#620)', :todo);
# 341: '(a+|b){0,}'i	AB	y	$&-$1	AB-B
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){0,}/ && $0', "AB", 're_tests 497/0 (#621)', :todo);
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){0,}/ && $1', "B", 're_tests 497/1 (#622)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{0...}/ && $0', "AB", 're_tests 498/0 (#623)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{0...}/ && $1', "B", 're_tests 498/1 (#624)', :todo);
# 342: '(a+|b)+'i	AB	y	$&-$1	AB-B
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b)+/ && $0', "AB", 're_tests 499/0 (#625)', :todo);
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b)+/ && $1', "B", 're_tests 499/1 (#626)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)+/ && $0', "AB", 're_tests 500/0 (#627)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)+/ && $1', "B", 're_tests 500/1 (#628)', :todo);
# 343: '(a+|b){1,}'i	AB	y	$&-$1	AB-B
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){1,}/ && $0', "AB", 're_tests 501/0 (#629)', :todo);
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){1,}/ && $1', "B", 're_tests 501/1 (#630)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{1...}/ && $0', "AB", 're_tests 502/0 (#631)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{1...}/ && $1', "B", 're_tests 502/1 (#632)', :todo);
# 344: '(a+|b)?'i	AB	y	$&-$1	A-A
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b)?/ && $0', "A", 're_tests 503/0 (#633)', :todo);
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b)?/ && $1', "A", 're_tests 503/1 (#634)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)?/ && $0', "A", 're_tests 504/0 (#635)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)?/ && $1', "A", 're_tests 504/1 (#636)', :todo);
# 345: '(a+|b){0,1}'i	AB	y	$&-$1	A-A
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){0,1}/ && $0', "A", 're_tests 505/0 (#637)', :todo);
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){0,1}/ && $1', "A", 're_tests 505/1 (#638)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{0..1}/ && $0', "A", 're_tests 506/0 (#639)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{0..1}/ && $1', "A", 're_tests 506/1 (#640)', :todo);
# 346: '(a+|b){0,1}?'i	AB	y	$&-$1	-
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){0,1}?/ && $0', "", 're_tests 507/0 (#641)', :todo);
is(eval '"AB" ~~ rx:perl5/(?i)(a+|b){0,1}?/ && $1', "", 're_tests 507/1 (#642)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{0..1}?/ && $0', "", 're_tests 508/0 (#643)', :todo);
is(eval '"AB" ~~ rx:i/(a+|b)**{0..1}?/ && $1', "", 're_tests 508/1 (#644)', :todo);
# 347: ')('i	-	c	-	Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 348: '[^ab]*'i	CDE	y	$&	CDE
is(eval '"CDE" ~~ rx:perl5/(?i)[^ab]*/ && $0', "CDE", 're_tests 509/0 (#645)', :todo);
is(eval '"CDE" ~~ rx:i/<[^ab]>*/ && $0', "CDE", 're_tests 510/0 (#646)', :todo);
# 349: 'abc'i		n	-	-
ok(eval 'not ("" ~~ rx:perl5/(?i)abc/)', 're_tests 511  (#647)', :todo);
ok(eval 'not ("" ~~ rx:i/abc/)', 're_tests 512  (#648)', :todo);
# 350: 'a*'i		y	$&	
is(eval '"" ~~ rx:perl5/(?i)a*/ && $0', "", 're_tests 513/0 (#649)', :todo);
is(eval '"" ~~ rx:i/a*/ && $0', "", 're_tests 514/0 (#650)', :todo);
# 351: '([abc])*d'i	ABBBCD	y	$&-$1	ABBBCD-C
is(eval '"ABBBCD" ~~ rx:perl5/(?i)([abc])*d/ && $0', "ABBBCD", 're_tests 515/0 (#651)', :todo);
is(eval '"ABBBCD" ~~ rx:perl5/(?i)([abc])*d/ && $1', "C", 're_tests 515/1 (#652)', :todo);
is(eval '"ABBBCD" ~~ rx:i/(<[abc]>)*d/ && $0', "ABBBCD", 're_tests 516/0 (#653)', :todo);
is(eval '"ABBBCD" ~~ rx:i/(<[abc]>)*d/ && $1', "C", 're_tests 516/1 (#654)', :todo);
# 352: '([abc])*bcd'i	ABCD	y	$&-$1	ABCD-A
is(eval '"ABCD" ~~ rx:perl5/(?i)([abc])*bcd/ && $0', "ABCD", 're_tests 517/0 (#655)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)([abc])*bcd/ && $1', "A", 're_tests 517/1 (#656)', :todo);
is(eval '"ABCD" ~~ rx:i/(<[abc]>)*bcd/ && $0', "ABCD", 're_tests 518/0 (#657)', :todo);
is(eval '"ABCD" ~~ rx:i/(<[abc]>)*bcd/ && $1', "A", 're_tests 518/1 (#658)', :todo);
# 353: 'a|b|c|d|e'i	E	y	$&	E
is(eval '"E" ~~ rx:perl5/(?i)a|b|c|d|e/ && $0', "E", 're_tests 519/0 (#659)', :todo);
is(eval '"E" ~~ rx:i/a|b|c|d|e/ && $0', "E", 're_tests 520/0 (#660)', :todo);
# 354: '(a|b|c|d|e)f'i	EF	y	$&-$1	EF-E
is(eval '"EF" ~~ rx:perl5/(?i)(a|b|c|d|e)f/ && $0', "EF", 're_tests 521/0 (#661)', :todo);
is(eval '"EF" ~~ rx:perl5/(?i)(a|b|c|d|e)f/ && $1', "E", 're_tests 521/1 (#662)', :todo);
is(eval '"EF" ~~ rx:i/(a|b|c|d|e)f/ && $0', "EF", 're_tests 522/0 (#663)', :todo);
is(eval '"EF" ~~ rx:i/(a|b|c|d|e)f/ && $1', "E", 're_tests 522/1 (#664)', :todo);
# 355: 'abcd*efg'i	ABCDEFG	y	$&	ABCDEFG
is(eval '"ABCDEFG" ~~ rx:perl5/(?i)abcd*efg/ && $0', "ABCDEFG", 're_tests 523/0 (#665)', :todo);
is(eval '"ABCDEFG" ~~ rx:i/abcd*efg/ && $0', "ABCDEFG", 're_tests 524/0 (#666)', :todo);
# 356: 'ab*'i	XABYABBBZ	y	$&	AB
is(eval '"XABYABBBZ" ~~ rx:perl5/(?i)ab*/ && $0', "AB", 're_tests 525/0 (#667)', :todo);
is(eval '"XABYABBBZ" ~~ rx:i/ab*/ && $0', "AB", 're_tests 526/0 (#668)', :todo);
# 357: 'ab*'i	XAYABBBZ	y	$&	A
is(eval '"XAYABBBZ" ~~ rx:perl5/(?i)ab*/ && $0', "A", 're_tests 527/0 (#669)', :todo);
is(eval '"XAYABBBZ" ~~ rx:i/ab*/ && $0', "A", 're_tests 528/0 (#670)', :todo);
# 358: '(ab|cd)e'i	ABCDE	y	$&-$1	CDE-CD
is(eval '"ABCDE" ~~ rx:perl5/(?i)(ab|cd)e/ && $0', "CDE", 're_tests 529/0 (#671)', :todo);
is(eval '"ABCDE" ~~ rx:perl5/(?i)(ab|cd)e/ && $1', "CD", 're_tests 529/1 (#672)', :todo);
is(eval '"ABCDE" ~~ rx:i/(ab|cd)e/ && $0', "CDE", 're_tests 530/0 (#673)', :todo);
is(eval '"ABCDE" ~~ rx:i/(ab|cd)e/ && $1', "CD", 're_tests 530/1 (#674)', :todo);
# 359: '[abhgefdc]ij'i	HIJ	y	$&	HIJ
is(eval '"HIJ" ~~ rx:perl5/(?i)[abhgefdc]ij/ && $0', "HIJ", 're_tests 531/0 (#675)', :todo);
is(eval '"HIJ" ~~ rx:i/<[abhgefdc]>ij/ && $0', "HIJ", 're_tests 532/0 (#676)', :todo);
# 360: '^(ab|cd)e'i	ABCDE	n	x$1y	XY
# SKIPPED: script doesn't understand `x$1y' yet
# SKIPPED: script doesn't understand `x$1y' yet
# 361: '(abc|)ef'i	ABCDEF	y	$&-$1	EF-
is(eval '"ABCDEF" ~~ rx:perl5/(?i)(abc|)ef/ && $0', "EF", 're_tests 533/0 (#677)', :todo);
is(eval '"ABCDEF" ~~ rx:perl5/(?i)(abc|)ef/ && $1', "", 're_tests 533/1 (#678)', :todo);
is(eval '"ABCDEF" ~~ rx:i/(abc|)ef/ && $0', "EF", 're_tests 534/0 (#679)', :todo);
is(eval '"ABCDEF" ~~ rx:i/(abc|)ef/ && $1', "", 're_tests 534/1 (#680)', :todo);
# 362: '(a|b)c*d'i	ABCD	y	$&-$1	BCD-B
is(eval '"ABCD" ~~ rx:perl5/(?i)(a|b)c*d/ && $0', "BCD", 're_tests 535/0 (#681)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)(a|b)c*d/ && $1', "B", 're_tests 535/1 (#682)', :todo);
is(eval '"ABCD" ~~ rx:i/(a|b)c*d/ && $0', "BCD", 're_tests 536/0 (#683)', :todo);
is(eval '"ABCD" ~~ rx:i/(a|b)c*d/ && $1', "B", 're_tests 536/1 (#684)', :todo);
# 363: '(ab|ab*)bc'i	ABC	y	$&-$1	ABC-A
is(eval '"ABC" ~~ rx:perl5/(?i)(ab|ab*)bc/ && $0', "ABC", 're_tests 537/0 (#685)', :todo);
is(eval '"ABC" ~~ rx:perl5/(?i)(ab|ab*)bc/ && $1', "A", 're_tests 537/1 (#686)', :todo);
is(eval '"ABC" ~~ rx:i/(ab|ab*)bc/ && $0', "ABC", 're_tests 538/0 (#687)', :todo);
is(eval '"ABC" ~~ rx:i/(ab|ab*)bc/ && $1', "A", 're_tests 538/1 (#688)', :todo);
# 364: 'a([bc]*)c*'i	ABC	y	$&-$1	ABC-BC
is(eval '"ABC" ~~ rx:perl5/(?i)a([bc]*)c*/ && $0', "ABC", 're_tests 539/0 (#689)', :todo);
is(eval '"ABC" ~~ rx:perl5/(?i)a([bc]*)c*/ && $1', "BC", 're_tests 539/1 (#690)', :todo);
is(eval '"ABC" ~~ rx:i/a(<[bc]>*)c*/ && $0', "ABC", 're_tests 540/0 (#691)', :todo);
is(eval '"ABC" ~~ rx:i/a(<[bc]>*)c*/ && $1', "BC", 're_tests 540/1 (#692)', :todo);
# 365: 'a([bc]*)(c*d)'i	ABCD	y	$&-$1-$2	ABCD-BC-D
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]*)(c*d)/ && $0', "ABCD", 're_tests 541/0 (#693)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]*)(c*d)/ && $1', "BC", 're_tests 541/1 (#694)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]*)(c*d)/ && $2', "D", 're_tests 541/2 (#695)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>*)(c*d)/ && $0', "ABCD", 're_tests 542/0 (#696)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>*)(c*d)/ && $1', "BC", 're_tests 542/1 (#697)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>*)(c*d)/ && $2', "D", 're_tests 542/2 (#698)', :todo);
# 366: 'a([bc]+)(c*d)'i	ABCD	y	$&-$1-$2	ABCD-BC-D
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]+)(c*d)/ && $0', "ABCD", 're_tests 543/0 (#699)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]+)(c*d)/ && $1', "BC", 're_tests 543/1 (#700)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]+)(c*d)/ && $2', "D", 're_tests 543/2 (#701)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>+)(c*d)/ && $0', "ABCD", 're_tests 544/0 (#702)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>+)(c*d)/ && $1', "BC", 're_tests 544/1 (#703)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>+)(c*d)/ && $2', "D", 're_tests 544/2 (#704)', :todo);
# 367: 'a([bc]*)(c+d)'i	ABCD	y	$&-$1-$2	ABCD-B-CD
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]*)(c+d)/ && $0', "ABCD", 're_tests 545/0 (#705)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]*)(c+d)/ && $1', "B", 're_tests 545/1 (#706)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(?i)a([bc]*)(c+d)/ && $2', "CD", 're_tests 545/2 (#707)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>*)(c+d)/ && $0', "ABCD", 're_tests 546/0 (#708)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>*)(c+d)/ && $1', "B", 're_tests 546/1 (#709)', :todo);
is(eval '"ABCD" ~~ rx:i/a(<[bc]>*)(c+d)/ && $2', "CD", 're_tests 546/2 (#710)', :todo);
# 368: 'a[bcd]*dcdcde'i	ADCDCDE	y	$&	ADCDCDE
is(eval '"ADCDCDE" ~~ rx:perl5/(?i)a[bcd]*dcdcde/ && $0', "ADCDCDE", 're_tests 547/0 (#711)', :todo);
is(eval '"ADCDCDE" ~~ rx:i/a<[bcd]>*dcdcde/ && $0', "ADCDCDE", 're_tests 548/0 (#712)', :todo);
# 369: 'a[bcd]+dcdcde'i	ADCDCDE	n	-	-
ok(eval 'not ("ADCDCDE" ~~ rx:perl5/(?i)a[bcd]+dcdcde/)', 're_tests 549  (#713)', :todo);
ok(eval 'not ("ADCDCDE" ~~ rx:i/a<[bcd]>+dcdcde/)', 're_tests 550  (#714)', :todo);
# 370: '(ab|a)b*c'i	ABC	y	$&-$1	ABC-AB
is(eval '"ABC" ~~ rx:perl5/(?i)(ab|a)b*c/ && $0', "ABC", 're_tests 551/0 (#715)', :todo);
is(eval '"ABC" ~~ rx:perl5/(?i)(ab|a)b*c/ && $1', "AB", 're_tests 551/1 (#716)', :todo);
is(eval '"ABC" ~~ rx:i/(ab|a)b*c/ && $0', "ABC", 're_tests 552/0 (#717)', :todo);
is(eval '"ABC" ~~ rx:i/(ab|a)b*c/ && $1', "AB", 're_tests 552/1 (#718)', :todo);
# 371: '((a)(b)c)(d)'i	ABCD	y	$1-$2-$3-$4	ABC-A-B-D
# SKIPPED: script doesn't understand `$1-$2-$3-$4' yet
# SKIPPED: script doesn't understand `$1-$2-$3-$4' yet
# 372: '[a-zA-Z_][a-zA-Z0-9_]*'i	ALPHA	y	$&	ALPHA
is(eval '"ALPHA" ~~ rx:perl5/(?i)[a-zA-Z_][a-zA-Z0-9_]*/ && $0', "ALPHA", 're_tests 553/0 (#719)', :todo);
is(eval '"ALPHA" ~~ rx:i/<[a-zA-Z_]><[a-zA-Z0-9_]>*/ && $0', "ALPHA", 're_tests 554/0 (#720)', :todo);
# 373: '^a(bc+|b[eh])g|.h$'i	ABH	y	$&-$1	BH-
is(eval '"ABH" ~~ rx:perl5/(?i)^a(bc+|b[eh])g|.h$/ && $0', "BH", 're_tests 555/0 (#721)', :todo);
is(eval '"ABH" ~~ rx:perl5/(?i)^a(bc+|b[eh])g|.h$/ && $1', "", 're_tests 555/1 (#722)', :todo);
is(eval '"ABH" ~~ rx:i/^a(bc+|b<[eh]>)g|\Nh$/ && $0', "BH", 're_tests 556/0 (#723)', :todo);
is(eval '"ABH" ~~ rx:i/^a(bc+|b<[eh]>)g|\Nh$/ && $1', "", 're_tests 556/1 (#724)', :todo);
# 374: '(bc+d$|ef*g.|h?i(j|k))'i	EFFGZ	y	$&-$1-$2	EFFGZ-EFFGZ-
is(eval '"EFFGZ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $0', "EFFGZ", 're_tests 557/0 (#725)', :todo);
is(eval '"EFFGZ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $1', "EFFGZ", 're_tests 557/1 (#726)', :todo);
is(eval '"EFFGZ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $2', "", 're_tests 557/2 (#727)', :todo);
is(eval '"EFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $0', "EFFGZ", 're_tests 558/0 (#728)', :todo);
is(eval '"EFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $1', "EFFGZ", 're_tests 558/1 (#729)', :todo);
is(eval '"EFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $2', "", 're_tests 558/2 (#730)', :todo);
# 375: '(bc+d$|ef*g.|h?i(j|k))'i	IJ	y	$&-$1-$2	IJ-IJ-J
is(eval '"IJ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $0', "IJ", 're_tests 559/0 (#731)', :todo);
is(eval '"IJ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $1', "IJ", 're_tests 559/1 (#732)', :todo);
is(eval '"IJ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $2', "J", 're_tests 559/2 (#733)', :todo);
is(eval '"IJ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $0', "IJ", 're_tests 560/0 (#734)', :todo);
is(eval '"IJ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $1', "IJ", 're_tests 560/1 (#735)', :todo);
is(eval '"IJ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $2', "J", 're_tests 560/2 (#736)', :todo);
# 376: '(bc+d$|ef*g.|h?i(j|k))'i	EFFG	n	-	-
ok(eval 'not ("EFFG" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/)', 're_tests 561  (#737)', :todo);
ok(eval 'not ("EFFG" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/)', 're_tests 562  (#738)', :todo);
# 377: '(bc+d$|ef*g.|h?i(j|k))'i	BCDD	n	-	-
ok(eval 'not ("BCDD" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/)', 're_tests 563  (#739)', :todo);
ok(eval 'not ("BCDD" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/)', 're_tests 564  (#740)', :todo);
# 378: '(bc+d$|ef*g.|h?i(j|k))'i	REFFGZ	y	$&-$1-$2	EFFGZ-EFFGZ-
is(eval '"REFFGZ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $0', "EFFGZ", 're_tests 565/0 (#741)', :todo);
is(eval '"REFFGZ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $1', "EFFGZ", 're_tests 565/1 (#742)', :todo);
is(eval '"REFFGZ" ~~ rx:perl5/(?i)(bc+d$|ef*g.|h?i(j|k))/ && $2', "", 're_tests 565/2 (#743)', :todo);
is(eval '"REFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $0', "EFFGZ", 're_tests 566/0 (#744)', :todo);
is(eval '"REFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $1', "EFFGZ", 're_tests 566/1 (#745)', :todo);
is(eval '"REFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $2', "", 're_tests 566/2 (#746)', :todo);
# 379: '((((((((((a))))))))))'i	A	y	$10	A
is(eval '"A" ~~ rx:perl5/(?i)((((((((((a))))))))))/ && $10', "A", 're_tests 567/10 (#747)', :todo);
is(eval '"A" ~~ rx:i/((((((((((a))))))))))/ && $10', "A", 're_tests 568/10 (#748)', :todo);
# 380: '((((((((((a))))))))))\10'i	AA	y	$&	AA
is(eval '"AA" ~~ rx:perl5/(?i)((((((((((a))))))))))\10/ && $0', "AA", 're_tests 569/0 (#749)', :todo);
is(eval '"AA" ~~ rx:i/((((((((((a))))))))))$10/ && $0', "AA", 're_tests 570/0 (#750)', :todo);
# 381: '((((((((((a))))))))))${bang}'i	AA	n	-	-
ok(eval 'not ("AA" ~~ rx:perl5/(?i)((((((((((a))))))))))${bang}/)', 're_tests 571  (#751)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 382: '((((((((((a))))))))))${bang}'i	A!	y	$&	A!
is(eval '"A!" ~~ rx:perl5/(?i)((((((((((a))))))))))${bang}/ && $0', "A!", 're_tests 572/0 (#752)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 383: '(((((((((a)))))))))'i	A	y	$&	A
is(eval '"A" ~~ rx:perl5/(?i)(((((((((a)))))))))/ && $0', "A", 're_tests 573/0 (#753)', :todo);
is(eval '"A" ~~ rx:i/(((((((((a)))))))))/ && $0', "A", 're_tests 574/0 (#754)', :todo);
# 384: '(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))'i	A	y	$1	A
is(eval '"A" ~~ rx:perl5/(?i)(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))/ && $1', "A", 're_tests 575/1 (#755)', :todo);
is(eval '"A" ~~ rx:i/[[[[[[[[[(a)]]]]]]]]]/ && $1', "A", 're_tests 576/1 (#756)', :todo);
# 385: '(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))'i	C	y	$1	C
is(eval '"C" ~~ rx:perl5/(?i)(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))/ && $1', "C", 're_tests 577/1 (#757)', :todo);
is(eval '"C" ~~ rx:i/[[[[[[[[[(a|b|c)]]]]]]]]]/ && $1', "C", 're_tests 578/1 (#758)', :todo);
# 386: 'multiple words of text'i	UH-UH	n	-	-
ok(eval 'not ("UH-UH" ~~ rx:perl5/(?i)multiple words of text/)', 're_tests 579  (#759)', :todo);
ok(eval 'not ("UH-UH" ~~ rx:i/multiple<sp>words<sp>of<sp>text/)', 're_tests 580  (#760)', :todo);
# 387: 'multiple words'i	MULTIPLE WORDS, YEAH	y	$&	MULTIPLE WORDS
is(eval '"MULTIPLE WORDS, YEAH" ~~ rx:perl5/(?i)multiple words/ && $0', "MULTIPLE WORDS", 're_tests 581/0 (#761)', :todo);
is(eval '"MULTIPLE WORDS, YEAH" ~~ rx:i/multiple<sp>words/ && $0', "MULTIPLE WORDS", 're_tests 582/0 (#762)', :todo);
# 388: '(.*)c(.*)'i	ABCDE	y	$&-$1-$2	ABCDE-AB-DE
is(eval '"ABCDE" ~~ rx:perl5/(?i)(.*)c(.*)/ && $0', "ABCDE", 're_tests 583/0 (#763)', :todo);
is(eval '"ABCDE" ~~ rx:perl5/(?i)(.*)c(.*)/ && $1', "AB", 're_tests 583/1 (#764)', :todo);
is(eval '"ABCDE" ~~ rx:perl5/(?i)(.*)c(.*)/ && $2', "DE", 're_tests 583/2 (#765)', :todo);
is(eval '"ABCDE" ~~ rx:i/(\N*)c(\N*)/ && $0', "ABCDE", 're_tests 584/0 (#766)', :todo);
is(eval '"ABCDE" ~~ rx:i/(\N*)c(\N*)/ && $1', "AB", 're_tests 584/1 (#767)', :todo);
is(eval '"ABCDE" ~~ rx:i/(\N*)c(\N*)/ && $2', "DE", 're_tests 584/2 (#768)', :todo);
# 389: '\((.*), (.*)\)'i	(A, B)	y	($2, $1)	(B, A)
# SKIPPED: script doesn't understand `($2, $1)' yet
# SKIPPED: script doesn't understand `($2, $1)' yet
# 390: '[k]'i	AB	n	-	-
ok(eval 'not ("AB" ~~ rx:perl5/(?i)[k]/)', 're_tests 585  (#769)', :todo);
ok(eval 'not ("AB" ~~ rx:i/<[k]>/)', 're_tests 586  (#770)', :todo);
# 391: 'abcd'i	ABCD	y	$&-\$&-\\$&	ABCD-$&-\ABCD
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# 392: 'a(bc)d'i	ABCD	y	$1-\$1-\\$1	BC-$1-\BC
# SKIPPED: script doesn't understand `$1-\$1-\\$1' yet
# SKIPPED: script doesn't understand `$1-\$1-\\$1' yet
# 393: 'a[-]?c'i	AC	y	$&	AC
is(eval '"AC" ~~ rx:perl5/(?i)a[-]?c/ && $0', "AC", 're_tests 587/0 (#771)', :todo);
is(eval '"AC" ~~ rx:i/a<[-]>?c/ && $0', "AC", 're_tests 588/0 (#772)', :todo);
# 394: '(abc)\1'i	ABCABC	y	$1	ABC
is(eval '"ABCABC" ~~ rx:perl5/(?i)(abc)\1/ && $1', "ABC", 're_tests 589/1 (#773)', :todo);
is(eval '"ABCABC" ~~ rx:i/(abc)$1/ && $1', "ABC", 're_tests 590/1 (#774)', :todo);
# 395: '([a-c]*)\1'i	ABCABC	y	$1	ABC
is(eval '"ABCABC" ~~ rx:perl5/(?i)([a-c]*)\1/ && $1', "ABC", 're_tests 591/1 (#775)', :todo);
is(eval '"ABCABC" ~~ rx:i/(<[a-c]>*)$1/ && $1', "ABC", 're_tests 592/1 (#776)', :todo);
# 396: a(?!b).	abad	y	$&	ad
is(eval '"abad" ~~ rx:perl5/a(?!b)./ && $0', "ad", 're_tests 593/0 (#777)', :todo);
is(eval '"abad" ~~ rx/a<!before b>\N/ && $0', "ad", 're_tests 594/0 (#778)', :todo);
# 397: a(?=d).	abad	y	$&	ad
is(eval '"abad" ~~ rx:perl5/a(?=d)./ && $0', "ad", 're_tests 595/0 (#779)', :todo);
is(eval '"abad" ~~ rx/a<before d>\N/ && $0', "ad", 're_tests 596/0 (#780)', :todo);
# 398: a(?=c|d).	abad	y	$&	ad
is(eval '"abad" ~~ rx:perl5/a(?=c|d)./ && $0', "ad", 're_tests 597/0 (#781)', :todo);
is(eval '"abad" ~~ rx/a<before c|d>\N/ && $0', "ad", 're_tests 598/0 (#782)', :todo);
# 399: a(?:b|c|d)(.)	ace	y	$1	e
is(eval '"ace" ~~ rx:perl5/a(?:b|c|d)(.)/ && $1', "e", 're_tests 599/1 (#783)', :todo);
is(eval '"ace" ~~ rx/a[b|c|d](\N)/ && $1', "e", 're_tests 600/1 (#784)', :todo);
# 400: a(?:b|c|d)*(.)	ace	y	$1	e
is(eval '"ace" ~~ rx:perl5/a(?:b|c|d)*(.)/ && $1', "e", 're_tests 601/1 (#785)', :todo);
is(eval '"ace" ~~ rx/a[b|c|d]*(\N)/ && $1', "e", 're_tests 602/1 (#786)', :todo);
# 401: a(?:b|c|d)+?(.)	ace	y	$1	e
is(eval '"ace" ~~ rx:perl5/a(?:b|c|d)+?(.)/ && $1', "e", 're_tests 603/1 (#787)', :todo);
is(eval '"ace" ~~ rx/a[b|c|d]+?(\N)/ && $1', "e", 're_tests 604/1 (#788)', :todo);
# 402: a(?:b|c|d)+?(.)	acdbcdbe	y	$1	d
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d)+?(.)/ && $1', "d", 're_tests 605/1 (#789)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]+?(\N)/ && $1', "d", 're_tests 606/1 (#790)', :todo);
# 403: a(?:b|c|d)+(.)	acdbcdbe	y	$1	e
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d)+(.)/ && $1', "e", 're_tests 607/1 (#791)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]+(\N)/ && $1', "e", 're_tests 608/1 (#792)', :todo);
# 404: a(?:b|c|d){2}(.)	acdbcdbe	y	$1	b
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){2}(.)/ && $1', "b", 're_tests 609/1 (#793)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{2}(\N)/ && $1', "b", 're_tests 610/1 (#794)', :todo);
# 405: a(?:b|c|d){4,5}(.)	acdbcdbe	y	$1	b
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){4,5}(.)/ && $1', "b", 're_tests 611/1 (#795)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{4..5}(\N)/ && $1', "b", 're_tests 612/1 (#796)', :todo);
# 406: a(?:b|c|d){4,5}?(.)	acdbcdbe	y	$1	d
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){4,5}?(.)/ && $1', "d", 're_tests 613/1 (#797)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{4..5}?(\N)/ && $1', "d", 're_tests 614/1 (#798)', :todo);
# 407: ((foo)|(bar))*	foobar	y	$1-$2-$3	bar-foo-bar
# SKIPPED: script doesn't understand `$1-$2-$3' yet
# SKIPPED: script doesn't understand `$1-$2-$3' yet
# 408: :(?:	-	c	-	Sequence (? incomplete
# -- SKIPPED - TESTS ERROR MESSAGE
# 409: a(?:b|c|d){6,7}(.)	acdbcdbe	y	$1	e
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){6,7}(.)/ && $1', "e", 're_tests 615/1 (#799)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{6..7}(\N)/ && $1', "e", 're_tests 616/1 (#800)', :todo);
# 410: a(?:b|c|d){6,7}?(.)	acdbcdbe	y	$1	e
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){6,7}?(.)/ && $1', "e", 're_tests 617/1 (#801)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{6..7}?(\N)/ && $1', "e", 're_tests 618/1 (#802)', :todo);
# 411: a(?:b|c|d){5,6}(.)	acdbcdbe	y	$1	e
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){5,6}(.)/ && $1', "e", 're_tests 619/1 (#803)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{5..6}(\N)/ && $1', "e", 're_tests 620/1 (#804)', :todo);
# 412: a(?:b|c|d){5,6}?(.)	acdbcdbe	y	$1	b
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){5,6}?(.)/ && $1', "b", 're_tests 621/1 (#805)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{5..6}?(\N)/ && $1', "b", 're_tests 622/1 (#806)', :todo);
# 413: a(?:b|c|d){5,7}(.)	acdbcdbe	y	$1	e
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){5,7}(.)/ && $1', "e", 're_tests 623/1 (#807)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{5..7}(\N)/ && $1', "e", 're_tests 624/1 (#808)', :todo);
# 414: a(?:b|c|d){5,7}?(.)	acdbcdbe	y	$1	b
is(eval '"acdbcdbe" ~~ rx:perl5/a(?:b|c|d){5,7}?(.)/ && $1', "b", 're_tests 625/1 (#809)', :todo);
is(eval '"acdbcdbe" ~~ rx/a[b|c|d]**{5..7}?(\N)/ && $1', "b", 're_tests 626/1 (#810)', :todo);
# 415: a(?:b|(c|e){1,2}?|d)+?(.)	ace	y	$1$2	ce
# SKIPPED: script doesn't understand `$1$2' yet
# SKIPPED: script doesn't understand `$1$2' yet
# 416: ^(.+)?B	AB	y	$1	A
is(eval '"AB" ~~ rx:perl5/^(.+)?B/ && $1', "A", 're_tests 627/1 (#811)', :todo);
is(eval '"AB" ~~ rx/^(\N+)?B/ && $1', "A", 're_tests 628/1 (#812)', :todo);
# 417: ^([^a-z])|(\^)$	.	y	$1	.
is(eval '"." ~~ rx:perl5/^([^a-z])|(\^)$/ && $1', ".", 're_tests 629/1 (#813)', :todo);
is(eval '"." ~~ rx/^(<[^a-z]>)|(\^)$/ && $1', ".", 're_tests 630/1 (#814)', :todo);
# 418: ^[<>]&	<&OUT	y	$&	<&
is(eval '"<&OUT" ~~ rx:perl5/^[<>]&/ && $0', "<&", 're_tests 631/0 (#815)', :todo);
is(eval '"<&OUT" ~~ rx/^<[<>]>\&/ && $0', "<&", 're_tests 632/0 (#816)', :todo);
# 419: ^(a\1?){4}$	aaaaaaaaaa	y	$1	aaaa
is(eval '"aaaaaaaaaa" ~~ rx:perl5/^(a\1?){4}$/ && $1', "aaaa", 're_tests 633/1 (#817)', :todo);
is(eval '"aaaaaaaaaa" ~~ rx/^(a$1?)**{4}$/ && $1', "aaaa", 're_tests 634/1 (#818)', :todo);
# 420: ^(a\1?){4}$	aaaaaaaaa	n	-	-
ok(eval 'not ("aaaaaaaaa" ~~ rx:perl5/^(a\1?){4}$/)', 're_tests 635  (#819)', :todo);
ok(eval 'not ("aaaaaaaaa" ~~ rx/^(a$1?)**{4}$/)', 're_tests 636  (#820)', :todo);
# 421: ^(a\1?){4}$	aaaaaaaaaaa	n	-	-
ok(eval 'not ("aaaaaaaaaaa" ~~ rx:perl5/^(a\1?){4}$/)', 're_tests 637  (#821)', :todo);
ok(eval 'not ("aaaaaaaaaaa" ~~ rx/^(a$1?)**{4}$/)', 're_tests 638  (#822)', :todo);
# 422: ^(a(?(1)\1)){4}$	aaaaaaaaaa	y	$1	aaaa
is(eval '"aaaaaaaaaa" ~~ rx:perl5/^(a(?(1)\1)){4}$/ && $1', "aaaa", 're_tests 639/1 (#823)', :todo);
is(eval '"aaaaaaaaaa" ~~ rx/^(a[ <(defined $1)> :: $1 ])**{4}$/ && $1', "aaaa", 're_tests 640/1 (#824)', :todo);
# 423: ^(a(?(1)\1)){4}$	aaaaaaaaa	n	-	-
ok(eval 'not ("aaaaaaaaa" ~~ rx:perl5/^(a(?(1)\1)){4}$/)', 're_tests 641  (#825)', :todo);
ok(eval 'not ("aaaaaaaaa" ~~ rx/^(a[ <(defined $1)> :: $1 ])**{4}$/)', 're_tests 642  (#826)', :todo);
# 424: ^(a(?(1)\1)){4}$	aaaaaaaaaaa	n	-	-
ok(eval 'not ("aaaaaaaaaaa" ~~ rx:perl5/^(a(?(1)\1)){4}$/)', 're_tests 643  (#827)', :todo);
ok(eval 'not ("aaaaaaaaaaa" ~~ rx/^(a[ <(defined $1)> :: $1 ])**{4}$/)', 're_tests 644  (#828)', :todo);
# 425: ((a{4})+)	aaaaaaaaa	y	$1	aaaaaaaa
is(eval '"aaaaaaaaa" ~~ rx:perl5/((a{4})+)/ && $1', "aaaaaaaa", 're_tests 645/1 (#829)', :todo);
is(eval '"aaaaaaaaa" ~~ rx/((a**{4})+)/ && $1', "aaaaaaaa", 're_tests 646/1 (#830)', :todo);
# 426: (((aa){2})+)	aaaaaaaaaa	y	$1	aaaaaaaa
is(eval '"aaaaaaaaaa" ~~ rx:perl5/(((aa){2})+)/ && $1', "aaaaaaaa", 're_tests 647/1 (#831)', :todo);
is(eval '"aaaaaaaaaa" ~~ rx/(((aa)**{2})+)/ && $1', "aaaaaaaa", 're_tests 648/1 (#832)', :todo);
# 427: (((a{2}){2})+)	aaaaaaaaaa	y	$1	aaaaaaaa
is(eval '"aaaaaaaaaa" ~~ rx:perl5/(((a{2}){2})+)/ && $1', "aaaaaaaa", 're_tests 649/1 (#833)', :todo);
is(eval '"aaaaaaaaaa" ~~ rx/(((a**{2})**{2})+)/ && $1', "aaaaaaaa", 're_tests 650/1 (#834)', :todo);
# 428: (?:(f)(o)(o)|(b)(a)(r))*	foobar	y	$1:$2:$3:$4:$5:$6	f:o:o:b:a:r
# SKIPPED: script doesn't understand `$1:$2:$3:$4:$5:$6' yet
# SKIPPED: script doesn't understand `$1:$2:$3:$4:$5:$6' yet
# 429: (?<=a)b	ab	y	$&	b
is(eval '"ab" ~~ rx:perl5/(?<=a)b/ && $0', "b", 're_tests 651/0 (#835)', :todo);
is(eval '"ab" ~~ rx/<after a>b/ && $0', "b", 're_tests 652/0 (#836)', :todo);
# 430: (?<=a)b	cb	n	-	-
ok(eval 'not ("cb" ~~ rx:perl5/(?<=a)b/)', 're_tests 653  (#837)', :todo);
ok(eval 'not ("cb" ~~ rx/<after a>b/)', 're_tests 654  (#838)', :todo);
# 431: (?<=a)b	b	n	-	-
ok(eval 'not ("b" ~~ rx:perl5/(?<=a)b/)', 're_tests 655  (#839)', :todo);
ok(eval 'not ("b" ~~ rx/<after a>b/)', 're_tests 656  (#840)', :todo);
# 432: (?<!c)b	ab	y	$&	b
is(eval '"ab" ~~ rx:perl5/(?<!c)b/ && $0', "b", 're_tests 657/0 (#841)', :todo);
is(eval '"ab" ~~ rx/<!after c>b/ && $0', "b", 're_tests 658/0 (#842)', :todo);
# 433: (?<!c)b	cb	n	-	-
ok(eval 'not ("cb" ~~ rx:perl5/(?<!c)b/)', 're_tests 659  (#843)', :todo);
ok(eval 'not ("cb" ~~ rx/<!after c>b/)', 're_tests 660  (#844)', :todo);
# 434: (?<!c)b	b	y	-	-
ok(eval '"b" ~~ rx:perl5/(?<!c)b/', 're_tests 661  (#845)', :todo);
ok(eval '"b" ~~ rx/<!after c>b/', 're_tests 662  (#846)', :todo);
# 435: (?<!c)b	b	y	$&	b
is(eval '"b" ~~ rx:perl5/(?<!c)b/ && $0', "b", 're_tests 663/0 (#847)', :todo);
is(eval '"b" ~~ rx/<!after c>b/ && $0', "b", 're_tests 664/0 (#848)', :todo);
# 436: (?<%)b	-	c	-	Sequence (?<%...) not recognized
# -- SKIPPED - TESTS ERROR MESSAGE
# 437: (?:..)*a	aba	y	$&	aba
is(eval '"aba" ~~ rx:perl5/(?:..)*a/ && $0', "aba", 're_tests 665/0 (#849)', :todo);
is(eval '"aba" ~~ rx/[\N\N]*a/ && $0', "aba", 're_tests 666/0 (#850)', :todo);
# 438: (?:..)*?a	aba	y	$&	a
is(eval '"aba" ~~ rx:perl5/(?:..)*?a/ && $0', "a", 're_tests 667/0 (#851)', :todo);
is(eval '"aba" ~~ rx/[\N\N]*?a/ && $0', "a", 're_tests 668/0 (#852)', :todo);
# 439: ^(?:b|a(?=(.)))*\1	abc	y	$&	ab
is(eval '"abc" ~~ rx:perl5/^(?:b|a(?=(.)))*\1/ && $0', "ab", 're_tests 669/0 (#853)', :todo);
is(eval '"abc" ~~ rx/^[b|a<before (\N)>]*$1/ && $0', "ab", 're_tests 670/0 (#854)', :todo);
# 440: ^(){3,5}	abc	y	a$1	a
# SKIPPED: script doesn't understand `a$1' yet
# SKIPPED: script doesn't understand `a$1' yet
# 441: ^(a+)*ax	aax	y	$1	a
is(eval '"aax" ~~ rx:perl5/^(a+)*ax/ && $1', "a", 're_tests 671/1 (#855)', :todo);
is(eval '"aax" ~~ rx/^(a+)*ax/ && $1', "a", 're_tests 672/1 (#856)', :todo);
# 442: ^((a|b)+)*ax	aax	y	$1	a
is(eval '"aax" ~~ rx:perl5/^((a|b)+)*ax/ && $1', "a", 're_tests 673/1 (#857)', :todo);
is(eval '"aax" ~~ rx/^((a|b)+)*ax/ && $1', "a", 're_tests 674/1 (#858)', :todo);
# 443: ^((a|bc)+)*ax	aax	y	$1	a
is(eval '"aax" ~~ rx:perl5/^((a|bc)+)*ax/ && $1', "a", 're_tests 675/1 (#859)', :todo);
is(eval '"aax" ~~ rx/^((a|bc)+)*ax/ && $1', "a", 're_tests 676/1 (#860)', :todo);
# 444: (a|x)*ab	cab	y	y$1	y
# SKIPPED: script doesn't understand `y$1' yet
# SKIPPED: script doesn't understand `y$1' yet
# 445: (a)*ab	cab	y	y$1	y
# SKIPPED: script doesn't understand `y$1' yet
# SKIPPED: script doesn't understand `y$1' yet
# 446: (?:(?i)a)b	ab	y	$&	ab
is(eval '"ab" ~~ rx:perl5/(?:(?i)a)b/ && $0', "ab", 're_tests 677/0 (#861)', :todo);
is(eval '"ab" ~~ rx/[:i a]b/ && $0', "ab", 're_tests 678/0 (#862)', :todo);
# 447: ((?i)a)b	ab	y	$&:$1	ab:a
is(eval '"ab" ~~ rx:perl5/((?i)a)b/ && $0', "ab", 're_tests 679/0 (#863)', :todo);
is(eval '"ab" ~~ rx:perl5/((?i)a)b/ && $1', "a", 're_tests 679/1 (#864)', :todo);
is(eval '"ab" ~~ rx/(:i a)b/ && $0', "ab", 're_tests 680/0 (#865)', :todo);
is(eval '"ab" ~~ rx/(:i a)b/ && $1', "a", 're_tests 680/1 (#866)', :todo);
# 448: (?:(?i)a)b	Ab	y	$&	Ab
is(eval '"Ab" ~~ rx:perl5/(?:(?i)a)b/ && $0', "Ab", 're_tests 681/0 (#867)', :todo);
is(eval '"Ab" ~~ rx/[:i a]b/ && $0', "Ab", 're_tests 682/0 (#868)', :todo);
# 449: ((?i)a)b	Ab	y	$&:$1	Ab:A
is(eval '"Ab" ~~ rx:perl5/((?i)a)b/ && $0', "Ab", 're_tests 683/0 (#869)', :todo);
is(eval '"Ab" ~~ rx:perl5/((?i)a)b/ && $1', "A", 're_tests 683/1 (#870)', :todo);
is(eval '"Ab" ~~ rx/(:i a)b/ && $0', "Ab", 're_tests 684/0 (#871)', :todo);
is(eval '"Ab" ~~ rx/(:i a)b/ && $1', "A", 're_tests 684/1 (#872)', :todo);
# 450: (?:(?i)a)b	aB	n	-	-
ok(eval 'not ("aB" ~~ rx:perl5/(?:(?i)a)b/)', 're_tests 685  (#873)', :todo);
ok(eval 'not ("aB" ~~ rx/[:i a]b/)', 're_tests 686  (#874)', :todo);
# 451: ((?i)a)b	aB	n	-	-
ok(eval 'not ("aB" ~~ rx:perl5/((?i)a)b/)', 're_tests 687  (#875)', :todo);
ok(eval 'not ("aB" ~~ rx/(:i a)b/)', 're_tests 688  (#876)', :todo);
# 452: (?i:a)b	ab	y	$&	ab
is(eval '"ab" ~~ rx:perl5/(?i:a)b/ && $0', "ab", 're_tests 689/0 (#877)', :todo);
is(eval '"ab" ~~ rx/[:i a]b/ && $0', "ab", 're_tests 690/0 (#878)', :todo);
# 453: ((?i:a))b	ab	y	$&:$1	ab:a
is(eval '"ab" ~~ rx:perl5/((?i:a))b/ && $0', "ab", 're_tests 691/0 (#879)', :todo);
is(eval '"ab" ~~ rx:perl5/((?i:a))b/ && $1', "a", 're_tests 691/1 (#880)', :todo);
is(eval '"ab" ~~ rx/([:i a])b/ && $0', "ab", 're_tests 692/0 (#881)', :todo);
is(eval '"ab" ~~ rx/([:i a])b/ && $1', "a", 're_tests 692/1 (#882)', :todo);
# 454: (?i:a)b	Ab	y	$&	Ab
is(eval '"Ab" ~~ rx:perl5/(?i:a)b/ && $0', "Ab", 're_tests 693/0 (#883)', :todo);
is(eval '"Ab" ~~ rx/[:i a]b/ && $0', "Ab", 're_tests 694/0 (#884)', :todo);
# 455: ((?i:a))b	Ab	y	$&:$1	Ab:A
is(eval '"Ab" ~~ rx:perl5/((?i:a))b/ && $0', "Ab", 're_tests 695/0 (#885)', :todo);
is(eval '"Ab" ~~ rx:perl5/((?i:a))b/ && $1', "A", 're_tests 695/1 (#886)', :todo);
is(eval '"Ab" ~~ rx/([:i a])b/ && $0', "Ab", 're_tests 696/0 (#887)', :todo);
is(eval '"Ab" ~~ rx/([:i a])b/ && $1', "A", 're_tests 696/1 (#888)', :todo);
# 456: (?i:a)b	aB	n	-	-
ok(eval 'not ("aB" ~~ rx:perl5/(?i:a)b/)', 're_tests 697  (#889)', :todo);
ok(eval 'not ("aB" ~~ rx/[:i a]b/)', 're_tests 698  (#890)', :todo);
# 457: ((?i:a))b	aB	n	-	-
ok(eval 'not ("aB" ~~ rx:perl5/((?i:a))b/)', 're_tests 699  (#891)', :todo);
ok(eval 'not ("aB" ~~ rx/([:i a])b/)', 're_tests 700  (#892)', :todo);
# 458: '(?:(?-i)a)b'i	ab	y	$&	ab
is(eval '"ab" ~~ rx:perl5/(?i)(?:(?-i)a)b/ && $0', "ab", 're_tests 701/0 (#893)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 459: '((?-i)a)b'i	ab	y	$&:$1	ab:a
is(eval '"ab" ~~ rx:perl5/(?i)((?-i)a)b/ && $0', "ab", 're_tests 702/0 (#894)', :todo);
is(eval '"ab" ~~ rx:perl5/(?i)((?-i)a)b/ && $1', "a", 're_tests 702/1 (#895)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 460: '(?:(?-i)a)b'i	aB	y	$&	aB
is(eval '"aB" ~~ rx:perl5/(?i)(?:(?-i)a)b/ && $0', "aB", 're_tests 703/0 (#896)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 461: '((?-i)a)b'i	aB	y	$&:$1	aB:a
is(eval '"aB" ~~ rx:perl5/(?i)((?-i)a)b/ && $0', "aB", 're_tests 704/0 (#897)', :todo);
is(eval '"aB" ~~ rx:perl5/(?i)((?-i)a)b/ && $1', "a", 're_tests 704/1 (#898)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 462: '(?:(?-i)a)b'i	Ab	n	-	-
ok(eval 'not ("Ab" ~~ rx:perl5/(?i)(?:(?-i)a)b/)', 're_tests 705  (#899)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 463: '((?-i)a)b'i	Ab	n	-	-
ok(eval 'not ("Ab" ~~ rx:perl5/(?i)((?-i)a)b/)', 're_tests 706  (#900)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 464: '(?:(?-i)a)b'i	aB	y	$&	aB
is(eval '"aB" ~~ rx:perl5/(?i)(?:(?-i)a)b/ && $0', "aB", 're_tests 707/0 (#901)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 465: '((?-i)a)b'i	aB	y	$1	a
is(eval '"aB" ~~ rx:perl5/(?i)((?-i)a)b/ && $1', "a", 're_tests 708/1 (#902)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 466: '(?:(?-i)a)b'i	AB	n	-	-
ok(eval 'not ("AB" ~~ rx:perl5/(?i)(?:(?-i)a)b/)', 're_tests 709  (#903)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 467: '((?-i)a)b'i	AB	n	-	-
ok(eval 'not ("AB" ~~ rx:perl5/(?i)((?-i)a)b/)', 're_tests 710  (#904)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 468: '(?-i:a)b'i	ab	y	$&	ab
is(eval '"ab" ~~ rx:perl5/(?i)(?-i:a)b/ && $0', "ab", 're_tests 711/0 (#905)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 469: '((?-i:a))b'i	ab	y	$&:$1	ab:a
is(eval '"ab" ~~ rx:perl5/(?i)((?-i:a))b/ && $0', "ab", 're_tests 712/0 (#906)', :todo);
is(eval '"ab" ~~ rx:perl5/(?i)((?-i:a))b/ && $1', "a", 're_tests 712/1 (#907)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 470: '(?-i:a)b'i	aB	y	$&	aB
is(eval '"aB" ~~ rx:perl5/(?i)(?-i:a)b/ && $0', "aB", 're_tests 713/0 (#908)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 471: '((?-i:a))b'i	aB	y	$&:$1	aB:a
is(eval '"aB" ~~ rx:perl5/(?i)((?-i:a))b/ && $0', "aB", 're_tests 714/0 (#909)', :todo);
is(eval '"aB" ~~ rx:perl5/(?i)((?-i:a))b/ && $1', "a", 're_tests 714/1 (#910)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 472: '(?-i:a)b'i	Ab	n	-	-
ok(eval 'not ("Ab" ~~ rx:perl5/(?i)(?-i:a)b/)', 're_tests 715  (#911)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 473: '((?-i:a))b'i	Ab	n	-	-
ok(eval 'not ("Ab" ~~ rx:perl5/(?i)((?-i:a))b/)', 're_tests 716  (#912)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 474: '(?-i:a)b'i	aB	y	$&	aB
is(eval '"aB" ~~ rx:perl5/(?i)(?-i:a)b/ && $0', "aB", 're_tests 717/0 (#913)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 475: '((?-i:a))b'i	aB	y	$1	a
is(eval '"aB" ~~ rx:perl5/(?i)((?-i:a))b/ && $1', "a", 're_tests 718/1 (#914)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 476: '(?-i:a)b'i	AB	n	-	-
ok(eval 'not ("AB" ~~ rx:perl5/(?i)(?-i:a)b/)', 're_tests 719  (#915)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 477: '((?-i:a))b'i	AB	n	-	-
ok(eval 'not ("AB" ~~ rx:perl5/(?i)((?-i:a))b/)', 're_tests 720  (#916)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 478: '((?-i:a.))b'i	a\nB	n	-	-
ok(eval 'not ("a\nB" ~~ rx:perl5/(?i)((?-i:a.))b/)', 're_tests 721  (#917)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 479: '((?s-i:a.))b'i	a\nB	y	$1	a\n
is(eval '"a\nB" ~~ rx:perl5/(?i)((?s-i:a.))b/ && $1', "a\n", 're_tests 722/1 (#918)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 480: '((?s-i:a.))b'i	B\nB	n	-	-
ok(eval 'not ("B\nB" ~~ rx:perl5/(?i)((?s-i:a.))b/)', 're_tests 723  (#919)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 481: (?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))	cabbbb	y	$&	cabbbb
is(eval '"cabbbb" ~~ rx:perl5/(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))/ && $0', "cabbbb", 're_tests 724/0 (#920)', :todo);
is(eval '"cabbbb" ~~ rx/[c|d][][a[][b][b[]][b[][b]]]/ && $0', "cabbbb", 're_tests 725/0 (#921)', :todo);
# 482: (?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))	caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb	y	$&	caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
is(eval '"caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" ~~ rx:perl5/(?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))/ && $0', "caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", 're_tests 726/0 (#922)', :todo);
is(eval '"caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" ~~ rx/[c|d][][aaaaaaaa[][bbbbbbbb][bbbbbbbb[]][bbbbbbbb[][bbbbbbbb]]]/ && $0', "caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", 're_tests 727/0 (#923)', :todo);
# 483: '(ab)\d\1'i	Ab4ab	y	$1	Ab
is(eval '"Ab4ab" ~~ rx:perl5/(?i)(ab)\d\1/ && $1', "Ab", 're_tests 728/1 (#924)', :todo);
is(eval '"Ab4ab" ~~ rx:i/(ab)\d$1/ && $1', "Ab", 're_tests 729/1 (#925)', :todo);
# 484: '(ab)\d\1'i	ab4Ab	y	$1	ab
is(eval '"ab4Ab" ~~ rx:perl5/(?i)(ab)\d\1/ && $1', "ab", 're_tests 730/1 (#926)', :todo);
is(eval '"ab4Ab" ~~ rx:i/(ab)\d$1/ && $1', "ab", 're_tests 731/1 (#927)', :todo);
# 485: foo\w*\d{4}baz	foobar1234baz	y	$&	foobar1234baz
is(eval '"foobar1234baz" ~~ rx:perl5/foo\w*\d{4}baz/ && $0', "foobar1234baz", 're_tests 732/0 (#928)', :todo);
is(eval '"foobar1234baz" ~~ rx/foo\w*\d**{4}baz/ && $0', "foobar1234baz", 're_tests 733/0 (#929)', :todo);
# 486: a(?{})b	cabd	y	$&	ab
is(eval '"cabd" ~~ rx:perl5/a(?{})b/ && $0', "ab", 're_tests 734/0 (#930)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 487: a(?{)b	-	c	-	Sequence (?{...}) not terminated or not {}-balanced
# -- SKIPPED - TESTS ERROR MESSAGE
# 488: a(?{{})b	-	c	-	Sequence (?{...}) not terminated or not {}-balanced
# -- SKIPPED - TESTS ERROR MESSAGE
# 489: a(?{}})b	-	c	-	
# -- SKIPPED - TESTS ERROR MESSAGE
# 490: a(?{"{"})b	-	c	-	Sequence (?{...}) not terminated or not {}-balanced
# -- SKIPPED - TESTS ERROR MESSAGE
# 491: a(?{"\{"})b	cabd	y	$&	ab
is(eval '"cabd" ~~ rx:perl5/a(?{"\{"})b/ && $0', "ab", 're_tests 735/0 (#931)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 492: a(?{"{"}})b	-	c	-	Unmatched right curly bracket
# -- SKIPPED - TESTS ERROR MESSAGE
# 493: a(?{$bl="\{"}).b	caxbd	y	$bl	{
# SKIPPED: script doesn't understand `$bl' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 494: x(~~)*(?:(?:F)?)?	x~~	y	-	-
ok(eval '"x~~" ~~ rx:perl5/x(~~)*(?:(?:F)?)?/', 're_tests 736  (#932)', :todo);
ok(eval '"x~~" ~~ rx/x(~~)*[[F]?]?/', 're_tests 737  (#933)', :todo);
# 495: ^a(?#xxx){3}c	aaac	y	$&	aaac
is(eval '"aaac" ~~ rx:perl5/^a(?#xxx){3}c/ && $0', "aaac", 're_tests 738/0 (#934)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?#...' yet
# 496: '^a (?#xxx) (?#yyy) {3}c'x	aaac	y	$&	aaac
is(eval '"aaac" ~~ rx:perl5/(?x)^a (?#xxx) (?#yyy) {3}c/ && $0', "aaac", 're_tests 739/0 (#935)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?#...' yet
# 497: (?<![cd])b	dbcb	n	-	-
ok(eval 'not ("dbcb" ~~ rx:perl5/(?<![cd])b/)', 're_tests 740  (#936)', :todo);
ok(eval 'not ("dbcb" ~~ rx/<!after <[cd]>>b/)', 're_tests 741  (#937)', :todo);
# 498: (?<![cd])[ab]	dbaacb	y	$&	a
is(eval '"dbaacb" ~~ rx:perl5/(?<![cd])[ab]/ && $0', "a", 're_tests 742/0 (#938)', :todo);
is(eval '"dbaacb" ~~ rx/<!after <[cd]>><[ab]>/ && $0', "a", 're_tests 743/0 (#939)', :todo);
# 499: (?<!(c|d))b	dbcb	n	-	-
ok(eval 'not ("dbcb" ~~ rx:perl5/(?<!(c|d))b/)', 're_tests 744  (#940)', :todo);
ok(eval 'not ("dbcb" ~~ rx/<!after (c|d)>b/)', 're_tests 745  (#941)', :todo);
# 500: (?<!(c|d))[ab]	dbaacb	y	$&	a
is(eval '"dbaacb" ~~ rx:perl5/(?<!(c|d))[ab]/ && $0', "a", 're_tests 746/0 (#942)', :todo);
is(eval '"dbaacb" ~~ rx/<!after (c|d)><[ab]>/ && $0', "a", 're_tests 747/0 (#943)', :todo);
# 501: (?<!cd)[ab]	cdaccb	y	$&	b
is(eval '"cdaccb" ~~ rx:perl5/(?<!cd)[ab]/ && $0', "b", 're_tests 748/0 (#944)', :todo);
is(eval '"cdaccb" ~~ rx/<!after cd><[ab]>/ && $0', "b", 're_tests 749/0 (#945)', :todo);
# 502: ^(?:a?b?)*$	a--	n	-	-
ok(eval 'not ("a--" ~~ rx:perl5/^(?:a?b?)*$/)', 're_tests 750  (#946)', :todo);
ok(eval 'not ("a--" ~~ rx/^[a?b?]*$/)', 're_tests 751  (#947)', :todo);
# 503: ((?s)^a(.))((?m)^b$)	a\nb\nc\n	y	$1;$2;$3	a\n;\n;b
# SKIPPED: script doesn't understand `$1;$2;$3' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 504: ((?m)^b$)	a\nb\nc\n	y	$1	b
is(eval '"a\nb\nc\n" ~~ rx:perl5/((?m)^b$)/ && $1', "b", 're_tests 752/1 (#948)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 505: (?m)^b	a\nb\n	y	$&	b
is(eval '"a\nb\n" ~~ rx:perl5/(?m)^b/ && $0', "b", 're_tests 753/0 (#949)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 506: (?m)^(b)	a\nb\n	y	$1	b
is(eval '"a\nb\n" ~~ rx:perl5/(?m)^(b)/ && $1', "b", 're_tests 754/1 (#950)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 507: ((?m)^b)	a\nb\n	y	$1	b
is(eval '"a\nb\n" ~~ rx:perl5/((?m)^b)/ && $1', "b", 're_tests 755/1 (#951)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 508: \n((?m)^b)	a\nb\n	y	$1	b
is(eval '"a\nb\n" ~~ rx:perl5/\n((?m)^b)/ && $1', "b", 're_tests 756/1 (#952)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 509: ((?s).)c(?!.)	a\nb\nc\n	y	$1	\n
is(eval '"a\nb\nc\n" ~~ rx:perl5/((?s).)c(?!.)/ && $1', "\n", 're_tests 757/1 (#953)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 510: ((?s).)c(?!.)	a\nb\nc\n	y	$1:$&	\n:\nc
# SKIPPED: script doesn't understand `$1:$&' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 511: ((?s)b.)c(?!.)	a\nb\nc\n	y	$1	b\n
is(eval '"a\nb\nc\n" ~~ rx:perl5/((?s)b.)c(?!.)/ && $1', "b\n", 're_tests 758/1 (#954)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 512: ((?s)b.)c(?!.)	a\nb\nc\n	y	$1:$&	b\n:b\nc
# SKIPPED: script doesn't understand `$1:$&' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 513: ^b	a\nb\nc\n	n	-	-
ok(eval 'not ("a\nb\nc\n" ~~ rx:perl5/^b/)', 're_tests 759  (#955)', :todo);
ok(eval 'not ("a\nb\nc\n" ~~ rx/^b/)', 're_tests 760  (#956)', :todo);
# 514: ()^b	a\nb\nc\n	n	-	-
ok(eval 'not ("a\nb\nc\n" ~~ rx:perl5/()^b/)', 're_tests 761  (#957)', :todo);
ok(eval 'not ("a\nb\nc\n" ~~ rx/()^b/)', 're_tests 762  (#958)', :todo);
# 515: ((?m)^b)	a\nb\nc\n	y	$1	b
is(eval '"a\nb\nc\n" ~~ rx:perl5/((?m)^b)/ && $1', "b", 're_tests 763/1 (#959)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 516: (?(1)a|b)	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/(?(1)a|b)/)', 're_tests 764  (#960)', :todo);
ok(eval 'not ("a" ~~ rx/[ <(defined $1)> :: a|b ]/)', 're_tests 765  (#961)', :todo);
# 517: (?(1)b|a)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/(?(1)b|a)/ && $0', "a", 're_tests 766/0 (#962)', :todo);
is(eval '"a" ~~ rx/[ <(defined $1)> :: b|a ]/ && $0', "a", 're_tests 767/0 (#963)', :todo);
# 518: (x)?(?(1)a|b)	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/(x)?(?(1)a|b)/)', 're_tests 768  (#964)', :todo);
ok(eval 'not ("a" ~~ rx/(x)?[ <(defined $1)> :: a|b ]/)', 're_tests 769  (#965)', :todo);
# 519: (x)?(?(1)b|a)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/(x)?(?(1)b|a)/ && $0', "a", 're_tests 770/0 (#966)', :todo);
is(eval '"a" ~~ rx/(x)?[ <(defined $1)> :: b|a ]/ && $0', "a", 're_tests 771/0 (#967)', :todo);
# 520: ()?(?(1)b|a)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/()?(?(1)b|a)/ && $0', "a", 're_tests 772/0 (#968)', :todo);
is(eval '"a" ~~ rx/()?[ <(defined $1)> :: b|a ]/ && $0', "a", 're_tests 773/0 (#969)', :todo);
# 521: ()(?(1)b|a)	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/()(?(1)b|a)/)', 're_tests 774  (#970)', :todo);
ok(eval 'not ("a" ~~ rx/()[ <(defined $1)> :: b|a ]/)', 're_tests 775  (#971)', :todo);
# 522: ()?(?(1)a|b)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/()?(?(1)a|b)/ && $0', "a", 're_tests 776/0 (#972)', :todo);
is(eval '"a" ~~ rx/()?[ <(defined $1)> :: a|b ]/ && $0', "a", 're_tests 777/0 (#973)', :todo);
# 523: ^(\()?blah(?(1)(\)))$	(blah)	y	$2	)
is(eval '"(blah)" ~~ rx:perl5/^(\()?blah(?(1)(\)))$/ && $2', ")", 're_tests 778/2 (#974)', :todo);
is(eval '"(blah)" ~~ rx/^(\()?blah[ <(defined $1)> :: (\)) ]$/ && $2', ")", 're_tests 779/2 (#975)', :todo);
# 524: ^(\()?blah(?(1)(\)))$	blah	y	($2)	()
# SKIPPED: script doesn't understand `($2)' yet
# SKIPPED: script doesn't understand `($2)' yet
# 525: ^(\()?blah(?(1)(\)))$	blah)	n	-	-
ok(eval 'not ("blah)" ~~ rx:perl5/^(\()?blah(?(1)(\)))$/)', 're_tests 780  (#976)', :todo);
ok(eval 'not ("blah)" ~~ rx/^(\()?blah[ <(defined $1)> :: (\)) ]$/)', 're_tests 781  (#977)', :todo);
# 526: ^(\()?blah(?(1)(\)))$	(blah	n	-	-
ok(eval 'not ("(blah" ~~ rx:perl5/^(\()?blah(?(1)(\)))$/)', 're_tests 782  (#978)', :todo);
ok(eval 'not ("(blah" ~~ rx/^(\()?blah[ <(defined $1)> :: (\)) ]$/)', 're_tests 783  (#979)', :todo);
# 527: ^(\(+)?blah(?(1)(\)))$	(blah)	y	$2	)
is(eval '"(blah)" ~~ rx:perl5/^(\(+)?blah(?(1)(\)))$/ && $2', ")", 're_tests 784/2 (#980)', :todo);
is(eval '"(blah)" ~~ rx/^(\(+)?blah[ <(defined $1)> :: (\)) ]$/ && $2', ")", 're_tests 785/2 (#981)', :todo);
# 528: ^(\(+)?blah(?(1)(\)))$	blah	y	($2)	()
# SKIPPED: script doesn't understand `($2)' yet
# SKIPPED: script doesn't understand `($2)' yet
# 529: ^(\(+)?blah(?(1)(\)))$	blah)	n	-	-
ok(eval 'not ("blah)" ~~ rx:perl5/^(\(+)?blah(?(1)(\)))$/)', 're_tests 786  (#982)', :todo);
ok(eval 'not ("blah)" ~~ rx/^(\(+)?blah[ <(defined $1)> :: (\)) ]$/)', 're_tests 787  (#983)', :todo);
# 530: ^(\(+)?blah(?(1)(\)))$	(blah	n	-	-
ok(eval 'not ("(blah" ~~ rx:perl5/^(\(+)?blah(?(1)(\)))$/)', 're_tests 788  (#984)', :todo);
ok(eval 'not ("(blah" ~~ rx/^(\(+)?blah[ <(defined $1)> :: (\)) ]$/)', 're_tests 789  (#985)', :todo);
# 531: (?(1?)a|b)	a	c	-	Switch condition not recognized
# -- SKIPPED - TESTS ERROR MESSAGE
# 532: (?(1)a|b|c)	a	c	-	Switch (?(condition)... contains too many branches
# -- SKIPPED - TESTS ERROR MESSAGE
# 533: (?(?{0})a|b)	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/(?(?{0})a|b)/)', 're_tests 790  (#986)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 534: (?(?{0})b|a)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/(?(?{0})b|a)/ && $0', "a", 're_tests 791/0 (#987)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 535: (?(?{1})b|a)	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/(?(?{1})b|a)/)', 're_tests 792  (#988)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 536: (?(?{1})a|b)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/(?(?{1})a|b)/ && $0', "a", 're_tests 793/0 (#989)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 537: (?(?!a)a|b)	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/(?(?!a)a|b)/)', 're_tests 794  (#990)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 538: (?(?!a)b|a)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/(?(?!a)b|a)/ && $0', "a", 're_tests 795/0 (#991)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 539: (?(?=a)b|a)	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/(?(?=a)b|a)/)', 're_tests 796  (#992)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 540: (?(?=a)a|b)	a	y	$&	a
is(eval '"a" ~~ rx:perl5/(?(?=a)a|b)/ && $0', "a", 're_tests 797/0 (#993)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 541: (?=(a+?))(\1ab)	aaab	y	$2	aab
is(eval '"aaab" ~~ rx:perl5/(?=(a+?))(\1ab)/ && $2', "aab", 're_tests 798/2 (#994)', :todo);
is(eval '"aaab" ~~ rx/<before (a+?)>($1ab)/ && $2', "aab", 're_tests 799/2 (#995)', :todo);
# 542: ^(?=(a+?))\1ab	aaab	n	-	-
ok(eval 'not ("aaab" ~~ rx:perl5/^(?=(a+?))\1ab/)', 're_tests 800  (#996)', :todo);
ok(eval 'not ("aaab" ~~ rx/^<before (a+?)>$1ab/)', 're_tests 801  (#997)', :todo);
# 543: (\w+:)+	one:	y	$1	one:
is(eval '"one:" ~~ rx:perl5/(\w+:)+/ && $1', "one:", 're_tests 802/1 (#998)', :todo);
is(eval '"one:" ~~ rx/(\w+\:)+/ && $1', "one:", 're_tests 803/1 (#999)', :todo);
# 544: $(?<=^(a))	a	y	$1	a
is(eval '"a" ~~ rx:perl5/$(?<=^(a))/ && $1', "a", 're_tests 804/1 (#1000)', :todo);
is(eval '"a" ~~ rx/$<after ^(a)>/ && $1', "a", 're_tests 805/1 (#1001)', :todo);
# 545: (?=(a+?))(\1ab)	aaab	y	$2	aab
is(eval '"aaab" ~~ rx:perl5/(?=(a+?))(\1ab)/ && $2', "aab", 're_tests 806/2 (#1002)', :todo);
is(eval '"aaab" ~~ rx/<before (a+?)>($1ab)/ && $2', "aab", 're_tests 807/2 (#1003)', :todo);
# 546: ^(?=(a+?))\1ab	aaab	n	-	-
ok(eval 'not ("aaab" ~~ rx:perl5/^(?=(a+?))\1ab/)', 're_tests 808  (#1004)', :todo);
ok(eval 'not ("aaab" ~~ rx/^<before (a+?)>$1ab/)', 're_tests 809  (#1005)', :todo);
# 547: ([\w:]+::)?(\w+)$	abcd:	n	-	-
ok(eval 'not ("abcd:" ~~ rx:perl5/([\w:]+::)?(\w+)$/)', 're_tests 810  (#1006)', :todo);
ok(eval 'not ("abcd:" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/)', 're_tests 811  (#1007)', :todo);
# 548: ([\w:]+::)?(\w+)$	abcd	y	$1-$2	-abcd
is(eval '"abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $1', "", 're_tests 812/1 (#1008)', :todo);
is(eval '"abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $2', "abcd", 're_tests 812/2 (#1009)', :todo);
is(eval '"abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $1', "", 're_tests 813/1 (#1010)', :todo);
is(eval '"abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $2', "abcd", 're_tests 813/2 (#1011)', :todo);
# 549: ([\w:]+::)?(\w+)$	xy:z:::abcd	y	$1-$2	xy:z:::-abcd
is(eval '"xy:z:::abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $1', "xy:z:::", 're_tests 814/1 (#1012)', :todo);
is(eval '"xy:z:::abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $2', "abcd", 're_tests 814/2 (#1013)', :todo);
is(eval '"xy:z:::abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $1', "xy:z:::", 're_tests 815/1 (#1014)', :todo);
is(eval '"xy:z:::abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $2', "abcd", 're_tests 815/2 (#1015)', :todo);
# 550: ^[^bcd]*(c+)	aexycd	y	$1	c
is(eval '"aexycd" ~~ rx:perl5/^[^bcd]*(c+)/ && $1', "c", 're_tests 816/1 (#1016)', :todo);
is(eval '"aexycd" ~~ rx/^<[^bcd]>*(c+)/ && $1', "c", 're_tests 817/1 (#1017)', :todo);
# 551: (a*)b+	caab	y	$1	aa
is(eval '"caab" ~~ rx:perl5/(a*)b+/ && $1', "aa", 're_tests 818/1 (#1018)', :todo);
is(eval '"caab" ~~ rx/(a*)b+/ && $1', "aa", 're_tests 819/1 (#1019)', :todo);
# 552: ([\w:]+::)?(\w+)$	abcd:	n	-	-
ok(eval 'not ("abcd:" ~~ rx:perl5/([\w:]+::)?(\w+)$/)', 're_tests 820  (#1020)', :todo);
ok(eval 'not ("abcd:" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/)', 're_tests 821  (#1021)', :todo);
# 553: ([\w:]+::)?(\w+)$	abcd	y	$1-$2	-abcd
is(eval '"abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $1', "", 're_tests 822/1 (#1022)', :todo);
is(eval '"abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $2', "abcd", 're_tests 822/2 (#1023)', :todo);
is(eval '"abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $1', "", 're_tests 823/1 (#1024)', :todo);
is(eval '"abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $2', "abcd", 're_tests 823/2 (#1025)', :todo);
# 554: ([\w:]+::)?(\w+)$	xy:z:::abcd	y	$1-$2	xy:z:::-abcd
is(eval '"xy:z:::abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $1', "xy:z:::", 're_tests 824/1 (#1026)', :todo);
is(eval '"xy:z:::abcd" ~~ rx:perl5/([\w:]+::)?(\w+)$/ && $2', "abcd", 're_tests 824/2 (#1027)', :todo);
is(eval '"xy:z:::abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $1', "xy:z:::", 're_tests 825/1 (#1028)', :todo);
is(eval '"xy:z:::abcd" ~~ rx/(<[\w:]>+\:\:)?(\w+)$/ && $2', "abcd", 're_tests 825/2 (#1029)', :todo);
# 555: ^[^bcd]*(c+)	aexycd	y	$1	c
is(eval '"aexycd" ~~ rx:perl5/^[^bcd]*(c+)/ && $1', "c", 're_tests 826/1 (#1030)', :todo);
is(eval '"aexycd" ~~ rx/^<[^bcd]>*(c+)/ && $1', "c", 're_tests 827/1 (#1031)', :todo);
# 556: (?{$a=2})a*aa(?{local$a=$a+1})k*c(?{$b=$a})	yaaxxaaaacd	y	$b	3
# SKIPPED: script doesn't understand `$b' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 557: (?{$a=2})(a(?{local$a=$a+1}))*aak*c(?{$b=$a})	yaaxxaaaacd	y	$b	4
# SKIPPED: script doesn't understand `$b' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 558: (>a+)ab	aaab	n	-	-
ok(eval 'not ("aaab" ~~ rx:perl5/(>a+)ab/)', 're_tests 828  (#1032)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 559: (?>a+)b	aaab	y	-	-
ok(eval '"aaab" ~~ rx:perl5/(?>a+)b/', 're_tests 829  (#1033)', :todo);
ok(eval '"aaab" ~~ rx/[a+]:b/', 're_tests 830  (#1034)', :todo);
# 560: ([[:]+)	a:[b]:	y	$1	:[
is(eval '"a:[b]:" ~~ rx:perl5/([[:]+)/ && $1', ":[", 're_tests 831/1 (#1035)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 561: ([[=]+)	a=[b]=	y	$1	=[
is(eval '"a=[b]=" ~~ rx:perl5/([[=]+)/ && $1', "=[", 're_tests 832/1 (#1036)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 562: ([[.]+)	a.[b].	y	$1	.[
is(eval '"a.[b]." ~~ rx:perl5/([[.]+)/ && $1', ".[", 're_tests 833/1 (#1037)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 563: [a[:xyz:	-	c	-	Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 564: [a[:xyz:]	-	c	-	POSIX class [:xyz:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 565: [a[:]b[:c]	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/[a[:]b[:c]/ && $0', "abc", 're_tests 834/0 (#1038)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 566: ([a[:xyz:]b]+)	pbaq	c	-	POSIX class [:xyz:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 567: [a[:]b[:c]	abc	y	$&	abc
is(eval '"abc" ~~ rx:perl5/[a[:]b[:c]/ && $0', "abc", 're_tests 835/0 (#1039)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 568: ([[:alpha:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 569: ([[:alnum:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 570: ([[:ascii:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy__--  ${nulnul}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 571: ([[:cntrl:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	${nulnul}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 572: ([[:digit:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 573: ([[:graph:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy__--
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 574: ([[:lower:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	cd
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 575: ([[:print:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy__--  
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 576: ([[:punct:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	__--
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 577: ([[:space:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	  
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 578: ([[:word:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy__
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 579: ([[:upper:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	AB
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 580: ([[:xdigit:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 581: ([[:^alpha:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 582: ([[:^alnum:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	__--  ${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 583: ([[:^ascii:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 584: ([[:^cntrl:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy__--  
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 585: ([[:^digit:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 586: ([[:^lower:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	AB
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 587: ([[:^print:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 588: ([[:^punct:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 589: ([[:^space:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	ABcd01Xy__--
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 590: ([[:^word:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	--  ${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 591: ([[:^upper:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	cd01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 592: ([[:^xdigit:]]+)	ABcd01Xy__--  ${nulnul}${ffff}	y	$1	Xy__--  ${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 593: [[:foo:]]	-	c	-	POSIX class [:foo:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 594: [[:^foo:]]	-	c	-	POSIX class [:^foo:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 595: ((?>a+)b)	aaab	y	$1	aaab
is(eval '"aaab" ~~ rx:perl5/((?>a+)b)/ && $1', "aaab", 're_tests 836/1 (#1040)', :todo);
is(eval '"aaab" ~~ rx/([a+]:b)/ && $1', "aaab", 're_tests 837/1 (#1041)', :todo);
# 596: (?>(a+))b	aaab	y	$1	aaa
is(eval '"aaab" ~~ rx:perl5/(?>(a+))b/ && $1', "aaa", 're_tests 838/1 (#1042)', :todo);
is(eval '"aaab" ~~ rx/[(a+)]:b/ && $1', "aaa", 're_tests 839/1 (#1043)', :todo);
# 597: ((?>[^()]+)|\([^()]*\))+	((abc(ade)ufh()()x	y	$&	abc(ade)ufh()()x
is(eval '"((abc(ade)ufh()()x" ~~ rx:perl5/((?>[^()]+)|\([^()]*\))+/ && $0', "abc(ade)ufh()()x", 're_tests 840/0 (#1044)', :todo);
is(eval '"((abc(ade)ufh()()x" ~~ rx/([<[^()]>+]:|\(<[^()]>*\))+/ && $0', "abc(ade)ufh()()x", 're_tests 841/0 (#1045)', :todo);
# 598: (?<=x+)y	-	c	-	Variable length lookbehind not implemented
# -- SKIPPED - TESTS ERROR MESSAGE
# 599: a{37,17}	-	c	-	Can't do {n,m} with n > m
# -- SKIPPED - TESTS ERROR MESSAGE
# 600: \Z	a\nb\n	y	$-[0]	3
is(eval '"a\nb\n" ~~ rx:perl5/\Z/ && getpos($/, 0)', 3, 're_tests 842/0 (#1046)', :todo);
is(eval '"a\nb\n" ~~ rx/\n?$/ && getpos($/, 0)', 3, 're_tests 843/0 (#1047)', :todo);
# 601: \z	a\nb\n	y	$-[0]	4
is(eval '"a\nb\n" ~~ rx:perl5/\z/ && getpos($/, 0)', 4, 're_tests 844/0 (#1048)', :todo);
is(eval '"a\nb\n" ~~ rx/$/ && getpos($/, 0)', 4, 're_tests 845/0 (#1049)', :todo);
# 602: $	a\nb\n	y	$-[0]	3
is(eval '"a\nb\n" ~~ rx:perl5/$/ && getpos($/, 0)', 3, 're_tests 846/0 (#1050)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 603: \Z	b\na\n	y	$-[0]	3
is(eval '"b\na\n" ~~ rx:perl5/\Z/ && getpos($/, 0)', 3, 're_tests 847/0 (#1051)', :todo);
is(eval '"b\na\n" ~~ rx/\n?$/ && getpos($/, 0)', 3, 're_tests 848/0 (#1052)', :todo);
# 604: \z	b\na\n	y	$-[0]	4
is(eval '"b\na\n" ~~ rx:perl5/\z/ && getpos($/, 0)', 4, 're_tests 849/0 (#1053)', :todo);
is(eval '"b\na\n" ~~ rx/$/ && getpos($/, 0)', 4, 're_tests 850/0 (#1054)', :todo);
# 605: $	b\na\n	y	$-[0]	3
is(eval '"b\na\n" ~~ rx:perl5/$/ && getpos($/, 0)', 3, 're_tests 851/0 (#1055)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 606: \Z	b\na	y	$-[0]	3
is(eval '"b\na" ~~ rx:perl5/\Z/ && getpos($/, 0)', 3, 're_tests 852/0 (#1056)', :todo);
is(eval '"b\na" ~~ rx/\n?$/ && getpos($/, 0)', 3, 're_tests 853/0 (#1057)', :todo);
# 607: \z	b\na	y	$-[0]	3
is(eval '"b\na" ~~ rx:perl5/\z/ && getpos($/, 0)', 3, 're_tests 854/0 (#1058)', :todo);
is(eval '"b\na" ~~ rx/$/ && getpos($/, 0)', 3, 're_tests 855/0 (#1059)', :todo);
# 608: $	b\na	y	$-[0]	3
is(eval '"b\na" ~~ rx:perl5/$/ && getpos($/, 0)', 3, 're_tests 856/0 (#1060)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 609: '\Z'm	a\nb\n	y	$-[0]	3
is(eval '"a\nb\n" ~~ rx:perl5/(?m)\Z/ && getpos($/, 0)', 3, 're_tests 857/0 (#1061)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 610: '\z'm	a\nb\n	y	$-[0]	4
is(eval '"a\nb\n" ~~ rx:perl5/(?m)\z/ && getpos($/, 0)', 4, 're_tests 858/0 (#1062)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 611: '$'m	a\nb\n	y	$-[0]	1
is(eval '"a\nb\n" ~~ rx:perl5/(?m)$/ && getpos($/, 0)', 1, 're_tests 859/0 (#1063)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 612: '\Z'm	b\na\n	y	$-[0]	3
is(eval '"b\na\n" ~~ rx:perl5/(?m)\Z/ && getpos($/, 0)', 3, 're_tests 860/0 (#1064)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 613: '\z'm	b\na\n	y	$-[0]	4
is(eval '"b\na\n" ~~ rx:perl5/(?m)\z/ && getpos($/, 0)', 4, 're_tests 861/0 (#1065)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 614: '$'m	b\na\n	y	$-[0]	1
is(eval '"b\na\n" ~~ rx:perl5/(?m)$/ && getpos($/, 0)', 1, 're_tests 862/0 (#1066)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 615: '\Z'm	b\na	y	$-[0]	3
is(eval '"b\na" ~~ rx:perl5/(?m)\Z/ && getpos($/, 0)', 3, 're_tests 863/0 (#1067)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 616: '\z'm	b\na	y	$-[0]	3
is(eval '"b\na" ~~ rx:perl5/(?m)\z/ && getpos($/, 0)', 3, 're_tests 864/0 (#1068)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 617: '$'m	b\na	y	$-[0]	1
is(eval '"b\na" ~~ rx:perl5/(?m)$/ && getpos($/, 0)', 1, 're_tests 865/0 (#1069)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 618: a\Z	a\nb\n	n	-	-
ok(eval 'not ("a\nb\n" ~~ rx:perl5/a\Z/)', 're_tests 866  (#1070)', :todo);
ok(eval 'not ("a\nb\n" ~~ rx/a\n?$/)', 're_tests 867  (#1071)', :todo);
# 619: a\z	a\nb\n	n	-	-
ok(eval 'not ("a\nb\n" ~~ rx:perl5/a\z/)', 're_tests 868  (#1072)', :todo);
ok(eval 'not ("a\nb\n" ~~ rx/a$/)', 're_tests 869  (#1073)', :todo);
# 620: a$	a\nb\n	n	-	-
ok(eval 'not ("a\nb\n" ~~ rx:perl5/a$/)', 're_tests 870  (#1074)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 621: a\Z	b\na\n	y	$-[0]	2
is(eval '"b\na\n" ~~ rx:perl5/a\Z/ && getpos($/, 0)', 2, 're_tests 871/0 (#1075)', :todo);
is(eval '"b\na\n" ~~ rx/a\n?$/ && getpos($/, 0)', 2, 're_tests 872/0 (#1076)', :todo);
# 622: a\z	b\na\n	n	-	-
ok(eval 'not ("b\na\n" ~~ rx:perl5/a\z/)', 're_tests 873  (#1077)', :todo);
ok(eval 'not ("b\na\n" ~~ rx/a$/)', 're_tests 874  (#1078)', :todo);
# 623: a$	b\na\n	y	$-[0]	2
is(eval '"b\na\n" ~~ rx:perl5/a$/ && getpos($/, 0)', 2, 're_tests 875/0 (#1079)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 624: a\Z	b\na	y	$-[0]	2
is(eval '"b\na" ~~ rx:perl5/a\Z/ && getpos($/, 0)', 2, 're_tests 876/0 (#1080)', :todo);
is(eval '"b\na" ~~ rx/a\n?$/ && getpos($/, 0)', 2, 're_tests 877/0 (#1081)', :todo);
# 625: a\z	b\na	y	$-[0]	2
is(eval '"b\na" ~~ rx:perl5/a\z/ && getpos($/, 0)', 2, 're_tests 878/0 (#1082)', :todo);
is(eval '"b\na" ~~ rx/a$/ && getpos($/, 0)', 2, 're_tests 879/0 (#1083)', :todo);
# 626: a$	b\na	y	$-[0]	2
is(eval '"b\na" ~~ rx:perl5/a$/ && getpos($/, 0)', 2, 're_tests 880/0 (#1084)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 627: 'a\Z'm	a\nb\n	n	-	-
ok(eval 'not ("a\nb\n" ~~ rx:perl5/(?m)a\Z/)', 're_tests 881  (#1085)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 628: 'a\z'm	a\nb\n	n	-	-
ok(eval 'not ("a\nb\n" ~~ rx:perl5/(?m)a\z/)', 're_tests 882  (#1086)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 629: 'a$'m	a\nb\n	y	$-[0]	0
is(eval '"a\nb\n" ~~ rx:perl5/(?m)a$/ && getpos($/, 0)', 0, 're_tests 883/0 (#1087)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 630: 'a\Z'm	b\na\n	y	$-[0]	2
is(eval '"b\na\n" ~~ rx:perl5/(?m)a\Z/ && getpos($/, 0)', 2, 're_tests 884/0 (#1088)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 631: 'a\z'm	b\na\n	n	-	-
ok(eval 'not ("b\na\n" ~~ rx:perl5/(?m)a\z/)', 're_tests 885  (#1089)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 632: 'a$'m	b\na\n	y	$-[0]	2
is(eval '"b\na\n" ~~ rx:perl5/(?m)a$/ && getpos($/, 0)', 2, 're_tests 886/0 (#1090)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 633: 'a\Z'm	b\na	y	$-[0]	2
is(eval '"b\na" ~~ rx:perl5/(?m)a\Z/ && getpos($/, 0)', 2, 're_tests 887/0 (#1091)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 634: 'a\z'm	b\na	y	$-[0]	2
is(eval '"b\na" ~~ rx:perl5/(?m)a\z/ && getpos($/, 0)', 2, 're_tests 888/0 (#1092)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 635: 'a$'m	b\na	y	$-[0]	2
is(eval '"b\na" ~~ rx:perl5/(?m)a$/ && getpos($/, 0)', 2, 're_tests 889/0 (#1093)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 636: aa\Z	aa\nb\n	n	-	-
ok(eval 'not ("aa\nb\n" ~~ rx:perl5/aa\Z/)', 're_tests 890  (#1094)', :todo);
ok(eval 'not ("aa\nb\n" ~~ rx/aa\n?$/)', 're_tests 891  (#1095)', :todo);
# 637: aa\z	aa\nb\n	n	-	-
ok(eval 'not ("aa\nb\n" ~~ rx:perl5/aa\z/)', 're_tests 892  (#1096)', :todo);
ok(eval 'not ("aa\nb\n" ~~ rx/aa$/)', 're_tests 893  (#1097)', :todo);
# 638: aa$	aa\nb\n	n	-	-
ok(eval 'not ("aa\nb\n" ~~ rx:perl5/aa$/)', 're_tests 894  (#1098)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 639: aa\Z	b\naa\n	y	$-[0]	2
is(eval '"b\naa\n" ~~ rx:perl5/aa\Z/ && getpos($/, 0)', 2, 're_tests 895/0 (#1099)', :todo);
is(eval '"b\naa\n" ~~ rx/aa\n?$/ && getpos($/, 0)', 2, 're_tests 896/0 (#1100)', :todo);
# 640: aa\z	b\naa\n	n	-	-
ok(eval 'not ("b\naa\n" ~~ rx:perl5/aa\z/)', 're_tests 897  (#1101)', :todo);
ok(eval 'not ("b\naa\n" ~~ rx/aa$/)', 're_tests 898  (#1102)', :todo);
# 641: aa$	b\naa\n	y	$-[0]	2
is(eval '"b\naa\n" ~~ rx:perl5/aa$/ && getpos($/, 0)', 2, 're_tests 899/0 (#1103)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 642: aa\Z	b\naa	y	$-[0]	2
is(eval '"b\naa" ~~ rx:perl5/aa\Z/ && getpos($/, 0)', 2, 're_tests 900/0 (#1104)', :todo);
is(eval '"b\naa" ~~ rx/aa\n?$/ && getpos($/, 0)', 2, 're_tests 901/0 (#1105)', :todo);
# 643: aa\z	b\naa	y	$-[0]	2
is(eval '"b\naa" ~~ rx:perl5/aa\z/ && getpos($/, 0)', 2, 're_tests 902/0 (#1106)', :todo);
is(eval '"b\naa" ~~ rx/aa$/ && getpos($/, 0)', 2, 're_tests 903/0 (#1107)', :todo);
# 644: aa$	b\naa	y	$-[0]	2
is(eval '"b\naa" ~~ rx:perl5/aa$/ && getpos($/, 0)', 2, 're_tests 904/0 (#1108)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 645: 'aa\Z'm	aa\nb\n	n	-	-
ok(eval 'not ("aa\nb\n" ~~ rx:perl5/(?m)aa\Z/)', 're_tests 905  (#1109)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 646: 'aa\z'm	aa\nb\n	n	-	-
ok(eval 'not ("aa\nb\n" ~~ rx:perl5/(?m)aa\z/)', 're_tests 906  (#1110)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 647: 'aa$'m	aa\nb\n	y	$-[0]	0
is(eval '"aa\nb\n" ~~ rx:perl5/(?m)aa$/ && getpos($/, 0)', 0, 're_tests 907/0 (#1111)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 648: 'aa\Z'm	b\naa\n	y	$-[0]	2
is(eval '"b\naa\n" ~~ rx:perl5/(?m)aa\Z/ && getpos($/, 0)', 2, 're_tests 908/0 (#1112)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 649: 'aa\z'm	b\naa\n	n	-	-
ok(eval 'not ("b\naa\n" ~~ rx:perl5/(?m)aa\z/)', 're_tests 909  (#1113)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 650: 'aa$'m	b\naa\n	y	$-[0]	2
is(eval '"b\naa\n" ~~ rx:perl5/(?m)aa$/ && getpos($/, 0)', 2, 're_tests 910/0 (#1114)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 651: 'aa\Z'm	b\naa	y	$-[0]	2
is(eval '"b\naa" ~~ rx:perl5/(?m)aa\Z/ && getpos($/, 0)', 2, 're_tests 911/0 (#1115)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 652: 'aa\z'm	b\naa	y	$-[0]	2
is(eval '"b\naa" ~~ rx:perl5/(?m)aa\z/ && getpos($/, 0)', 2, 're_tests 912/0 (#1116)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 653: 'aa$'m	b\naa	y	$-[0]	2
is(eval '"b\naa" ~~ rx:perl5/(?m)aa$/ && getpos($/, 0)', 2, 're_tests 913/0 (#1117)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 654: aa\Z	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/aa\Z/)', 're_tests 914  (#1118)', :todo);
ok(eval 'not ("ac\nb\n" ~~ rx/aa\n?$/)', 're_tests 915  (#1119)', :todo);
# 655: aa\z	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/aa\z/)', 're_tests 916  (#1120)', :todo);
ok(eval 'not ("ac\nb\n" ~~ rx/aa$/)', 're_tests 917  (#1121)', :todo);
# 656: aa$	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/aa$/)', 're_tests 918  (#1122)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 657: aa\Z	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/aa\Z/)', 're_tests 919  (#1123)', :todo);
ok(eval 'not ("b\nac\n" ~~ rx/aa\n?$/)', 're_tests 920  (#1124)', :todo);
# 658: aa\z	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/aa\z/)', 're_tests 921  (#1125)', :todo);
ok(eval 'not ("b\nac\n" ~~ rx/aa$/)', 're_tests 922  (#1126)', :todo);
# 659: aa$	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/aa$/)', 're_tests 923  (#1127)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 660: aa\Z	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/aa\Z/)', 're_tests 924  (#1128)', :todo);
ok(eval 'not ("b\nac" ~~ rx/aa\n?$/)', 're_tests 925  (#1129)', :todo);
# 661: aa\z	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/aa\z/)', 're_tests 926  (#1130)', :todo);
ok(eval 'not ("b\nac" ~~ rx/aa$/)', 're_tests 927  (#1131)', :todo);
# 662: aa$	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/aa$/)', 're_tests 928  (#1132)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 663: 'aa\Z'm	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)aa\Z/)', 're_tests 929  (#1133)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 664: 'aa\z'm	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)aa\z/)', 're_tests 930  (#1134)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 665: 'aa$'m	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)aa$/)', 're_tests 931  (#1135)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 666: 'aa\Z'm	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)aa\Z/)', 're_tests 932  (#1136)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 667: 'aa\z'm	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)aa\z/)', 're_tests 933  (#1137)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 668: 'aa$'m	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)aa$/)', 're_tests 934  (#1138)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 669: 'aa\Z'm	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)aa\Z/)', 're_tests 935  (#1139)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 670: 'aa\z'm	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)aa\z/)', 're_tests 936  (#1140)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 671: 'aa$'m	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)aa$/)', 're_tests 937  (#1141)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 672: aa\Z	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/aa\Z/)', 're_tests 938  (#1142)', :todo);
ok(eval 'not ("ca\nb\n" ~~ rx/aa\n?$/)', 're_tests 939  (#1143)', :todo);
# 673: aa\z	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/aa\z/)', 're_tests 940  (#1144)', :todo);
ok(eval 'not ("ca\nb\n" ~~ rx/aa$/)', 're_tests 941  (#1145)', :todo);
# 674: aa$	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/aa$/)', 're_tests 942  (#1146)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 675: aa\Z	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/aa\Z/)', 're_tests 943  (#1147)', :todo);
ok(eval 'not ("b\nca\n" ~~ rx/aa\n?$/)', 're_tests 944  (#1148)', :todo);
# 676: aa\z	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/aa\z/)', 're_tests 945  (#1149)', :todo);
ok(eval 'not ("b\nca\n" ~~ rx/aa$/)', 're_tests 946  (#1150)', :todo);
# 677: aa$	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/aa$/)', 're_tests 947  (#1151)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 678: aa\Z	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/aa\Z/)', 're_tests 948  (#1152)', :todo);
ok(eval 'not ("b\nca" ~~ rx/aa\n?$/)', 're_tests 949  (#1153)', :todo);
# 679: aa\z	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/aa\z/)', 're_tests 950  (#1154)', :todo);
ok(eval 'not ("b\nca" ~~ rx/aa$/)', 're_tests 951  (#1155)', :todo);
# 680: aa$	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/aa$/)', 're_tests 952  (#1156)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 681: 'aa\Z'm	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)aa\Z/)', 're_tests 953  (#1157)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 682: 'aa\z'm	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)aa\z/)', 're_tests 954  (#1158)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 683: 'aa$'m	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)aa$/)', 're_tests 955  (#1159)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 684: 'aa\Z'm	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)aa\Z/)', 're_tests 956  (#1160)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 685: 'aa\z'm	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)aa\z/)', 're_tests 957  (#1161)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 686: 'aa$'m	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)aa$/)', 're_tests 958  (#1162)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 687: 'aa\Z'm	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)aa\Z/)', 're_tests 959  (#1163)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 688: 'aa\z'm	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)aa\z/)', 're_tests 960  (#1164)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 689: 'aa$'m	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)aa$/)', 're_tests 961  (#1165)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 690: ab\Z	ab\nb\n	n	-	-
ok(eval 'not ("ab\nb\n" ~~ rx:perl5/ab\Z/)', 're_tests 962  (#1166)', :todo);
ok(eval 'not ("ab\nb\n" ~~ rx/ab\n?$/)', 're_tests 963  (#1167)', :todo);
# 691: ab\z	ab\nb\n	n	-	-
ok(eval 'not ("ab\nb\n" ~~ rx:perl5/ab\z/)', 're_tests 964  (#1168)', :todo);
ok(eval 'not ("ab\nb\n" ~~ rx/ab$/)', 're_tests 965  (#1169)', :todo);
# 692: ab$	ab\nb\n	n	-	-
ok(eval 'not ("ab\nb\n" ~~ rx:perl5/ab$/)', 're_tests 966  (#1170)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 693: ab\Z	b\nab\n	y	$-[0]	2
is(eval '"b\nab\n" ~~ rx:perl5/ab\Z/ && getpos($/, 0)', 2, 're_tests 967/0 (#1171)', :todo);
is(eval '"b\nab\n" ~~ rx/ab\n?$/ && getpos($/, 0)', 2, 're_tests 968/0 (#1172)', :todo);
# 694: ab\z	b\nab\n	n	-	-
ok(eval 'not ("b\nab\n" ~~ rx:perl5/ab\z/)', 're_tests 969  (#1173)', :todo);
ok(eval 'not ("b\nab\n" ~~ rx/ab$/)', 're_tests 970  (#1174)', :todo);
# 695: ab$	b\nab\n	y	$-[0]	2
is(eval '"b\nab\n" ~~ rx:perl5/ab$/ && getpos($/, 0)', 2, 're_tests 971/0 (#1175)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 696: ab\Z	b\nab	y	$-[0]	2
is(eval '"b\nab" ~~ rx:perl5/ab\Z/ && getpos($/, 0)', 2, 're_tests 972/0 (#1176)', :todo);
is(eval '"b\nab" ~~ rx/ab\n?$/ && getpos($/, 0)', 2, 're_tests 973/0 (#1177)', :todo);
# 697: ab\z	b\nab	y	$-[0]	2
is(eval '"b\nab" ~~ rx:perl5/ab\z/ && getpos($/, 0)', 2, 're_tests 974/0 (#1178)', :todo);
is(eval '"b\nab" ~~ rx/ab$/ && getpos($/, 0)', 2, 're_tests 975/0 (#1179)', :todo);
# 698: ab$	b\nab	y	$-[0]	2
is(eval '"b\nab" ~~ rx:perl5/ab$/ && getpos($/, 0)', 2, 're_tests 976/0 (#1180)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 699: 'ab\Z'm	ab\nb\n	n	-	-
ok(eval 'not ("ab\nb\n" ~~ rx:perl5/(?m)ab\Z/)', 're_tests 977  (#1181)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 700: 'ab\z'm	ab\nb\n	n	-	-
ok(eval 'not ("ab\nb\n" ~~ rx:perl5/(?m)ab\z/)', 're_tests 978  (#1182)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 701: 'ab$'m	ab\nb\n	y	$-[0]	0
is(eval '"ab\nb\n" ~~ rx:perl5/(?m)ab$/ && getpos($/, 0)', 0, 're_tests 979/0 (#1183)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 702: 'ab\Z'm	b\nab\n	y	$-[0]	2
is(eval '"b\nab\n" ~~ rx:perl5/(?m)ab\Z/ && getpos($/, 0)', 2, 're_tests 980/0 (#1184)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 703: 'ab\z'm	b\nab\n	n	-	-
ok(eval 'not ("b\nab\n" ~~ rx:perl5/(?m)ab\z/)', 're_tests 981  (#1185)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 704: 'ab$'m	b\nab\n	y	$-[0]	2
is(eval '"b\nab\n" ~~ rx:perl5/(?m)ab$/ && getpos($/, 0)', 2, 're_tests 982/0 (#1186)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 705: 'ab\Z'm	b\nab	y	$-[0]	2
is(eval '"b\nab" ~~ rx:perl5/(?m)ab\Z/ && getpos($/, 0)', 2, 're_tests 983/0 (#1187)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 706: 'ab\z'm	b\nab	y	$-[0]	2
is(eval '"b\nab" ~~ rx:perl5/(?m)ab\z/ && getpos($/, 0)', 2, 're_tests 984/0 (#1188)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 707: 'ab$'m	b\nab	y	$-[0]	2
is(eval '"b\nab" ~~ rx:perl5/(?m)ab$/ && getpos($/, 0)', 2, 're_tests 985/0 (#1189)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 708: ab\Z	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/ab\Z/)', 're_tests 986  (#1190)', :todo);
ok(eval 'not ("ac\nb\n" ~~ rx/ab\n?$/)', 're_tests 987  (#1191)', :todo);
# 709: ab\z	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/ab\z/)', 're_tests 988  (#1192)', :todo);
ok(eval 'not ("ac\nb\n" ~~ rx/ab$/)', 're_tests 989  (#1193)', :todo);
# 710: ab$	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/ab$/)', 're_tests 990  (#1194)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 711: ab\Z	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/ab\Z/)', 're_tests 991  (#1195)', :todo);
ok(eval 'not ("b\nac\n" ~~ rx/ab\n?$/)', 're_tests 992  (#1196)', :todo);
# 712: ab\z	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/ab\z/)', 're_tests 993  (#1197)', :todo);
ok(eval 'not ("b\nac\n" ~~ rx/ab$/)', 're_tests 994  (#1198)', :todo);
# 713: ab$	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/ab$/)', 're_tests 995  (#1199)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 714: ab\Z	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/ab\Z/)', 're_tests 996  (#1200)', :todo);
ok(eval 'not ("b\nac" ~~ rx/ab\n?$/)', 're_tests 997  (#1201)', :todo);
# 715: ab\z	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/ab\z/)', 're_tests 998  (#1202)', :todo);
ok(eval 'not ("b\nac" ~~ rx/ab$/)', 're_tests 999  (#1203)', :todo);
# 716: ab$	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/ab$/)', 're_tests 1000  (#1204)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 717: 'ab\Z'm	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)ab\Z/)', 're_tests 1001  (#1205)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 718: 'ab\z'm	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)ab\z/)', 're_tests 1002  (#1206)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 719: 'ab$'m	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)ab$/)', 're_tests 1003  (#1207)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 720: 'ab\Z'm	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)ab\Z/)', 're_tests 1004  (#1208)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 721: 'ab\z'm	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)ab\z/)', 're_tests 1005  (#1209)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 722: 'ab$'m	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)ab$/)', 're_tests 1006  (#1210)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 723: 'ab\Z'm	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)ab\Z/)', 're_tests 1007  (#1211)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 724: 'ab\z'm	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)ab\z/)', 're_tests 1008  (#1212)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 725: 'ab$'m	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)ab$/)', 're_tests 1009  (#1213)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 726: ab\Z	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/ab\Z/)', 're_tests 1010  (#1214)', :todo);
ok(eval 'not ("ca\nb\n" ~~ rx/ab\n?$/)', 're_tests 1011  (#1215)', :todo);
# 727: ab\z	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/ab\z/)', 're_tests 1012  (#1216)', :todo);
ok(eval 'not ("ca\nb\n" ~~ rx/ab$/)', 're_tests 1013  (#1217)', :todo);
# 728: ab$	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/ab$/)', 're_tests 1014  (#1218)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 729: ab\Z	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/ab\Z/)', 're_tests 1015  (#1219)', :todo);
ok(eval 'not ("b\nca\n" ~~ rx/ab\n?$/)', 're_tests 1016  (#1220)', :todo);
# 730: ab\z	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/ab\z/)', 're_tests 1017  (#1221)', :todo);
ok(eval 'not ("b\nca\n" ~~ rx/ab$/)', 're_tests 1018  (#1222)', :todo);
# 731: ab$	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/ab$/)', 're_tests 1019  (#1223)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 732: ab\Z	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/ab\Z/)', 're_tests 1020  (#1224)', :todo);
ok(eval 'not ("b\nca" ~~ rx/ab\n?$/)', 're_tests 1021  (#1225)', :todo);
# 733: ab\z	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/ab\z/)', 're_tests 1022  (#1226)', :todo);
ok(eval 'not ("b\nca" ~~ rx/ab$/)', 're_tests 1023  (#1227)', :todo);
# 734: ab$	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/ab$/)', 're_tests 1024  (#1228)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 735: 'ab\Z'm	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)ab\Z/)', 're_tests 1025  (#1229)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 736: 'ab\z'm	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)ab\z/)', 're_tests 1026  (#1230)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 737: 'ab$'m	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)ab$/)', 're_tests 1027  (#1231)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 738: 'ab\Z'm	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)ab\Z/)', 're_tests 1028  (#1232)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 739: 'ab\z'm	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)ab\z/)', 're_tests 1029  (#1233)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 740: 'ab$'m	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)ab$/)', 're_tests 1030  (#1234)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 741: 'ab\Z'm	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)ab\Z/)', 're_tests 1031  (#1235)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 742: 'ab\z'm	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)ab\z/)', 're_tests 1032  (#1236)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 743: 'ab$'m	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)ab$/)', 're_tests 1033  (#1237)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 744: abb\Z	abb\nb\n	n	-	-
ok(eval 'not ("abb\nb\n" ~~ rx:perl5/abb\Z/)', 're_tests 1034  (#1238)', :todo);
ok(eval 'not ("abb\nb\n" ~~ rx/abb\n?$/)', 're_tests 1035  (#1239)', :todo);
# 745: abb\z	abb\nb\n	n	-	-
ok(eval 'not ("abb\nb\n" ~~ rx:perl5/abb\z/)', 're_tests 1036  (#1240)', :todo);
ok(eval 'not ("abb\nb\n" ~~ rx/abb$/)', 're_tests 1037  (#1241)', :todo);
# 746: abb$	abb\nb\n	n	-	-
ok(eval 'not ("abb\nb\n" ~~ rx:perl5/abb$/)', 're_tests 1038  (#1242)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 747: abb\Z	b\nabb\n	y	$-[0]	2
is(eval '"b\nabb\n" ~~ rx:perl5/abb\Z/ && getpos($/, 0)', 2, 're_tests 1039/0 (#1243)', :todo);
is(eval '"b\nabb\n" ~~ rx/abb\n?$/ && getpos($/, 0)', 2, 're_tests 1040/0 (#1244)', :todo);
# 748: abb\z	b\nabb\n	n	-	-
ok(eval 'not ("b\nabb\n" ~~ rx:perl5/abb\z/)', 're_tests 1041  (#1245)', :todo);
ok(eval 'not ("b\nabb\n" ~~ rx/abb$/)', 're_tests 1042  (#1246)', :todo);
# 749: abb$	b\nabb\n	y	$-[0]	2
is(eval '"b\nabb\n" ~~ rx:perl5/abb$/ && getpos($/, 0)', 2, 're_tests 1043/0 (#1247)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 750: abb\Z	b\nabb	y	$-[0]	2
is(eval '"b\nabb" ~~ rx:perl5/abb\Z/ && getpos($/, 0)', 2, 're_tests 1044/0 (#1248)', :todo);
is(eval '"b\nabb" ~~ rx/abb\n?$/ && getpos($/, 0)', 2, 're_tests 1045/0 (#1249)', :todo);
# 751: abb\z	b\nabb	y	$-[0]	2
is(eval '"b\nabb" ~~ rx:perl5/abb\z/ && getpos($/, 0)', 2, 're_tests 1046/0 (#1250)', :todo);
is(eval '"b\nabb" ~~ rx/abb$/ && getpos($/, 0)', 2, 're_tests 1047/0 (#1251)', :todo);
# 752: abb$	b\nabb	y	$-[0]	2
is(eval '"b\nabb" ~~ rx:perl5/abb$/ && getpos($/, 0)', 2, 're_tests 1048/0 (#1252)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 753: 'abb\Z'm	abb\nb\n	n	-	-
ok(eval 'not ("abb\nb\n" ~~ rx:perl5/(?m)abb\Z/)', 're_tests 1049  (#1253)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 754: 'abb\z'm	abb\nb\n	n	-	-
ok(eval 'not ("abb\nb\n" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1050  (#1254)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 755: 'abb$'m	abb\nb\n	y	$-[0]	0
is(eval '"abb\nb\n" ~~ rx:perl5/(?m)abb$/ && getpos($/, 0)', 0, 're_tests 1051/0 (#1255)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 756: 'abb\Z'm	b\nabb\n	y	$-[0]	2
is(eval '"b\nabb\n" ~~ rx:perl5/(?m)abb\Z/ && getpos($/, 0)', 2, 're_tests 1052/0 (#1256)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 757: 'abb\z'm	b\nabb\n	n	-	-
ok(eval 'not ("b\nabb\n" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1053  (#1257)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 758: 'abb$'m	b\nabb\n	y	$-[0]	2
is(eval '"b\nabb\n" ~~ rx:perl5/(?m)abb$/ && getpos($/, 0)', 2, 're_tests 1054/0 (#1258)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 759: 'abb\Z'm	b\nabb	y	$-[0]	2
is(eval '"b\nabb" ~~ rx:perl5/(?m)abb\Z/ && getpos($/, 0)', 2, 're_tests 1055/0 (#1259)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 760: 'abb\z'm	b\nabb	y	$-[0]	2
is(eval '"b\nabb" ~~ rx:perl5/(?m)abb\z/ && getpos($/, 0)', 2, 're_tests 1056/0 (#1260)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 761: 'abb$'m	b\nabb	y	$-[0]	2
is(eval '"b\nabb" ~~ rx:perl5/(?m)abb$/ && getpos($/, 0)', 2, 're_tests 1057/0 (#1261)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 762: abb\Z	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/abb\Z/)', 're_tests 1058  (#1262)', :todo);
ok(eval 'not ("ac\nb\n" ~~ rx/abb\n?$/)', 're_tests 1059  (#1263)', :todo);
# 763: abb\z	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/abb\z/)', 're_tests 1060  (#1264)', :todo);
ok(eval 'not ("ac\nb\n" ~~ rx/abb$/)', 're_tests 1061  (#1265)', :todo);
# 764: abb$	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/abb$/)', 're_tests 1062  (#1266)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 765: abb\Z	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/abb\Z/)', 're_tests 1063  (#1267)', :todo);
ok(eval 'not ("b\nac\n" ~~ rx/abb\n?$/)', 're_tests 1064  (#1268)', :todo);
# 766: abb\z	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/abb\z/)', 're_tests 1065  (#1269)', :todo);
ok(eval 'not ("b\nac\n" ~~ rx/abb$/)', 're_tests 1066  (#1270)', :todo);
# 767: abb$	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/abb$/)', 're_tests 1067  (#1271)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 768: abb\Z	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/abb\Z/)', 're_tests 1068  (#1272)', :todo);
ok(eval 'not ("b\nac" ~~ rx/abb\n?$/)', 're_tests 1069  (#1273)', :todo);
# 769: abb\z	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/abb\z/)', 're_tests 1070  (#1274)', :todo);
ok(eval 'not ("b\nac" ~~ rx/abb$/)', 're_tests 1071  (#1275)', :todo);
# 770: abb$	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/abb$/)', 're_tests 1072  (#1276)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 771: 'abb\Z'm	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)abb\Z/)', 're_tests 1073  (#1277)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 772: 'abb\z'm	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1074  (#1278)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 773: 'abb$'m	ac\nb\n	n	-	-
ok(eval 'not ("ac\nb\n" ~~ rx:perl5/(?m)abb$/)', 're_tests 1075  (#1279)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 774: 'abb\Z'm	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)abb\Z/)', 're_tests 1076  (#1280)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 775: 'abb\z'm	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1077  (#1281)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 776: 'abb$'m	b\nac\n	n	-	-
ok(eval 'not ("b\nac\n" ~~ rx:perl5/(?m)abb$/)', 're_tests 1078  (#1282)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 777: 'abb\Z'm	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)abb\Z/)', 're_tests 1079  (#1283)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 778: 'abb\z'm	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1080  (#1284)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 779: 'abb$'m	b\nac	n	-	-
ok(eval 'not ("b\nac" ~~ rx:perl5/(?m)abb$/)', 're_tests 1081  (#1285)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 780: abb\Z	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/abb\Z/)', 're_tests 1082  (#1286)', :todo);
ok(eval 'not ("ca\nb\n" ~~ rx/abb\n?$/)', 're_tests 1083  (#1287)', :todo);
# 781: abb\z	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/abb\z/)', 're_tests 1084  (#1288)', :todo);
ok(eval 'not ("ca\nb\n" ~~ rx/abb$/)', 're_tests 1085  (#1289)', :todo);
# 782: abb$	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/abb$/)', 're_tests 1086  (#1290)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 783: abb\Z	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/abb\Z/)', 're_tests 1087  (#1291)', :todo);
ok(eval 'not ("b\nca\n" ~~ rx/abb\n?$/)', 're_tests 1088  (#1292)', :todo);
# 784: abb\z	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/abb\z/)', 're_tests 1089  (#1293)', :todo);
ok(eval 'not ("b\nca\n" ~~ rx/abb$/)', 're_tests 1090  (#1294)', :todo);
# 785: abb$	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/abb$/)', 're_tests 1091  (#1295)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 786: abb\Z	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/abb\Z/)', 're_tests 1092  (#1296)', :todo);
ok(eval 'not ("b\nca" ~~ rx/abb\n?$/)', 're_tests 1093  (#1297)', :todo);
# 787: abb\z	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/abb\z/)', 're_tests 1094  (#1298)', :todo);
ok(eval 'not ("b\nca" ~~ rx/abb$/)', 're_tests 1095  (#1299)', :todo);
# 788: abb$	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/abb$/)', 're_tests 1096  (#1300)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 789: 'abb\Z'm	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)abb\Z/)', 're_tests 1097  (#1301)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 790: 'abb\z'm	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1098  (#1302)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 791: 'abb$'m	ca\nb\n	n	-	-
ok(eval 'not ("ca\nb\n" ~~ rx:perl5/(?m)abb$/)', 're_tests 1099  (#1303)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 792: 'abb\Z'm	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)abb\Z/)', 're_tests 1100  (#1304)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 793: 'abb\z'm	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1101  (#1305)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 794: 'abb$'m	b\nca\n	n	-	-
ok(eval 'not ("b\nca\n" ~~ rx:perl5/(?m)abb$/)', 're_tests 1102  (#1306)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 795: 'abb\Z'm	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)abb\Z/)', 're_tests 1103  (#1307)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 796: 'abb\z'm	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)abb\z/)', 're_tests 1104  (#1308)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 797: 'abb$'m	b\nca	n	-	-
ok(eval 'not ("b\nca" ~~ rx:perl5/(?m)abb$/)', 're_tests 1105  (#1309)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 798: (^|x)(c)	ca	y	$2	c
is(eval '"ca" ~~ rx:perl5/(^|x)(c)/ && $2', "c", 're_tests 1106/2 (#1310)', :todo);
is(eval '"ca" ~~ rx/(^|x)(c)/ && $2', "c", 're_tests 1107/2 (#1311)', :todo);
# 799: a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz	x	n	-	-
ok(eval 'not ("x" ~~ rx:perl5/a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz/)', 're_tests 1108  (#1312)', :todo);
ok(eval 'not ("x" ~~ rx/a*abc?xyz+pqr**{3}ab**{2...}xy**{4..5}pq**{0..6}AB**{0...}zz/)', 're_tests 1109  (#1313)', :todo);
# 800: a(?{$a=2;$b=3;($b)=$a})b	yabz	y	$b	2
# SKIPPED: script doesn't understand `$b' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 801: round\(((?>[^()]+))\)	_I(round(xs * sz),1)	y	$1	xs * sz
is(eval '"_I(round(xs * sz),1)" ~~ rx:perl5/round\(((?>[^()]+))\)/ && $1', "xs * sz", 're_tests 1110/1 (#1314)', :todo);
is(eval '"_I(round(xs * sz),1)" ~~ rx/round\(([<[^()]>+]:)\)/ && $1', "xs * sz", 're_tests 1111/1 (#1315)', :todo);
# 802: '((?x:.) )'	x 	y	$1-	x -
# SKIPPED: script doesn't understand `$1-' yet
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 803: '((?-x:.) )'x	x 	y	$1-	x-
# SKIPPED: script doesn't understand `$1-' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 804: foo.bart	foo.bart	y	-	-
ok(eval '"foo.bart" ~~ rx:perl5/foo.bart/', 're_tests 1112  (#1316)', :todo);
ok(eval '"foo.bart" ~~ rx/foo\Nbart/', 're_tests 1113  (#1317)', :todo);
# 805: '^d[x][x][x]'m	abcd\ndxxx	y	-	-
ok(eval '"abcd\ndxxx" ~~ rx:perl5/(?m)^d[x][x][x]/', 're_tests 1114  (#1318)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 806: .X(.+)+X	bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+X/', 're_tests 1115  (#1319)', :todo);
ok(eval '"bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+X/', 're_tests 1116  (#1320)', :todo);
# 807: .X(.+)+XX	bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+XX/', 're_tests 1117  (#1321)', :todo);
ok(eval '"bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+XX/', 're_tests 1118  (#1322)', :todo);
# 808: .XX(.+)+X	bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.XX(.+)+X/', 're_tests 1119  (#1323)', :todo);
ok(eval '"bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NXX(\N+)+X/', 're_tests 1120  (#1324)', :todo);
# 809: .X(.+)+X	bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+X/)', 're_tests 1121  (#1325)', :todo);
ok(eval 'not ("bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+X/)', 're_tests 1122  (#1326)', :todo);
# 810: .X(.+)+XX	bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+XX/)', 're_tests 1123  (#1327)', :todo);
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+XX/)', 're_tests 1124  (#1328)', :todo);
# 811: .XX(.+)+X	bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.XX(.+)+X/)', 're_tests 1125  (#1329)', :todo);
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NXX(\N+)+X/)', 're_tests 1126  (#1330)', :todo);
# 812: .X(.+)+[X]	bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+[X]/', 're_tests 1127  (#1331)', :todo);
ok(eval '"bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+<[X]>/', 're_tests 1128  (#1332)', :todo);
# 813: .X(.+)+[X][X]	bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+[X][X]/', 're_tests 1129  (#1333)', :todo);
ok(eval '"bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+<[X]><[X]>/', 're_tests 1130  (#1334)', :todo);
# 814: .XX(.+)+[X]	bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.XX(.+)+[X]/', 're_tests 1131  (#1335)', :todo);
ok(eval '"bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NXX(\N+)+<[X]>/', 're_tests 1132  (#1336)', :todo);
# 815: .X(.+)+[X]	bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+[X]/)', 're_tests 1133  (#1337)', :todo);
ok(eval 'not ("bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+<[X]>/)', 're_tests 1134  (#1338)', :todo);
# 816: .X(.+)+[X][X]	bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.X(.+)+[X][X]/)', 're_tests 1135  (#1339)', :todo);
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NX(\N+)+<[X]><[X]>/)', 're_tests 1136  (#1340)', :todo);
# 817: .XX(.+)+[X]	bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.XX(.+)+[X]/)', 're_tests 1137  (#1341)', :todo);
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\NXX(\N+)+<[X]>/)', 're_tests 1138  (#1342)', :todo);
# 818: .[X](.+)+[X]	bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.[X](.+)+[X]/', 're_tests 1139  (#1343)', :todo);
ok(eval '"bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\N<[X]>(\N+)+<[X]>/', 're_tests 1140  (#1344)', :todo);
# 819: .[X](.+)+[X][X]	bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.[X](.+)+[X][X]/', 're_tests 1141  (#1345)', :todo);
ok(eval '"bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\N<[X]>(\N+)+<[X]><[X]>/', 're_tests 1142  (#1346)', :todo);
# 820: .[X][X](.+)+[X]	bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	y	-	-
ok(eval '"bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.[X][X](.+)+[X]/', 're_tests 1143  (#1347)', :todo);
ok(eval '"bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\N<[X]><[X]>(\N+)+<[X]>/', 're_tests 1144  (#1348)', :todo);
# 821: .[X](.+)+[X]	bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.[X](.+)+[X]/)', 're_tests 1145  (#1349)', :todo);
ok(eval 'not ("bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\N<[X]>(\N+)+<[X]>/)', 're_tests 1146  (#1350)', :todo);
# 822: .[X](.+)+[X][X]	bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.[X](.+)+[X][X]/)', 're_tests 1147  (#1351)', :todo);
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\N<[X]>(\N+)+<[X]><[X]>/)', 're_tests 1148  (#1352)', :todo);
# 823: .[X][X](.+)+[X]	bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa	n	-	-
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx:perl5/.[X][X](.+)+[X]/)', 're_tests 1149  (#1353)', :todo);
ok(eval 'not ("bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ rx/\N<[X]><[X]>(\N+)+<[X]>/)', 're_tests 1150  (#1354)', :todo);
# 824: tt+$	xxxtt	y	-	-
ok(eval '"xxxtt" ~~ rx:perl5/tt+$/', 're_tests 1151  (#1355)', :todo);
ok(eval '"xxxtt" ~~ rx/tt+$/', 're_tests 1152  (#1356)', :todo);
# 825: ([a-\d]+)	za-9z	y	$1	a-9
is(eval '"za-9z" ~~ rx:perl5/([a-\d]+)/ && $1', "a-9", 're_tests 1153/1 (#1357)', :todo);
is(eval '"za-9z" ~~ rx/(<[a-\d]>+)/ && $1', "a-9", 're_tests 1154/1 (#1358)', :todo);
# 826: ([\d-z]+)	a0-za	y	$1	0-z
is(eval '"a0-za" ~~ rx:perl5/([\d-z]+)/ && $1', "0-z", 're_tests 1155/1 (#1359)', :todo);
is(eval '"a0-za" ~~ rx/(<[\d-z]>+)/ && $1', "0-z", 're_tests 1156/1 (#1360)', :todo);
# 827: ([\d-\s]+)	a0- z	y	$1	0- 
is(eval '"a0- z" ~~ rx:perl5/([\d-\s]+)/ && $1', "0- ", 're_tests 1157/1 (#1361)', :todo);
is(eval '"a0- z" ~~ rx/(<[\d-\s]>+)/ && $1', "0- ", 're_tests 1158/1 (#1362)', :todo);
# 828: ([a-[:digit:]]+)	za-9z	y	$1	a-9
is(eval '"za-9z" ~~ rx:perl5/([a-[:digit:]]+)/ && $1', "a-9", 're_tests 1159/1 (#1363)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 829: ([[:digit:]-z]+)	=0-z=	y	$1	0-z
is(eval '"=0-z=" ~~ rx:perl5/([[:digit:]-z]+)/ && $1', "0-z", 're_tests 1160/1 (#1364)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 830: ([[:digit:]-[:alpha:]]+)	=0-z=	y	$1	0-z
is(eval '"=0-z=" ~~ rx:perl5/([[:digit:]-[:alpha:]]+)/ && $1', "0-z", 're_tests 1161/1 (#1365)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 831: \GX.*X	aaaXbX	n	-	-
ok(eval 'not ("aaaXbX" ~~ rx:perl5/\GX.*X/)', 're_tests 1162  (#1366)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 832: (\d+\.\d+)	3.1415926	y	$1	3.1415926
is(eval '"3.1415926" ~~ rx:perl5/(\d+\.\d+)/ && $1', "3.1415926", 're_tests 1163/1 (#1367)', :todo);
is(eval '"3.1415926" ~~ rx/(\d+\.\d+)/ && $1', "3.1415926", 're_tests 1164/1 (#1368)', :todo);
# 833: (\ba.{0,10}br)	have a web browser	y	$1	a web br
is(eval '"have a web browser" ~~ rx:perl5/(\ba.{0,10}br)/ && $1', "a web br", 're_tests 1165/1 (#1369)', :todo);
is(eval '"have a web browser" ~~ rx/(\ba\N**{0..10}br)/ && $1', "a web br", 're_tests 1166/1 (#1370)', :todo);
# 834: '\.c(pp|xx|c)?$'i	Changes	n	-	-
ok(eval 'not ("Changes" ~~ rx:perl5/(?i)\.c(pp|xx|c)?$/)', 're_tests 1167  (#1371)', :todo);
ok(eval 'not ("Changes" ~~ rx:i/\.c(pp|xx|c)?$/)', 're_tests 1168  (#1372)', :todo);
# 835: '\.c(pp|xx|c)?$'i	IO.c	y	-	-
ok(eval '"IO.c" ~~ rx:perl5/(?i)\.c(pp|xx|c)?$/', 're_tests 1169  (#1373)', :todo);
ok(eval '"IO.c" ~~ rx:i/\.c(pp|xx|c)?$/', 're_tests 1170  (#1374)', :todo);
# 836: '(\.c(pp|xx|c)?$)'i	IO.c	y	$1	.c
is(eval '"IO.c" ~~ rx:perl5/(?i)(\.c(pp|xx|c)?$)/ && $1', ".c", 're_tests 1171/1 (#1375)', :todo);
is(eval '"IO.c" ~~ rx:i/(\.c(pp|xx|c)?$)/ && $1', ".c", 're_tests 1172/1 (#1376)', :todo);
# 837: ^([a-z]:)	C:/	n	-	-
ok(eval 'not ("C:/" ~~ rx:perl5/^([a-z]:)/)', 're_tests 1173  (#1377)', :todo);
ok(eval 'not ("C:/" ~~ rx/^(<[a-z]>\:)/)', 're_tests 1174  (#1378)', :todo);
# 838: '^\S\s+aa$'m	\nx aa	y	-	-
ok(eval '"\nx aa" ~~ rx:perl5/(?m)^\S\s+aa$/', 're_tests 1175  (#1379)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 839: (^|a)b	ab	y	-	-
ok(eval '"ab" ~~ rx:perl5/(^|a)b/', 're_tests 1176  (#1380)', :todo);
ok(eval '"ab" ~~ rx/(^|a)b/', 're_tests 1177  (#1381)', :todo);
# 840: ^([ab]*?)(b)?(c)$	abac	y	-$2-	--
# SKIPPED: script doesn't understand `-$2-' yet
# SKIPPED: script doesn't understand `-$2-' yet
# 841: (\w)?(abc)\1b	abcab	n	-	-
ok(eval 'not ("abcab" ~~ rx:perl5/(\w)?(abc)\1b/)', 're_tests 1178  (#1382)', :todo);
ok(eval 'not ("abcab" ~~ rx/(\w)?(abc)$1b/)', 're_tests 1179  (#1383)', :todo);
# 842: ^(?:.,){2}c	a,b,c	y	-	-
ok(eval '"a,b,c" ~~ rx:perl5/^(?:.,){2}c/', 're_tests 1180  (#1384)', :todo);
ok(eval '"a,b,c" ~~ rx/^[\N,]**{2}c/', 're_tests 1181  (#1385)', :todo);
# 843: ^(.,){2}c	a,b,c	y	$1	b,
is(eval '"a,b,c" ~~ rx:perl5/^(.,){2}c/ && $1', "b,", 're_tests 1182/1 (#1386)', :todo);
is(eval '"a,b,c" ~~ rx/^(\N,)**{2}c/ && $1', "b,", 're_tests 1183/1 (#1387)', :todo);
# 844: ^(?:[^,]*,){2}c	a,b,c	y	-	-
ok(eval '"a,b,c" ~~ rx:perl5/^(?:[^,]*,){2}c/', 're_tests 1184  (#1388)', :todo);
ok(eval '"a,b,c" ~~ rx/^[<[^,]>*,]**{2}c/', 're_tests 1185  (#1389)', :todo);
# 845: ^([^,]*,){2}c	a,b,c	y	$1	b,
is(eval '"a,b,c" ~~ rx:perl5/^([^,]*,){2}c/ && $1', "b,", 're_tests 1186/1 (#1390)', :todo);
is(eval '"a,b,c" ~~ rx/^(<[^,]>*,)**{2}c/ && $1', "b,", 're_tests 1187/1 (#1391)', :todo);
# 846: ^([^,]*,){3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]*,){3}d/ && $1', "c,", 're_tests 1188/1 (#1392)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>*,)**{3}d/ && $1', "c,", 're_tests 1189/1 (#1393)', :todo);
# 847: ^([^,]*,){3,}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]*,){3,}d/ && $1', "c,", 're_tests 1190/1 (#1394)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>*,)**{3...}d/ && $1', "c,", 're_tests 1191/1 (#1395)', :todo);
# 848: ^([^,]*,){0,3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]*,){0,3}d/ && $1', "c,", 're_tests 1192/1 (#1396)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>*,)**{0..3}d/ && $1', "c,", 're_tests 1193/1 (#1397)', :todo);
# 849: ^([^,]{1,3},){3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{1,3},){3}d/ && $1', "c,", 're_tests 1194/1 (#1398)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{1..3},)**{3}d/ && $1', "c,", 're_tests 1195/1 (#1399)', :todo);
# 850: ^([^,]{1,3},){3,}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{1,3},){3,}d/ && $1', "c,", 're_tests 1196/1 (#1400)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{1..3},)**{3...}d/ && $1', "c,", 're_tests 1197/1 (#1401)', :todo);
# 851: ^([^,]{1,3},){0,3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{1,3},){0,3}d/ && $1', "c,", 're_tests 1198/1 (#1402)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{1..3},)**{0..3}d/ && $1', "c,", 're_tests 1199/1 (#1403)', :todo);
# 852: ^([^,]{1,},){3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{1,},){3}d/ && $1', "c,", 're_tests 1200/1 (#1404)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{1...},)**{3}d/ && $1', "c,", 're_tests 1201/1 (#1405)', :todo);
# 853: ^([^,]{1,},){3,}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{1,},){3,}d/ && $1', "c,", 're_tests 1202/1 (#1406)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{1...},)**{3...}d/ && $1', "c,", 're_tests 1203/1 (#1407)', :todo);
# 854: ^([^,]{1,},){0,3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{1,},){0,3}d/ && $1', "c,", 're_tests 1204/1 (#1408)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{1...},)**{0..3}d/ && $1', "c,", 're_tests 1205/1 (#1409)', :todo);
# 855: ^([^,]{0,3},){3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{0,3},){3}d/ && $1', "c,", 're_tests 1206/1 (#1410)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{0..3},)**{3}d/ && $1', "c,", 're_tests 1207/1 (#1411)', :todo);
# 856: ^([^,]{0,3},){3,}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{0,3},){3,}d/ && $1', "c,", 're_tests 1208/1 (#1412)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{0..3},)**{3...}d/ && $1', "c,", 're_tests 1209/1 (#1413)', :todo);
# 857: ^([^,]{0,3},){0,3}d	aaa,b,c,d	y	$1	c,
is(eval '"aaa,b,c,d" ~~ rx:perl5/^([^,]{0,3},){0,3}d/ && $1', "c,", 're_tests 1210/1 (#1414)', :todo);
is(eval '"aaa,b,c,d" ~~ rx/^(<[^,]>**{0..3},)**{0..3}d/ && $1', "c,", 're_tests 1211/1 (#1415)', :todo);
# 858: (?i)		y	-	-
ok(eval '"" ~~ rx:perl5/(?i)/', 're_tests 1212  (#1416)', :todo);
ok(eval '"" ~~ rx/:i /', 're_tests 1213  (#1417)', :todo);
# 859: '(?!\A)x'm	a\nxb\n	y	-	-
ok(eval '"a\nxb\n" ~~ rx:perl5/(?m)(?!\A)x/', 're_tests 1214  (#1418)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 860: ^(a(b)?)+$	aba	y	-$1-$2-	-a--
# SKIPPED: script doesn't understand `-$1-$2-' yet
# SKIPPED: script doesn't understand `-$1-$2-' yet
# 861: ^(aa(bb)?)+$	aabbaa	y	-$1-$2-	-aa--
# SKIPPED: script doesn't understand `-$1-$2-' yet
# SKIPPED: script doesn't understand `-$1-$2-' yet
# 862: '^.{9}abc.*\n'm	123\nabcabcabcabc\n	y	-	-
ok(eval '"123\nabcabcabcabc\n" ~~ rx:perl5/(?m)^.{9}abc.*\n/', 're_tests 1215  (#1419)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 863: ^(a)?a$	a	y	-$1-	--
# SKIPPED: script doesn't understand `-$1-' yet
# SKIPPED: script doesn't understand `-$1-' yet
# 864: ^(a)?(?(1)a|b)+$	a	n	-	-
ok(eval 'not ("a" ~~ rx:perl5/^(a)?(?(1)a|b)+$/)', 're_tests 1216  (#1420)', :todo);
ok(eval 'not ("a" ~~ rx/^(a)?[ <(defined $1)> :: a|b ]+$/)', 're_tests 1217  (#1421)', :todo);
# 865: ^(a\1?)(a\1?)(a\2?)(a\3?)$	aaaaaa	y	$1,$2,$3,$4	a,aa,a,aa
# SKIPPED: script doesn't understand `$1,$2,$3,$4' yet
# SKIPPED: script doesn't understand `$1,$2,$3,$4' yet
# 866: ^(a\1?){4}$	aaaaaa	y	$1	aa
is(eval '"aaaaaa" ~~ rx:perl5/^(a\1?){4}$/ && $1', "aa", 're_tests 1218/1 (#1422)', :todo);
is(eval '"aaaaaa" ~~ rx/^(a$1?)**{4}$/ && $1', "aa", 're_tests 1219/1 (#1423)', :todo);
# 867: ^(0+)?(?:x(1))?	x1	y	-	-
ok(eval '"x1" ~~ rx:perl5/^(0+)?(?:x(1))?/', 're_tests 1220  (#1424)', :todo);
ok(eval '"x1" ~~ rx/^(0+)?[x(1)]?/', 're_tests 1221  (#1425)', :todo);
# 868: ^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?	012cxx0190	y	-	-
ok(eval '"012cxx0190" ~~ rx:perl5/^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?/', 're_tests 1222  (#1426)', :todo);
ok(eval '"012cxx0190" ~~ rx/^(<[0-9a-fA-F]>+)[x(<[0-9a-fA-F]>+)?][x(<[0-9a-fA-F]>+)]?/', 're_tests 1223  (#1427)', :todo);
# 869: ^(b+?|a){1,2}c	bbbac	y	$1	a
is(eval '"bbbac" ~~ rx:perl5/^(b+?|a){1,2}c/ && $1', "a", 're_tests 1224/1 (#1428)', :todo);
is(eval '"bbbac" ~~ rx/^(b+?|a)**{1..2}c/ && $1', "a", 're_tests 1225/1 (#1429)', :todo);
# 870: ^(b+?|a){1,2}c	bbbbac	y	$1	a
is(eval '"bbbbac" ~~ rx:perl5/^(b+?|a){1,2}c/ && $1', "a", 're_tests 1226/1 (#1430)', :todo);
is(eval '"bbbbac" ~~ rx/^(b+?|a)**{1..2}c/ && $1', "a", 're_tests 1227/1 (#1431)', :todo);
# 871: \((\w\. \w+)\)	cd. (A. Tw)	y	-$1-	-A. Tw-
# SKIPPED: script doesn't understand `-$1-' yet
# SKIPPED: script doesn't understand `-$1-' yet
# 872: ((?:aaaa|bbbb)cccc)?	aaaacccc	y	-	-
ok(eval '"aaaacccc" ~~ rx:perl5/((?:aaaa|bbbb)cccc)?/', 're_tests 1228  (#1432)', :todo);
ok(eval '"aaaacccc" ~~ rx/([aaaa|bbbb]cccc)?/', 're_tests 1229  (#1433)', :todo);
# 873: ((?:aaaa|bbbb)cccc)?	bbbbcccc	y	-	-
ok(eval '"bbbbcccc" ~~ rx:perl5/((?:aaaa|bbbb)cccc)?/', 're_tests 1230  (#1434)', :todo);
ok(eval '"bbbbcccc" ~~ rx/([aaaa|bbbb]cccc)?/', 're_tests 1231  (#1435)', :todo);
# 874: (a)?(a)+	a	y	$1:$2	:a	-
is(eval '"a" ~~ rx:perl5/(a)?(a)+/ && $1', "", 're_tests 1232/1 (#1436)', :todo);
is(eval '"a" ~~ rx:perl5/(a)?(a)+/ && $2', "a", 're_tests 1232/2 (#1437)', :todo);
is(eval '"a" ~~ rx/(a)?(a)+/ && $1', "", 're_tests 1233/1 (#1438)', :todo);
is(eval '"a" ~~ rx/(a)?(a)+/ && $2', "a", 're_tests 1233/2 (#1439)', :todo);
# 875: (ab)?(ab)+	ab	y	$1:$2	:ab	-
is(eval '"ab" ~~ rx:perl5/(ab)?(ab)+/ && $1', "", 're_tests 1234/1 (#1440)', :todo);
is(eval '"ab" ~~ rx:perl5/(ab)?(ab)+/ && $2', "ab", 're_tests 1234/2 (#1441)', :todo);
is(eval '"ab" ~~ rx/(ab)?(ab)+/ && $1', "", 're_tests 1235/1 (#1442)', :todo);
is(eval '"ab" ~~ rx/(ab)?(ab)+/ && $2', "ab", 're_tests 1235/2 (#1443)', :todo);
# 876: (abc)?(abc)+	abc	y	$1:$2	:abc	-
is(eval '"abc" ~~ rx:perl5/(abc)?(abc)+/ && $1', "", 're_tests 1236/1 (#1444)', :todo);
is(eval '"abc" ~~ rx:perl5/(abc)?(abc)+/ && $2', "abc", 're_tests 1236/2 (#1445)', :todo);
is(eval '"abc" ~~ rx/(abc)?(abc)+/ && $1', "", 're_tests 1237/1 (#1446)', :todo);
is(eval '"abc" ~~ rx/(abc)?(abc)+/ && $2', "abc", 're_tests 1237/2 (#1447)', :todo);
# 877: 'b\s^'m	a\nb\n	n	-	-
ok(eval 'not ("a\nb\n" ~~ rx:perl5/(?m)b\s^/)', 're_tests 1238  (#1448)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 878: \ba	a	y	-	-
ok(eval '"a" ~~ rx:perl5/\ba/', 're_tests 1239  (#1449)', :todo);
ok(eval '"a" ~~ rx/\ba/', 're_tests 1240  (#1450)', :todo);
# 879: ^(a(??{"(?!)"})|(a)(?{1}))b	ab	y	$2	a	# [ID 20010811.006]
is(eval '"ab" ~~ rx:perl5/^(a(??{"(?!)"})|(a)(?{1}))b/ && $2', "a", 're_tests 1241/2 (#1451)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(??{...' yet
# 880: ab(?i)cd	AbCd	n	-	-	# [ID 20010809.023]
ok(eval 'not ("AbCd" ~~ rx:perl5/ab(?i)cd/)', 're_tests 1242  (#1452)', :todo);
ok(eval 'not ("AbCd" ~~ rx/ab:i cd/)', 're_tests 1243  (#1453)', :todo);
# 881: ab(?i)cd	abCd	y	-	-
ok(eval '"abCd" ~~ rx:perl5/ab(?i)cd/', 're_tests 1244  (#1454)', :todo);
ok(eval '"abCd" ~~ rx/ab:i cd/', 're_tests 1245  (#1455)', :todo);
# 882: (A|B)*(?(1)(CD)|(CD))	CD	y	$2-$3	-CD
is(eval '"CD" ~~ rx:perl5/(A|B)*(?(1)(CD)|(CD))/ && $2', "", 're_tests 1246/2 (#1456)', :todo);
is(eval '"CD" ~~ rx:perl5/(A|B)*(?(1)(CD)|(CD))/ && $3', "CD", 're_tests 1246/3 (#1457)', :todo);
is(eval '"CD" ~~ rx/(A|B)*[ <(defined $1)> :: (CD)|(CD) ]/ && $2', "", 're_tests 1247/2 (#1458)', :todo);
is(eval '"CD" ~~ rx/(A|B)*[ <(defined $1)> :: (CD)|(CD) ]/ && $3', "CD", 're_tests 1247/3 (#1459)', :todo);
# 883: (A|B)*(?(1)(CD)|(CD))	ABCD	y	$2-$3	CD-
is(eval '"ABCD" ~~ rx:perl5/(A|B)*(?(1)(CD)|(CD))/ && $2', "CD", 're_tests 1248/2 (#1460)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(A|B)*(?(1)(CD)|(CD))/ && $3', "", 're_tests 1248/3 (#1461)', :todo);
is(eval '"ABCD" ~~ rx/(A|B)*[ <(defined $1)> :: (CD)|(CD) ]/ && $2', "CD", 're_tests 1249/2 (#1462)', :todo);
is(eval '"ABCD" ~~ rx/(A|B)*[ <(defined $1)> :: (CD)|(CD) ]/ && $3', "", 're_tests 1249/3 (#1463)', :todo);
# 884: (A|B)*?(?(1)(CD)|(CD))	CD	y	$2-$3	-CD	# [ID 20010803.016]
is(eval '"CD" ~~ rx:perl5/(A|B)*?(?(1)(CD)|(CD))/ && $2', "", 're_tests 1250/2 (#1464)', :todo);
is(eval '"CD" ~~ rx:perl5/(A|B)*?(?(1)(CD)|(CD))/ && $3', "CD", 're_tests 1250/3 (#1465)', :todo);
is(eval '"CD" ~~ rx/(A|B)*?[ <(defined $1)> :: (CD)|(CD) ]/ && $2', "", 're_tests 1251/2 (#1466)', :todo);
is(eval '"CD" ~~ rx/(A|B)*?[ <(defined $1)> :: (CD)|(CD) ]/ && $3', "CD", 're_tests 1251/3 (#1467)', :todo);
# 885: (A|B)*?(?(1)(CD)|(CD))	ABCD	y	$2-$3	CD-
is(eval '"ABCD" ~~ rx:perl5/(A|B)*?(?(1)(CD)|(CD))/ && $2', "CD", 're_tests 1252/2 (#1468)', :todo);
is(eval '"ABCD" ~~ rx:perl5/(A|B)*?(?(1)(CD)|(CD))/ && $3', "", 're_tests 1252/3 (#1469)', :todo);
is(eval '"ABCD" ~~ rx/(A|B)*?[ <(defined $1)> :: (CD)|(CD) ]/ && $2', "CD", 're_tests 1253/2 (#1470)', :todo);
is(eval '"ABCD" ~~ rx/(A|B)*?[ <(defined $1)> :: (CD)|(CD) ]/ && $3', "", 're_tests 1253/3 (#1471)', :todo);
# 886: '^(o)(?!.*\1)'i	Oo	n	-	-
ok(eval 'not ("Oo" ~~ rx:perl5/(?i)^(o)(?!.*\1)/)', 're_tests 1254  (#1472)', :todo);
ok(eval 'not ("Oo" ~~ rx:i/^(o)<!before \N*$1>/)', 're_tests 1255  (#1473)', :todo);
# 887: (.*)\d+\1	abc12bc	y	$1	bc
is(eval '"abc12bc" ~~ rx:perl5/(.*)\d+\1/ && $1', "bc", 're_tests 1256/1 (#1474)', :todo);
is(eval '"abc12bc" ~~ rx/(\N*)\d+$1/ && $1', "bc", 're_tests 1257/1 (#1475)', :todo);
# 888: (?m:(foo\s*$))	foo\n bar	y	$1	foo
is(eval '"foo\n bar" ~~ rx:perl5/(?m:(foo\s*$))/ && $1', "foo", 're_tests 1258/1 (#1476)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 889: (.*)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)c/ && $1', "ab", 're_tests 1259/1 (#1477)', :todo);
is(eval '"abcd" ~~ rx/(\N*)c/ && $1', "ab", 're_tests 1260/1 (#1478)', :todo);
# 890: (.*)(?=c)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=c)/ && $1', "ab", 're_tests 1261/1 (#1479)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before c>/ && $1', "ab", 're_tests 1262/1 (#1480)', :todo);
# 891: (.*)(?=c)c	abcd	yB	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=c)c/ && $1', "ab", 're_tests 1263/1 (#1481)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before c>c/ && $1', "ab", 're_tests 1264/1 (#1482)', :todo);
# 892: (.*)(?=b|c)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=b|c)/ && $1', "ab", 're_tests 1265/1 (#1483)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before b|c>/ && $1', "ab", 're_tests 1266/1 (#1484)', :todo);
# 893: (.*)(?=b|c)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=b|c)c/ && $1', "ab", 're_tests 1267/1 (#1485)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before b|c>c/ && $1', "ab", 're_tests 1268/1 (#1486)', :todo);
# 894: (.*)(?=c|b)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=c|b)/ && $1', "ab", 're_tests 1269/1 (#1487)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before c|b>/ && $1', "ab", 're_tests 1270/1 (#1488)', :todo);
# 895: (.*)(?=c|b)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=c|b)c/ && $1', "ab", 're_tests 1271/1 (#1489)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before c|b>c/ && $1', "ab", 're_tests 1272/1 (#1490)', :todo);
# 896: (.*)(?=[bc])	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=[bc])/ && $1', "ab", 're_tests 1273/1 (#1491)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before <[bc]>>/ && $1', "ab", 're_tests 1274/1 (#1492)', :todo);
# 897: (.*)(?=[bc])c	abcd	yB	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?=[bc])c/ && $1', "ab", 're_tests 1275/1 (#1493)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<before <[bc]>>c/ && $1', "ab", 're_tests 1276/1 (#1494)', :todo);
# 898: (.*)(?<=b)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=b)/ && $1', "ab", 're_tests 1277/1 (#1495)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after b>/ && $1', "ab", 're_tests 1278/1 (#1496)', :todo);
# 899: (.*)(?<=b)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=b)c/ && $1', "ab", 're_tests 1279/1 (#1497)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after b>c/ && $1', "ab", 're_tests 1280/1 (#1498)', :todo);
# 900: (.*)(?<=b|c)	abcd	y	$1	abc
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=b|c)/ && $1', "abc", 're_tests 1281/1 (#1499)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after b|c>/ && $1', "abc", 're_tests 1282/1 (#1500)', :todo);
# 901: (.*)(?<=b|c)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=b|c)c/ && $1', "ab", 're_tests 1283/1 (#1501)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after b|c>c/ && $1', "ab", 're_tests 1284/1 (#1502)', :todo);
# 902: (.*)(?<=c|b)	abcd	y	$1	abc
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=c|b)/ && $1', "abc", 're_tests 1285/1 (#1503)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after c|b>/ && $1', "abc", 're_tests 1286/1 (#1504)', :todo);
# 903: (.*)(?<=c|b)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=c|b)c/ && $1', "ab", 're_tests 1287/1 (#1505)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after c|b>c/ && $1', "ab", 're_tests 1288/1 (#1506)', :todo);
# 904: (.*)(?<=[bc])	abcd	y	$1	abc
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=[bc])/ && $1', "abc", 're_tests 1289/1 (#1507)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after <[bc]>>/ && $1', "abc", 're_tests 1290/1 (#1508)', :todo);
# 905: (.*)(?<=[bc])c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*)(?<=[bc])c/ && $1', "ab", 're_tests 1291/1 (#1509)', :todo);
is(eval '"abcd" ~~ rx/(\N*)<after <[bc]>>c/ && $1', "ab", 're_tests 1292/1 (#1510)', :todo);
# 906: (.*?)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)c/ && $1', "ab", 're_tests 1293/1 (#1511)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)c/ && $1', "ab", 're_tests 1294/1 (#1512)', :todo);
# 907: (.*?)(?=c)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=c)/ && $1', "ab", 're_tests 1295/1 (#1513)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before c>/ && $1', "ab", 're_tests 1296/1 (#1514)', :todo);
# 908: (.*?)(?=c)c	abcd	yB	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=c)c/ && $1', "ab", 're_tests 1297/1 (#1515)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before c>c/ && $1', "ab", 're_tests 1298/1 (#1516)', :todo);
# 909: (.*?)(?=b|c)	abcd	y	$1	a
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=b|c)/ && $1', "a", 're_tests 1299/1 (#1517)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before b|c>/ && $1', "a", 're_tests 1300/1 (#1518)', :todo);
# 910: (.*?)(?=b|c)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=b|c)c/ && $1', "ab", 're_tests 1301/1 (#1519)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before b|c>c/ && $1', "ab", 're_tests 1302/1 (#1520)', :todo);
# 911: (.*?)(?=c|b)	abcd	y	$1	a
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=c|b)/ && $1', "a", 're_tests 1303/1 (#1521)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before c|b>/ && $1', "a", 're_tests 1304/1 (#1522)', :todo);
# 912: (.*?)(?=c|b)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=c|b)c/ && $1', "ab", 're_tests 1305/1 (#1523)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before c|b>c/ && $1', "ab", 're_tests 1306/1 (#1524)', :todo);
# 913: (.*?)(?=[bc])	abcd	y	$1	a
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=[bc])/ && $1', "a", 're_tests 1307/1 (#1525)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before <[bc]>>/ && $1', "a", 're_tests 1308/1 (#1526)', :todo);
# 914: (.*?)(?=[bc])c	abcd	yB	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?=[bc])c/ && $1', "ab", 're_tests 1309/1 (#1527)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<before <[bc]>>c/ && $1', "ab", 're_tests 1310/1 (#1528)', :todo);
# 915: (.*?)(?<=b)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=b)/ && $1', "ab", 're_tests 1311/1 (#1529)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after b>/ && $1', "ab", 're_tests 1312/1 (#1530)', :todo);
# 916: (.*?)(?<=b)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=b)c/ && $1', "ab", 're_tests 1313/1 (#1531)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after b>c/ && $1', "ab", 're_tests 1314/1 (#1532)', :todo);
# 917: (.*?)(?<=b|c)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=b|c)/ && $1', "ab", 're_tests 1315/1 (#1533)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after b|c>/ && $1', "ab", 're_tests 1316/1 (#1534)', :todo);
# 918: (.*?)(?<=b|c)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=b|c)c/ && $1', "ab", 're_tests 1317/1 (#1535)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after b|c>c/ && $1', "ab", 're_tests 1318/1 (#1536)', :todo);
# 919: (.*?)(?<=c|b)	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=c|b)/ && $1', "ab", 're_tests 1319/1 (#1537)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after c|b>/ && $1', "ab", 're_tests 1320/1 (#1538)', :todo);
# 920: (.*?)(?<=c|b)c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=c|b)c/ && $1', "ab", 're_tests 1321/1 (#1539)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after c|b>c/ && $1', "ab", 're_tests 1322/1 (#1540)', :todo);
# 921: (.*?)(?<=[bc])	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=[bc])/ && $1', "ab", 're_tests 1323/1 (#1541)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after <[bc]>>/ && $1', "ab", 're_tests 1324/1 (#1542)', :todo);
# 922: (.*?)(?<=[bc])c	abcd	y	$1	ab
is(eval '"abcd" ~~ rx:perl5/(.*?)(?<=[bc])c/ && $1', "ab", 're_tests 1325/1 (#1543)', :todo);
is(eval '"abcd" ~~ rx/(\N*?)<after <[bc]>>c/ && $1', "ab", 're_tests 1326/1 (#1544)', :todo);
# 923: 2(]*)?$\1	2	y	$&	2
is(eval '"2" ~~ rx:perl5/2(]*)?$\1/ && $0', "2", 're_tests 1327/0 (#1545)', :todo);
is(eval '"2" ~~ rx/2(]*)?$$1/ && $0', "2", 're_tests 1328/0 (#1546)', :todo);
# 924: (??{})	x	y	-	-
ok(eval '"x" ~~ rx:perl5/(??{})/', 're_tests 1329  (#1547)', :todo);
# -- SKIPPED - p5re_to_p6rule doesn't support `(??{...' yet
# 925: a(b)??	abc	y	<$1>	<>	# undef [perl #16773]
# SKIPPED: script doesn't understand `<$1>' yet
# SKIPPED: script doesn't understand `<$1>' yet
# 926: (\d{1,3}\.){3,}	128.134.142.8	y	<$1>	<142.>	# [perl #18019]
# SKIPPED: script doesn't understand `<$1>' yet
# SKIPPED: script doesn't understand `<$1>' yet
# 927: ^.{3,4}(.+)\1\z	foobarbar	y	$1	bar	# 16 tests for [perl #23171]
is(eval '"foobarbar" ~~ rx:perl5/^.{3,4}(.+)\1\z/ && $1', "bar", 're_tests 1330/1 (#1548)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{3..4}(\N+)$1$/ && $1', "bar", 're_tests 1331/1 (#1549)', :todo);
# 928: ^(?:f|o|b){3,4}(.+)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){3,4}(.+)\1\z/ && $1', "bar", 're_tests 1332/1 (#1550)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{3..4}(\N+)$1$/ && $1', "bar", 're_tests 1333/1 (#1551)', :todo);
# 929: ^.{3,4}((?:b|a|r)+)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^.{3,4}((?:b|a|r)+)\1\z/ && $1', "bar", 're_tests 1334/1 (#1552)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{3..4}([b|a|r]+)$1$/ && $1', "bar", 're_tests 1335/1 (#1553)', :todo);
# 930: ^(?:f|o|b){3,4}((?:b|a|r)+)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){3,4}((?:b|a|r)+)\1\z/ && $1', "bar", 're_tests 1336/1 (#1554)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{3..4}([b|a|r]+)$1$/ && $1', "bar", 're_tests 1337/1 (#1555)', :todo);
# 931: ^.{3,4}(.+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^.{3,4}(.+?)\1\z/ && $1', "bar", 're_tests 1338/1 (#1556)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{3..4}(\N+?)$1$/ && $1', "bar", 're_tests 1339/1 (#1557)', :todo);
# 932: ^(?:f|o|b){3,4}(.+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){3,4}(.+?)\1\z/ && $1', "bar", 're_tests 1340/1 (#1558)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{3..4}(\N+?)$1$/ && $1', "bar", 're_tests 1341/1 (#1559)', :todo);
# 933: ^.{3,4}((?:b|a|r)+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^.{3,4}((?:b|a|r)+?)\1\z/ && $1', "bar", 're_tests 1342/1 (#1560)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{3..4}([b|a|r]+?)$1$/ && $1', "bar", 're_tests 1343/1 (#1561)', :todo);
# 934: ^(?:f|o|b){3,4}((?:b|a|r)+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){3,4}((?:b|a|r)+?)\1\z/ && $1', "bar", 're_tests 1344/1 (#1562)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{3..4}([b|a|r]+?)$1$/ && $1', "bar", 're_tests 1345/1 (#1563)', :todo);
# 935: ^.{2,3}?(.+)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^.{2,3}?(.+)\1\z/ && $1', "bar", 're_tests 1346/1 (#1564)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{2..3}?(\N+)$1$/ && $1', "bar", 're_tests 1347/1 (#1565)', :todo);
# 936: ^(?:f|o|b){2,3}?(.+)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){2,3}?(.+)\1\z/ && $1', "bar", 're_tests 1348/1 (#1566)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{2..3}?(\N+)$1$/ && $1', "bar", 're_tests 1349/1 (#1567)', :todo);
# 937: ^.{2,3}?((?:b|a|r)+)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^.{2,3}?((?:b|a|r)+)\1\z/ && $1', "bar", 're_tests 1350/1 (#1568)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{2..3}?([b|a|r]+)$1$/ && $1', "bar", 're_tests 1351/1 (#1569)', :todo);
# 938: ^(?:f|o|b){2,3}?((?:b|a|r)+)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){2,3}?((?:b|a|r)+)\1\z/ && $1', "bar", 're_tests 1352/1 (#1570)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{2..3}?([b|a|r]+)$1$/ && $1', "bar", 're_tests 1353/1 (#1571)', :todo);
# 939: ^.{2,3}?(.+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^.{2,3}?(.+?)\1\z/ && $1', "bar", 're_tests 1354/1 (#1572)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{2..3}?(\N+?)$1$/ && $1', "bar", 're_tests 1355/1 (#1573)', :todo);
# 940: ^(?:f|o|b){2,3}?(.+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){2,3}?(.+?)\1\z/ && $1', "bar", 're_tests 1356/1 (#1574)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{2..3}?(\N+?)$1$/ && $1', "bar", 're_tests 1357/1 (#1575)', :todo);
# 941: ^.{2,3}?((?:b|a|r)+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^.{2,3}?((?:b|a|r)+?)\1\z/ && $1', "bar", 're_tests 1358/1 (#1576)', :todo);
is(eval '"foobarbar" ~~ rx/^\N**{2..3}?([b|a|r]+?)$1$/ && $1', "bar", 're_tests 1359/1 (#1577)', :todo);
# 942: ^(?:f|o|b){2,3}?((?:b|a|r)+?)\1\z	foobarbar	y	$1	bar
is(eval '"foobarbar" ~~ rx:perl5/^(?:f|o|b){2,3}?((?:b|a|r)+?)\1\z/ && $1', "bar", 're_tests 1360/1 (#1578)', :todo);
is(eval '"foobarbar" ~~ rx/^[f|o|b]**{2..3}?([b|a|r]+?)$1$/ && $1', "bar", 're_tests 1361/1 (#1579)', :todo);
# 943: .*a(?!(b|cd)*e).*f	......abef	n	-	-	# [perl #23030]
ok(eval 'not ("......abef" ~~ rx:perl5/.*a(?!(b|cd)*e).*f/)', 're_tests 1362  (#1580)', :todo);
ok(eval 'not ("......abef" ~~ rx/\N*a<!before (b|cd)*e>\N*f/)', 're_tests 1363  (#1581)', :todo);
# 944: x(?#	x	c	-	Sequence (?#... not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
# 945: :x(?#:	x	c	-	Sequence (?#... not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
