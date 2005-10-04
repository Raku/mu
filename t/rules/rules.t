#!/usr/bin/pugs

# This file has been generated from re_tests-file (in perl5-sources).
# Since then, this file is edited manually.
# Original lines from re_tests are in comments

# following comments are (temporarily) in effect:
#   - there are many error messages "Warning: PGE doesn't actually do
#     :ignorecase yet"
#   - negated character classes currently are not recognized by PGE.
#     this must be implemented on parrot side. (see docs/quickref/rules)
#   - [\w] in this file was replaced with <alnum>, so underscore '_' was
#     just ignored.
#   - [\s] was replaced with <space>
#   - when 'ab' ~~ /(a|b)*/ evaluated, $0 is an array containing all its
#     matches, and not last value as in Perl5, thus causing many P5-derived
#     tests to behave differently. Currently such places are written
#     as $0[-1], but this is yet to decide. 

use v6;
use Test;

plan 683;

skip_rest "skipped tests - this file will be moved to parrot land";
exit;

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

# I don't know how to get the equivalent of $-[$n] in perl6,
# so this code uses following (yet-undefined) function for those tests:
#    getpos($/, $n) == $-[$n] from perl5
#
# This is just in case these tests might be of some value. (IMHO they are.)
# Of course if these tests are deemed unwanted, they can be easily removed.
# (by grepping for getpos)


# Tests from re_tests in perl5-source

# --- re_tests ---

#   1: abc    abc    y    $&    abc
#   2: abc    abc    y    $-[0]    0
#   3: abc    abc    y    $+[0]    3 # SKIP
is(("abc" ~~ /abc/ && $<>), "abc", 're_tests 2/0 (#3)');
is(("abc" ~~ /abc/ && $/.from), 0, 're_tests 2/0 (#4)');
#   4: abc    xbc    n    -    -
ok((not ("xbc" ~~ /abc/)), 're_tests 4  (#6)');
#   5: abc    axc    n    -    -
ok((not ("axc" ~~ /abc/)), 're_tests 6  (#8)');
#   6: abc    abx    n    -    -
ok((not ("abx" ~~ /abc/)), 're_tests 8  (#10)');
#   7: abc    xabcy    y    $&    abc
#   8: abc    xabcy    y    $-[0]    1
#   9: abc    xabcy    y    $+[0]    4 # SKIP
is(("xabcy" ~~ /abc/ && $<>), "abc", 're_tests 10/0 (#13)');
is(("xabcy" ~~ /abc/ && $/.from), 1, 're_tests 10/0 (#14)');
#  10: abc    ababc    y    $&    abc
#  11: abc    ababc    y    $-[0]    2
#  12: abc    ababc    y    $+[0]    5 # SKIP
is(("ababc" ~~ /abc/ && $<>), "abc", 're_tests 12/0 (#17)');
is(("ababc" ~~ /abc/ && $/.from), 2, 're_tests 12/0 (#18)');
#  13: ab*c    abc    y    $&    abc
#  14: ab*c    abc    y    $-[0]    0
#  15: ab*c    abc    y    $+[0]    3 # SKIP
is(("abc" ~~ /ab*c/ && $<>), "abc", 're_tests 14/0 (#21)');
is(("abc" ~~ /ab*c/ && $/.from), 0, 're_tests 14/0 (#22)');
#  16: ab*bc    abc    y    $&    abc
#  17: ab*bc    abc    y    $-[0]    0
#  18: ab*bc    abc    y    $+[0]    3 # SKIP
is(("abc" ~~ /ab*bc/ && $<>), "abc", 're_tests 16/0 (#25)');
is(("abc" ~~ /ab*bc/ && $/.from), 0, 're_tests 16/0 (#26)');
#  19: ab*bc    abbc    y    $&    abbc
#  20: ab*bc    abbc    y    $-[0]    0
#  21: ab*bc    abbc    y    $+[0]    4 # SKIP
is(("abbc" ~~ /ab*bc/ && $<>), "abbc", 're_tests 18/0 (#29)');
is(("abbc" ~~ /ab*bc/ && $/.from), 0, 're_tests 18/0 (#30)');
#  22: ab*bc    abbbbc    y    $&    abbbbc
#  23: ab*bc    abbbbc    y    $-[0]    0
#  24: ab*bc    abbbbc    y    $+[0]    6 # SKIP
is(("abbbbc" ~~ /ab*bc/ && $<>), "abbbbc", 're_tests 20/0 (#33)');
is(("abbbbc" ~~ /ab*bc/ && $/.from), 0, 're_tests 20/0 (#34)');
#  25: .{1}    abbbbc    y    $&    a
#  26: .{1}    abbbbc    y    $-[0]    0
#  27: .{1}    abbbbc    y    $+[0]    1 # SKIP
is(("abbbbc" ~~ /\N**{1}/ && $<>), "a", 're_tests 22/0 (#37)');
is(("abbbbc" ~~ /\N**{1}/ && $/.from), 0, 're_tests 22/0 (#38)');
#  28: .{3,4}    abbbbc    y    $&    abbb
#  29: .{3,4}    abbbbc    y    $-[0]    0
#  30: .{3,4}    abbbbc    y    $+[0]    4 # SKIP
is(("abbbbc" ~~ /\N**{3..4}/ && $<>), "abbb", 're_tests 24/0 (#41)');
is(("abbbbc" ~~ /\N**{3..4}/ && $/.from), 0, 're_tests 24/0 (#42)');
#  31: ab{0,}bc    abbbbc    y    $&    abbbbc
#  32: ab{0,}bc    abbbbc    y    $-[0]    0
#  33: ab{0,}bc    abbbbc    y    $+[0]    6 # SKIP
is(("abbbbc" ~~ /ab**{0...}bc/ && $<>), "abbbbc", 're_tests 26/0 (#45)');
is(("abbbbc" ~~ /ab**{0...}bc/ && $/.from), 0, 're_tests 26/0 (#46)');
#  34: ab+bc    abbc    y    $&    abbc
#  35: ab+bc    abbc    y    $-[0]    0
#  36: ab+bc    abbc    y    $+[0]    4 # SKIP
is(("abbc" ~~ /ab+bc/ && $<>), "abbc", 're_tests 28/0 (#49)');
is(("abbc" ~~ /ab+bc/ && $/.from), 0, 're_tests 28/0 (#50)');
#  37: ab+bc    abc    n    -    -
ok((not ("abc" ~~ /ab+bc/)), 're_tests 30  (#52)');
#  38: ab+bc    abq    n    -    -
ok((not ("abq" ~~ /ab+bc/)), 're_tests 32  (#54)');
#  39: ab{1,}bc    abq    n    -    -
ok((not ("abq" ~~ /ab**{1...}bc/)), 're_tests 34  (#56)');
#  40: ab+bc    abbbbc    y    $&    abbbbc
#  41: ab+bc    abbbbc    y    $-[0]    0
#  42: ab+bc    abbbbc    y    $+[0]    6 # SKIP
is(("abbbbc" ~~ /ab+bc/ && $<>), "abbbbc", 're_tests 36/0 (#59)');
is(("abbbbc" ~~ /ab+bc/ && $/.from), 0, 're_tests 36/0 (#60)');
#  43: ab{1,}bc    abbbbc    y    $&    abbbbc
#  44: ab{1,}bc    abbbbc    y    $-[0]    0
#  45: ab{1,}bc    abbbbc    y    $+[0]    6 # SKIP
is(("abbbbc" ~~ /ab**{1...}bc/ && $<>), "abbbbc", 're_tests 38/0 (#63)');
is(("abbbbc" ~~ /ab**{1...}bc/ && $/.from), 0, 're_tests 38/0 (#64)');
#  46: ab{1,3}bc    abbbbc    y    $&    abbbbc
#  47: ab{1,3}bc    abbbbc    y    $-[0]    0
#  48: ab{1,3}bc    abbbbc    y    $+[0]    6 # SKIP
is(("abbbbc" ~~ /ab**{1..3}bc/ && $<>), "abbbbc", 're_tests 40/0 (#67)');
is(("abbbbc" ~~ /ab**{1..3}bc/ && $/.from), 0, 're_tests 40/0 (#68)');
#  49: ab{3,4}bc    abbbbc    y    $&    abbbbc
#  50: ab{3,4}bc    abbbbc    y    $-[0]    0
#  51: ab{3,4}bc    abbbbc    y    $+[0]    6 # SKIP
is(("abbbbc" ~~ /ab**{3..4}bc/ && $<>), "abbbbc", 're_tests 42/0 (#71)');
is(("abbbbc" ~~ /ab**{3..4}bc/ && $/.from), 0, 're_tests 42/0 (#72)');
#  52: ab{4,5}bc    abbbbc    n    -    -
ok((not ("abbbbc" ~~ /ab**{4..5}bc/)), 're_tests 44  (#74)');
#  53: ab?bc    abbc    y    $&    abbc
is(("abbc" ~~ /ab?bc/ && $<>), "abbc", 're_tests 46/0 (#76)');
#  54: ab?bc    abc    y    $&    abc
is(("abc" ~~ /ab?bc/ && $<>), "abc", 're_tests 48/0 (#78)');
#  55: ab{0,1}bc    abc    y    $&    abc
is(("abc" ~~ /ab**{0..1}bc/ && $<>), "abc", 're_tests 50/0 (#80)');
#  56: ab?bc    abbbbc    n    -    -
ok((not ("abbbbc" ~~ /ab?bc/)), 're_tests 52  (#82)');
#  57: ab?c    abc    y    $&    abc
is(("abc" ~~ /ab?c/ && $<>), "abc", 're_tests 54/0 (#84)');
#  58: ab{0,1}c    abc    y    $&    abc
is(("abc" ~~ /ab**{0..1}c/ && $<>), "abc", 're_tests 56/0 (#86)');
#  59: ^abc$    abc    y    $&    abc
is(("abc" ~~ /^abc$/ && $<>), "abc", 're_tests 58/0 (#88)');
#  60: ^abc$    abcc    n    -    -
ok((not ("abcc" ~~ /^abc$/)), 're_tests 60  (#90)');
#  61: ^abc    abcc    y    $&    abc
is(("abcc" ~~ /^abc/ && $<>), "abc", 're_tests 62/0 (#92)');
#  62: ^abc$    aabc    n    -    -
ok((not ("aabc" ~~ /^abc$/)), 're_tests 64  (#94)');
#  63: abc$    aabc    y    $&    abc
is(("aabc" ~~ /abc$/ && $<>), "abc", 're_tests 66/0 (#96)');
#  64: abc$    aabcd    n    -    -
ok((not ("aabcd" ~~ /abc$/)), 're_tests 68  (#98)');
#  65: ^    abc    y    $&    
is(("abc" ~~ /^/ && $<>), "", 're_tests 70/0 (#100)');
#  66: $    abc    y    $&    
is(("abc" ~~ /$/ && $<>), "", 're_tests 72/0 (#102)');
#  67: a.c    abc    y    $&    abc
is(("abc" ~~ /a\Nc/ && $<>), "abc", 're_tests 74/0 (#104)');
#  68: a.c    axc    y    $&    axc
is(("axc" ~~ /a\Nc/ && $<>), "axc", 're_tests 76/0 (#106)');
#  69: a.*c    axyzc    y    $&    axyzc
is(("axyzc" ~~ /a\N*c/ && $<>), "axyzc", 're_tests 78/0 (#108)');
#  70: a.*c    axyzd    n    -    -
ok((not ("axyzd" ~~ /a\N*c/)), 're_tests 80  (#110)');
#  71: a[bc]d    abc    n    -    -
ok((not ("abc" ~~ /a<[bc]>d/)), 're_tests 82  (#112)');
#  72: a[bc]d    abd    y    $&    abd
is(("abd" ~~ /a<[bc]>d/ && $<>), "abd", 're_tests 84/0 (#114)');
#  73: a[b-d]e    abd    n    -    -
ok((not ("abd" ~~ /a<[b..d]>e/)), 're_tests 86  (#116)');
#  74: a[b-d]e    ace    y    $&    ace
is(("ace" ~~ /a<[b..d]>e/ && $<>), "ace", 're_tests 88/0 (#118)');
#  75: a[b-d]    aac    y    $&    ac
is(("aac" ~~ /a<[b..d]>/ && $<>), "ac", 're_tests 90/0 (#120)');
#  76: a[-b]    a-    y    $&    a-
is(("a-" ~~ /a<[\-b]>/ && $<>), "a-", 're_tests 92/0 (#122)');
#  77: a[b-]    a-    y    $&    a-
is(("a-" ~~ /a<[b\-]>/ && $<>), "a-", 're_tests 94/0 (#124)');
#  78: a[b-a]    -    c    -    Invalid [] range "b-a"
# -- SKIPPED - TESTS ERROR MESSAGE
#  79: a[]b    -    c    -    Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
#  80: a[    -    c    -    Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
#  81: a]    a]    y    $&    a]
is(("a]" ~~ /a<[\]]>/ && $<>), "a]", 're_tests 96/0 (#126)');
#  82: a[]]b    a]b    y    $&    a]b
is(("a]b" ~~ /a<[\]]>b/ && $<>), "a]b", 're_tests 98/0 (#128)');
#  83: a[^bc]d    aed    y    $&    aed
is(("aed" ~~ /a<-[bc]>d/ && $<>), "aed", 're_tests 100/0 (#130)');
#  84: a[^bc]d    abd    n    -    -
ok((not ("abd" ~~ /a<-[bc]>d/)), 're_tests 102  (#132)');
#  85: a[^-b]c    adc    y    $&    adc
is(("adc" ~~ /a<-[\-b]>c/ && $<>), "adc", 're_tests 104/0 (#134)');
#  86: a[^-b]c    a-c    n    -    -
ok((not ("a-c" ~~ /a<-[\-b]>c/)), 're_tests 106  (#136)');
#  87: a[^]b]c    a]c    n    -    -
ok((not ("a]c" ~~ /a<-[\]b]>c/)), 're_tests 108  (#138)');
#  88: a[^]b]c    adc    y    $&    adc
is(("adc" ~~ /a<-[\]b]>c/ && $<>), "adc", 're_tests 110/0 (#140)');
#  89: \ba\b    a-    y    -    -
ok(("a-" ~~ /\ba\b/), 're_tests 112  (#142)');
#  90: \ba\b    -a    y    -    -
ok(("-a" ~~ /\ba\b/), 're_tests 114  (#144)');
#  91: \ba\b    -a-    y    -    -
ok(("-a-" ~~ /\ba\b/), 're_tests 116  (#146)');
#  92: \by\b    xy    n    -    -
ok((not ("xy" ~~ /\by\b/)), 're_tests 118  (#148)');
#  93: \by\b    yz    n    -    -
ok((not ("yz" ~~ /\by\b/)), 're_tests 120  (#150)');
#  94: \by\b    xyz    n    -    -
ok((not ("xyz" ~~ /\by\b/)), 're_tests 122  (#152)');
#  95: \Ba\B    a-    n    -    -
ok((not ("a-" ~~ /\Ba\B/)), 're_tests 124  (#154)');
#  96: \Ba\B    -a    n    -    -
ok((not ("-a" ~~ /\Ba\B/)), 're_tests 126  (#156)');
#  97: \Ba\B    -a-    n    -    -
ok((not ("-a-" ~~ /\Ba\B/)), 're_tests 128  (#158)');
#  98: \By\b    xy    y    -    -
ok(("xy" ~~ /\By\b/), 're_tests 130  (#160)');
#  99: \By\b    xy    y    $-[0]    1
# 100: \By\b    xy    y    $+[0]    2 # SKIP
is(("xy" ~~ /\By\b/ && $/.from), 1, 're_tests 132/0 (#162)');
# 101: \By\b    xy    y    -    -
ok(("xy" ~~ /\By\b/), 're_tests 134  (#164)');
# 102: \by\B    yz    y    -    -
ok(("yz" ~~ /\by\B/), 're_tests 136  (#166)');
# 103: \By\B    xyz    y    -    -
ok(("xyz" ~~ /\By\B/), 're_tests 138  (#168)');
# 104: \w    a    y    -    -
ok(("a" ~~ /\w/), 're_tests 140  (#170)');
# 105: \w    -    n    -    -
ok((not ("-" ~~ /\w/)), 're_tests 142  (#172)');
# 106: \W    a    n    -    -
ok((not ("a" ~~ /\W/)), 're_tests 144  (#174)');
# 107: \W    -    y    -    -
ok(("-" ~~ /\W/), 're_tests 146  (#176)');
# 108: a\sb    a b    y    -    -
ok(("a b" ~~ /a\sb/), 're_tests 148  (#178)');
# 109: a\sb    a-b    n    -    -
ok((not ("a-b" ~~ /a\sb/)), 're_tests 150  (#180)');
# 110: a\Sb    a b    n    -    -
ok((not ("a b" ~~ /a\Sb/)), 're_tests 152  (#182)');
# 111: a\Sb    a-b    y    -    -
ok(("a-b" ~~ /a\Sb/), 're_tests 154  (#184)');
# 112: \d    1    y    -    -
ok(("1" ~~ /\d/), 're_tests 156  (#186)');
# 113: \d    -    n    -    -
ok((not ("-" ~~ /\d/)), 're_tests 158  (#188)');
# 114: \D    1    n    -    -
ok((not ("1" ~~ /\D/)), 're_tests 160  (#190)');
# 115: \D    -    y    -    -
ok(("-" ~~ /\D/), 're_tests 162  (#192)');
# 116: [\w]    a    y    -    -
ok(("a" ~~ /<alnum>/), 're_tests 164  (#194)');
# 117: [\w]    -    n    -    -
ok((not ("-" ~~ /<alnum>/)), 're_tests 166  (#196)');
# 118: [\W]    a    n    -    -
ok((not ("a" ~~ /<-alnum>/)), 're_tests 168  (#198)', :todo<feature>);
# 119: [\W]    -    y    -    -
ok(("-" ~~ /<-alnum>/), 're_tests 170  (#200)', :todo<feature>);
# 120: a[\s]b    a b    y    -    -
ok(("a b" ~~ /a<space>b/), 're_tests 172  (#202)');
# 121: a[\s]b    a-b    n    -    -
ok((not ("a-b" ~~ /a<space>b/)), 're_tests 174  (#204)');
# 122: a[\S]b    a b    n    -    -
ok((not ("a b" ~~ /a<-space>b/)), 're_tests 176  (#206)', :todo<feature>);
# 123: a[\S]b    a-b    y    -    -
ok(("a-b" ~~ /a<-space>b/), 're_tests 178  (#208)', :todo<feature>);
# 124: [\d]    1    y    -    -
ok(("1" ~~ /<digit>/), 're_tests 180  (#210)');
# 125: [\d]    -    n    -    -
ok((not ("-" ~~ /<digit>/)), 're_tests 182  (#212)');
# 126: [\D]    1    n    -    -
ok((not ("1" ~~ /<-digit>/)), 're_tests 184  (#214)', :todo<feature>);
# 127: [\D]    -    y    -    -
ok(("-" ~~ /<-digit>/), 're_tests 186  (#216)', :todo<feature>);
# 128: ab|cd    abc    y    $&    ab
is(("abc" ~~ /ab|cd/ && $<>), "ab", 're_tests 188/0 (#218)');
# 129: ab|cd    abcd    y    $&    ab
is(("abcd" ~~ /ab|cd/ && $<>), "ab", 're_tests 190/0 (#220)');
# 130: ()ef    def    y    $&-$0    ef-
is(("def" ~~ /(<?null>)ef/ && $<>), "ef", 're_tests 192/0 (#223)');
is(("def" ~~ /(<?null>)ef/ && $0), "", 're_tests 192/1 (#224)');
# 131: ()ef    def    y    $-[0]    1
# 132: ()ef    def    y    $+[0]    3 # SKIP
is(("def" ~~ /(<?null>)ef/ && $/.from), 1, 're_tests 194/0 (#226)');
# 133: ()ef    def    y    $-[1]    1
# 134: ()ef    def    y    $+[1]    1 # SKIP
is(("def" ~~ /(<?null>)ef/ && $/[0].from), 1, 're_tests 196/1 (#228)');
# 135: *a    -    c    -    Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 136: (*)b    -    c    -    Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 137: $b    b    n    -    -
my $b = 'x';
ok((not ("b" ~~ /$b/)), 're_tests 198  (#230)');
# 138: a\    -    c    -    Search pattern not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
# 139: a\(b    a(b    y    $&-$0    a(b-
is(("a(b" ~~ /a\(b/ && $<>), "a(b", 're_tests 200/0 (#233)');
is(("a(b" ~~ /a\(b/ && $0), "", 're_tests 200/1 (#234)');
# 140: a\(*b    ab    y    $&    ab
is(("ab" ~~ /a\(*b/ && $<>), "ab", 're_tests 202/0 (#236)');
# 141: a\(*b    a((b    y    $&    a((b
is(("a((b" ~~ /a\(*b/ && $<>), "a((b", 're_tests 204/0 (#238)');
# 142: a\\b    a\b    y    $&    a\b
is(("a\\b" ~~ /a\\b/ && $<>), "a\\b", 're_tests 206/0 (#240)');
# 143: abc)    -    c    -    Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 144: (abc    -    c    -    Unmatched (
# -- SKIPPED - TESTS ERROR MESSAGE
# 145: ((a))    abc    y    $&-$0-$1    a-a-a
is(("abc" ~~ /((a))/ && $<>), "a", 're_tests 208/0 (#244)');
is(("abc" ~~ /((a))/ && $0), "a", 're_tests 208/1 (#245)');
is(("abc" ~~ /((a))/ && $1), "a", 're_tests 208/2 (#246)', :todo<bug>);
# 146: ((a))    abc    y    $-[0]-$-[1]-$-[2]    0-0-0
# 147: ((a))    abc    y    $+[0]-$+[1]-$+[2]    1-1-1 # SKIP
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# 148: ((a))    abc    b    @-    0 0 0
# SKIPPED: script doesn't understand `@-' yet
# SKIPPED: script doesn't understand `@-' yet
# 149: ((a))    abc    b    @+    1 1 1
# SKIPPED: script doesn't understand `@+' yet
# SKIPPED: script doesn't understand `@+' yet
# 150: (a)b(c)    abc    y    $&-$0-$1    abc-a-c
is(("abc" ~~ /(a)b(c)/ && $<>), "abc", 're_tests 210/0 (#250)');
is(("abc" ~~ /(a)b(c)/ && $0), "a", 're_tests 210/1 (#251)');
is(("abc" ~~ /(a)b(c)/ && $1), "c", 're_tests 210/2 (#252)');
# 151: (a)b(c)    abc    y    $-[0]-$-[1]-$-[2]    0-0-2
# 152: (a)b(c)    abc    y    $+[0]-$+[1]-$+[2]    3-1-3 # SKIP
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# SKIPPED: script doesn't understand `$-[0]-$-[1]-$-[2]' yet
# 153: a+b+c    aabbabc    y    $&    abc
is(("aabbabc" ~~ /a+b+c/ && $<>), "abc", 're_tests 212/0 (#254)');
# 154: a{1,}b{1,}c    aabbabc    y    $&    abc
is(("aabbabc" ~~ /a**{1...}b**{1...}c/ && $<>), "abc", 're_tests 214/0 (#256)');
# 155: a**    -    c    -    Nested quantifiers
# -- SKIPPED - TESTS ERROR MESSAGE
# 156: a.+?c    abcabc    y    $&    abc
is(("abcabc" ~~ /a\N+?c/ && $<>), "abc", 're_tests 216/0 (#258)');
# 157: (a+|b)*    ab    y    $&-$0    ab-b
is(("ab" ~~ /(a+|b)*/ && $<>), "ab", 're_tests 218/0 (#261)');
is(("ab" ~~ /(a+|b)*/ && $0[-1]), "b", 're_tests 218/1 (#262)');
# 158: (a+|b)*    ab    y    $-[0]    0
# 159: (a+|b)*    ab    y    $+[0]    2 # SKIP
is(("ab" ~~ /(a+|b)*/ && $/.from), 0, 're_tests 220/0 (#264)');
# 160: (a+|b)*    ab    y    $-[1]    1
# 161: (a+|b)*    ab    y    $+[1]    2 # SKIP
is(("ab" ~~ /(a+|b)*/ && $/[0].from), 1, 're_tests 222/1 (#266)', :todo<bug>);
# 162: (a+|b){0,}    ab    y    $&-$0    ab-b
is(("ab" ~~ /(a+|b)**{0...}/ && $<>), "ab", 're_tests 224/0 (#269)');
is(("ab" ~~ /(a+|b)**{0...}/ && $0), "b", 're_tests 224/1 (#270)');
# 163: (a+|b)+    ab    y    $&-$0    ab-b
is(("ab" ~~ /(a+|b)+/ && $<>), "ab", 're_tests 226/0 (#273)');
is(("ab" ~~ /(a+|b)+/ && $0[-1]), "b", 're_tests 226/1 (#274)');
# 164: (a+|b){1,}    ab    y    $&-$0    ab-b
is(("ab" ~~ /(a+|b)**{1...}/ && $<>), "ab", 're_tests 228/0 (#277)');
is(("ab" ~~ /(a+|b)**{1...}/ && $0), "b", 're_tests 228/1 (#278)');
# 165: (a+|b)?    ab    y    $&-$0    a-a
is(("ab" ~~ /(a+|b)?/ && $<>), "a", 're_tests 230/0 (#281)');
is(("ab" ~~ /(a+|b)?/ && $0), "a", 're_tests 230/1 (#282)');
# 166: (a+|b){0,1}    ab    y    $&-$0    a-a
is(("ab" ~~ /(a+|b)**{0..1}/ && $<>), "a", 're_tests 232/0 (#285)');
is(("ab" ~~ /(a+|b)**{0..1}/ && $0), "a", 're_tests 232/1 (#286)');
# 167: )(    -    c    -    Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 168: [^ab]*    cde    y    $&    cde
is(("cde" ~~ /<-[ab]>*/ && $<>), "cde", 're_tests 234/0 (#288)');
# 169: abc        n    -    -
ok((not ("" ~~ /abc/)), 're_tests 236  (#290)');
# 170: a*        y    $&    
is(("" ~~ /a*/ && $<>), "", 're_tests 238/0 (#292)');
# 171: ([abc])*d    abbbcd    y    $&-$0    abbbcd-c
is(("abbbcd" ~~ /(<[abc]>)*d/ && $<>), "abbbcd", 're_tests 240/0 (#295)');
is(("abbbcd" ~~ /(<[abc]>)*d/ && $0[-1]), "c", 're_tests 240/1 (#296)');
# 172: ([abc])*bcd    abcd    y    $&-$0    abcd-a
is(("abcd" ~~ /(<[abc]>)*bcd/ && $<>), "abcd", 're_tests 242/0 (#299)');
is(("abcd" ~~ /(<[abc]>)*bcd/ && $0), "a", 're_tests 242/1 (#300)');
# 173: a|b|c|d|e    e    y    $&    e
is(("e" ~~ /a|b|c|d|e/ && $<>), "e", 're_tests 244/0 (#302)');
# 174: (a|b|c|d|e)f    ef    y    $&-$0    ef-e
is(("ef" ~~ /(a|b|c|d|e)f/ && $<>), "ef", 're_tests 246/0 (#305)');
is(("ef" ~~ /(a|b|c|d|e)f/ && $0), "e", 're_tests 246/1 (#306)');
# 175: (a|b|c|d|e)f    ef    y    $-[0]    0
# 176: (a|b|c|d|e)f    ef    y    $+[0]    2 # SKIP
is(("ef" ~~ /(a|b|c|d|e)f/ && $/.from), 0, 're_tests 248/0 (#308)');
# 177: (a|b|c|d|e)f    ef    y    $-[1]    0
# 178: (a|b|c|d|e)f    ef    y    $+[1]    1 # SKIP
is(("ef" ~~ /(a|b|c|d|e)f/ && $/[0].from), 0, 're_tests 250/1 (#310)');
# 179: abcd*efg    abcdefg    y    $&    abcdefg
is(("abcdefg" ~~ /abcd*efg/ && $<>), "abcdefg", 're_tests 252/0 (#312)');
# 180: ab*    xabyabbbz    y    $&    ab
is(("xabyabbbz" ~~ /ab*/ && $<>), "ab", 're_tests 254/0 (#314)');
# 181: ab*    xayabbbz    y    $&    a
is(("xayabbbz" ~~ /ab*/ && $<>), "a", 're_tests 256/0 (#316)');
# 182: (ab|cd)e    abcde    y    $&-$0    cde-cd
is(("abcde" ~~ /(ab|cd)e/ && $<>), "cde", 're_tests 258/0 (#319)');
is(("abcde" ~~ /(ab|cd)e/ && $0), "cd", 're_tests 258/1 (#320)');
# 183: [abhgefdc]ij    hij    y    $&    hij
is(("hij" ~~ /<[abhgefdc]>ij/ && $<>), "hij", 're_tests 260/0 (#322)');
# 184: ^(ab|cd)e    abcde    n    x$0y    xy
# SKIPPED: script doesn't understand `x$0y' yet
# SKIPPED: script doesn't understand `x$0y' yet
# 185: (abc|)ef    abcdef    y    $&-$0    ef-
is(("abcdef" ~~ /(abc|<?null>)ef/ && $<>), "ef", 're_tests 262/0 (#325)');
is(("abcdef" ~~ /(abc|<?null>)ef/ && $0), "", 're_tests 262/1 (#326)');
# 186: (a|b)c*d    abcd    y    $&-$0    bcd-b
is(("abcd" ~~ /(a|b)c*d/ && $<>), "bcd", 're_tests 264/0 (#329)');
is(("abcd" ~~ /(a|b)c*d/ && $0), "b", 're_tests 264/1 (#330)');
# 187: (ab|ab*)bc    abc    y    $&-$0    abc-a
is(("abc" ~~ /(ab|ab*)bc/ && $<>), "abc", 're_tests 266/0 (#333)');
is(("abc" ~~ /(ab|ab*)bc/ && $0), "a", 're_tests 266/1 (#334)');
# 188: a([bc]*)c*    abc    y    $&-$0    abc-bc
is(("abc" ~~ /a(<[bc]>*)c*/ && $<>), "abc", 're_tests 268/0 (#337)');
is(("abc" ~~ /a(<[bc]>*)c*/ && $0), "bc", 're_tests 268/1 (#338)');
# 189: a([bc]*)(c*d)    abcd    y    $&-$0-$1    abcd-bc-d
is(("abcd" ~~ /a(<[bc]>*)(c*d)/ && $<>), "abcd", 're_tests 270/0 (#342)');
is(("abcd" ~~ /a(<[bc]>*)(c*d)/ && $0), "bc", 're_tests 270/1 (#343)');
is(("abcd" ~~ /a(<[bc]>*)(c*d)/ && $1), "d", 're_tests 270/2 (#344)');
# 190: a([bc]*)(c*d)    abcd    y    $-[0]    0
# 191: a([bc]*)(c*d)    abcd    y    $+[0]    4 # SKIP
is(("abcd" ~~ /a(<[bc]>*)(c*d)/ && $/.from), 0, 're_tests 272/0 (#346)');
# 192: a([bc]*)(c*d)    abcd    y    $-[1]    1
# 193: a([bc]*)(c*d)    abcd    y    $+[1]    3 # SKIP
is(("abcd" ~~ /a(<[bc]>*)(c*d)/ && $/[0].from), 1, 're_tests 274/1 (#348)');
# 194: a([bc]*)(c*d)    abcd    y    $-[2]    3
# 195: a([bc]*)(c*d)    abcd    y    $+[2]    4 # SKIP
is(("abcd" ~~ /a(<[bc]>*)(c*d)/ && $/[1].from), 3, 're_tests 276/2 (#350)');
# 196: a([bc]+)(c*d)    abcd    y    $&-$0-$1    abcd-bc-d
is(("abcd" ~~ /a(<[bc]>+)(c*d)/ && $<>), "abcd", 're_tests 278/0 (#354)');
is(("abcd" ~~ /a(<[bc]>+)(c*d)/ && $0), "bc", 're_tests 278/1 (#355)');
is(("abcd" ~~ /a(<[bc]>+)(c*d)/ && $1), "d", 're_tests 278/2 (#356)');
# 197: a([bc]*)(c+d)    abcd    y    $&-$0-$1    abcd-b-cd
is(("abcd" ~~ /a(<[bc]>*)(c+d)/ && $<>), "abcd", 're_tests 280/0 (#360)');
is(("abcd" ~~ /a(<[bc]>*)(c+d)/ && $0), "b", 're_tests 280/1 (#361)');
is(("abcd" ~~ /a(<[bc]>*)(c+d)/ && $1), "cd", 're_tests 280/2 (#362)');
# 198: a([bc]*)(c+d)    abcd    y    $-[0]    0
# 199: a([bc]*)(c+d)    abcd    y    $+[0]    4 # SKIP
is(("abcd" ~~ /a(<[bc]>*)(c+d)/ && $/.from), 0, 're_tests 282/0 (#364)');
# 200: a([bc]*)(c+d)    abcd    y    $-[1]    1
# 201: a([bc]*)(c+d)    abcd    y    $+[1]    2 # SKIP
is(("abcd" ~~ /a(<[bc]>*)(c+d)/ && $/[0].from), 1, 're_tests 284/1 (#366)');
# 202: a([bc]*)(c+d)    abcd    y    $-[2]    2
# 203: a([bc]*)(c+d)    abcd    y    $+[2]    4 # SKIP
is(("abcd" ~~ /a(<[bc]>*)(c+d)/ && $/[1].from), 2, 're_tests 286/2 (#368)');
# 204: a[bcd]*dcdcde    adcdcde    y    $&    adcdcde
is(("adcdcde" ~~ /a<[bcd]>*dcdcde/ && $<>), "adcdcde", 're_tests 288/0 (#370)');
# 205: a[bcd]+dcdcde    adcdcde    n    -    -
ok((not ("adcdcde" ~~ /a<[bcd]>+dcdcde/)), 're_tests 290  (#372)');
# 206: (ab|a)b*c    abc    y    $&-$0    abc-ab
is(("abc" ~~ /(ab|a)b*c/ && $<>), "abc", 're_tests 292/0 (#375)');
is(("abc" ~~ /(ab|a)b*c/ && $0), "ab", 're_tests 292/1 (#376)');
# 207: (ab|a)b*c    abc    y    $-[0]    0
# 208: (ab|a)b*c    abc    y    $+[0]    3 # SKIP
is(("abc" ~~ /(ab|a)b*c/ && $/.from), 0, 're_tests 294/0 (#378)');
# 209: (ab|a)b*c    abc    y    $-[1]    0
# 210: (ab|a)b*c    abc    y    $+[1]    2 # SKIP
is(("abc" ~~ /(ab|a)b*c/ && $/[0].from), 0, 're_tests 296/1 (#380)');
# 211: ((a)(b)c)(d)    abcd    y    $0-$1-$2-$3    abc-a-b-d
# SKIPPED: script doesn't understand `$0-$1-$2-$3' yet
# SKIPPED: script doesn't understand `$0-$1-$2-$3' yet
# 212: ((a)(b)c)(d)    abcd    y    $-[0]    0
# 213: ((a)(b)c)(d)    abcd    y    $+[0]    4 # SKIP
is(("abcd" ~~ /((a)(b)c)(d)/ && $/.from), 0, 're_tests 298/0 (#382)');
# 214: ((a)(b)c)(d)    abcd    y    $-[1]    0
# 215: ((a)(b)c)(d)    abcd    y    $+[1]    3 # SKIP
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[0].from), 0, 're_tests 300/1 (#384)');
# 216: ((a)(b)c)(d)    abcd    y    $-[2]    0
# 217: ((a)(b)c)(d)    abcd    y    $+[2]    1 # SKIP
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[1].from), 0, 're_tests 302/2 (#386)', :todo<bug>);
# 218: ((a)(b)c)(d)    abcd    y    $-[3]    1
# 219: ((a)(b)c)(d)    abcd    y    $+[3]    2 # SKIP
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[2].from), 1, 're_tests 304/3 (#388)', :todo<bug>);
# 220: ((a)(b)c)(d)    abcd    y    $-[4]    3
# 221: ((a)(b)c)(d)    abcd    y    $+[4]    4 # SKIP
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[3].from), 3, 're_tests 306/4 (#390)', :todo<bug>);
# 222: [a..zA..Z_][a..zA..Z0..9_]*    alpha    y    $&    alpha
is(("alpha" ~~ /<[a..zA..Z_]><[a..zA..Z0..9_]>*/ && $<>), "alpha", 're_tests 308/0 (#392)');
# 223: ^a(bc+|b[eh])g|.h$    abh    y    $&-$0    bh-
is(("abh" ~~ /^a(bc+|b<[eh]>)g|\Nh$/ && $<>), "bh", 're_tests 310/0 (#395)');
is(("abh" ~~ /^a(bc+|b<[eh]>)g|\Nh$/ && $0), "", 're_tests 310/1 (#396)');
# 224: (bc+d$|ef*g.|h?i(j|k))    effgz    y    $&-$0-$1    effgz-effgz-
is(("effgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $<>), "effgz", 're_tests 312/0 (#400)');
is(("effgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $0), "effgz", 're_tests 312/1 (#401)');
is(("effgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $1), "", 're_tests 312/2 (#402)');
# 225: (bc+d$|ef*g.|h?i(j|k))    ij    y    $&-$0-$1    ij-ij-j
is(("ij" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $<>), "ij", 're_tests 314/0 (#406)');
is(("ij" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $0), "ij", 're_tests 314/1 (#407)');
is(("ij" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $1), "j", 're_tests 314/2 (#408)', :todo<bug>);
# 226: (bc+d$|ef*g.|h?i(j|k))    effg    n    -    -
ok((not ("effg" ~~ /(bc+d$|ef*g\N|h?i(j|k))/)), 're_tests 316  (#410)');
# 227: (bc+d$|ef*g.|h?i(j|k))    bcdd    n    -    -
ok((not ("bcdd" ~~ /(bc+d$|ef*g\N|h?i(j|k))/)), 're_tests 318  (#412)');
# 228: (bc+d$|ef*g.|h?i(j|k))    reffgz    y    $&-$0-$1    effgz-effgz-
is(("reffgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $<>), "effgz", 're_tests 320/0 (#416)');
is(("reffgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $0), "effgz", 're_tests 320/1 (#417)');
is(("reffgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $1), "", 're_tests 320/2 (#418)');
# 229: ((((((((((a))))))))))    a    y    $00    a
is(("a" ~~ /((((((((((a))))))))))/ && $00), "a", 're_tests 322/10 (#420)');
# 230: ((((((((((a))))))))))    a    y    $-[0]    0
# 231: ((((((((((a))))))))))    a    y    $+[0]    1 # SKIP
is(("a" ~~ /((((((((((a))))))))))/ && $/.from), 0, 're_tests 324/0 (#422)');
# 232: ((((((((((a))))))))))    a    y    $-[10]    0
# 233: ((((((((((a))))))))))    a    y    $+[10]    1 # SKIP
is(("a" ~~ /((((((((((a))))))))))/ && $/[0].from), 0, 're_tests 326/10 (#424)');
# 234: ((((((((((a))))))))))\10    aa    y    $&    aa
is(("aa" ~~ /((((((((((a))))))))))$00/ && $<>), "aa", 're_tests 328/0 (#426)');
# 235: ((((((((((a))))))))))${bang}    aa    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 236: ((((((((((a))))))))))${bang}    a!    y    $&    a!
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 237: (((((((((a)))))))))    a    y    $&    a
is(("a" ~~ /(((((((((a)))))))))/ && $<>), "a", 're_tests 332/0 (#430)');
# 238: multiple words of text    uh-uh    n    -    -
ok((not ("uh-uh" ~~ /multiple<sp>words<sp>of<sp>text/)), 're_tests 334  (#432)');
# 239: multiple words    multiple words, yeah    y    $&    multiple words
is(("multiple words, yeah" ~~ /multiple<sp>words/ && $<>), "multiple words", 're_tests 336/0 (#434)');
# 240: (.*)c(.*)    abcde    y    $&-$0-$1    abcde-ab-de
is(("abcde" ~~ /(\N*)c(\N*)/ && $<>), "abcde", 're_tests 338/0 (#438)');
is(("abcde" ~~ /(\N*)c(\N*)/ && $0), "ab", 're_tests 338/1 (#439)');
is(("abcde" ~~ /(\N*)c(\N*)/ && $1), "de", 're_tests 338/2 (#440)');
# 241: \((.*), (.*)\)    (a, b)    y    ($1, $0)    (b, a)
# SKIPPED: script doesn't understand `($1, $0)' yet
# SKIPPED: script doesn't understand `($1, $0)' yet
# 242: [k]    ab    n    -    -
ok((not ("ab" ~~ /<[k]>/)), 're_tests 340  (#442)');
# 243: abcd    abcd    y    $&-\$&-\\$&    abcd-$&-\abcd
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# 244: a(bc)d    abcd    y    $0-\$0-\\$0    bc-$0-\bc
# SKIPPED: script doesn't understand `$0-\$0-\\$0' yet
# SKIPPED: script doesn't understand `$0-\$0-\\$0' yet
# 245: a[-]?c    ac    y    $&    ac
skip 1, "I'm not sure what this test should be.";
#is(("ac" ~~ /a<[..]>?c/ && $<>), "ac", 're_tests 342/0 (#444)');
# 246: (abc)\1    abcabc    y    $0    abc
is(("abcabc" ~~ /(abc)$0/ && $0), "abc", 're_tests 344/1 (#446)');
# 247: ([a-c]*)\1    abcabc    y    $0    abc
is(("abcabc" ~~ /(<[a..c]>*)$0/ && $0), "abc", 're_tests 346/1 (#448)');
# 248: \1    -    c    -    Reference to nonexistent group
# -- SKIPPED - TESTS ERROR MESSAGE
# 249: \2    -    c    -    Reference to nonexistent group
# -- SKIPPED - TESTS ERROR MESSAGE
# 250: (a)|\1    a    y    -    -
#ok(("a" ~~ /(a)|$0/), 're_tests 348  (#450)');
# 251: (a)|\1    x    n    -    -
#ok((not ("x" ~~ /(a)|$0/)), 're_tests 350  (#452)');
# 252: (a)|\2    -    c    -    Reference to nonexistent group
# -- SKIPPED - TESTS ERROR MESSAGE
# 253: (([a-c])b*?\2)*    ababbbcbc    y    $&-$0-$1    ababb-bb-b
is(("ababbbcbc" ~~ /((<[a..c]>)b*?$1)*/ && $<>), "ababb", 're_tests 352/0 (#456)', :todo<feature>);
is(("ababbbcbc" ~~ /((<[a..c]>)b*?$1)*/ && $0), "bb", 're_tests 352/1 (#457)', :todo<feature>);
is(("ababbbcbc" ~~ /((<[a..c]>)b*?$1)*/ && $1), "b", 're_tests 352/2 (#458)', :todo<feature>);
# 254: (([a-c])b*?\2){3}    ababbbcbc    y    $&-$0-$1    ababbbcbc-cbc-c
is(("ababbbcbc" ~~ /((<[a..c]>)b*?$1)**{3}/ && $<>), "ababbbcbc", 're_tests 354/0 (#462)', :todo<feature>);
is(("ababbbcbc" ~~ /((<[a..c]>)b*?$1)**{3}/ && $0), "cbc", 're_tests 354/1 (#463)', :todo<feature>);
is(("ababbbcbc" ~~ /((<[a..c]>)b*?$1)**{3}/ && $1), "c", 're_tests 354/2 (#464)', :todo<feature>);
# 255: ((\3|b)\2(a)x)+    aaxabxbaxbbx    n    -    -
#ok((not ("aaxabxbaxbbx" ~~ /(($2|b)$1(a)x)+/)), 're_tests 356  (#466)');
# 256: ((\3|b)\2(a)x)+    aaaxabaxbaaxbbax    y    $&-$0-$1-$2    bbax-bbax-b-a
# SKIPPED: script doesn't understand `$&-$0-$1-$2' yet
# SKIPPED: script doesn't understand `$&-$0-$1-$2' yet
# 257: ((\3|b)\2(a)){2,}    bbaababbabaaaaabbaaaabba    y    $&-$0-$1-$2    bbaaaabba-bba-b-a
# SKIPPED: script doesn't understand `$&-$0-$1-$2' yet
# SKIPPED: script doesn't understand `$&-$0-$1-$2' yet
# 258: (a)|(b)    b    y    $-[0]    0
# 259: (a)|(b)    b    y    $+[0]    1 # SKIP
is(("b" ~~ /(a)|(b)/ && $/.from), 0, 're_tests 358/0 (#468)');
# 260: (a)|(b)    b    y    x$-[1]    x
# 261: (a)|(b)    b    y    x$+[1]    x # SKIP
# SKIPPED: script doesn't understand `x$-[1]' yet
# SKIPPED: script doesn't understand `x$-[1]' yet
# 262: (a)|(b)    b    y    $-[2]    0
# 263: (a)|(b)    b    y    $+[2]    1 # SKIP
is(("b" ~~ /(a)|(b)/ && $/[1].from), 0, 're_tests 360/2 (#470)');
# 264: 'abc'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/abc/ && $<>), "ABC", 're_tests 362/0 (#472)', :todo<feature>);
# 265: 'abc'i    XBC    n    -    -
ok((not ("XBC" ~~ rx:i/abc/)), 're_tests 364  (#474)');
# 266: 'abc'i    AXC    n    -    -
ok((not ("AXC" ~~ rx:i/abc/)), 're_tests 366  (#476)');
# 267: 'abc'i    ABX    n    -    -
ok((not ("ABX" ~~ rx:i/abc/)), 're_tests 368  (#478)');
# 268: 'abc'i    XABCY    y    $&    ABC
is(("XABCY" ~~ rx:i/abc/ && $<>), "ABC", 're_tests 370/0 (#480)', :todo<feature>);
# 269: 'abc'i    ABABC    y    $&    ABC
is(("ABABC" ~~ rx:i/abc/ && $<>), "ABC", 're_tests 372/0 (#482)', :todo<feature>);
# 270: 'ab*c'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/ab*c/ && $<>), "ABC", 're_tests 374/0 (#484)', :todo<feature>);
# 271: 'ab*bc'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/ab*bc/ && $<>), "ABC", 're_tests 376/0 (#486)', :todo<feature>);
# 272: 'ab*bc'i    ABBC    y    $&    ABBC
is(("ABBC" ~~ rx:i/ab*bc/ && $<>), "ABBC", 're_tests 378/0 (#488)', :todo<feature>);
# 273: 'ab*?bc'i    ABBBBC    y    $&    ABBBBC
is(("ABBBBC" ~~ rx:i/ab*?bc/ && $<>), "ABBBBC", 're_tests 380/0 (#490)', :todo<feature>);
# 274: 'ab{0,}?bc'i    ABBBBC    y    $&    ABBBBC
is(("ABBBBC" ~~ rx:i/ab**{0...}?bc/ && $<>), "ABBBBC", 're_tests 382/0 (#492)', :todo<feature>);
# 275: 'ab+?bc'i    ABBC    y    $&    ABBC
is(("ABBC" ~~ rx:i/ab+?bc/ && $<>), "ABBC", 're_tests 384/0 (#494)', :todo<feature>);
# 276: 'ab+bc'i    ABC    n    -    -
ok((not ("ABC" ~~ rx:i/ab+bc/)), 're_tests 386  (#496)');
# 277: 'ab+bc'i    ABQ    n    -    -
ok((not ("ABQ" ~~ rx:i/ab+bc/)), 're_tests 388  (#498)');
# 278: 'ab{1,}bc'i    ABQ    n    -    -
ok((not ("ABQ" ~~ rx:i/ab**{1...}bc/)), 're_tests 390  (#500)');
# 279: 'ab+bc'i    ABBBBC    y    $&    ABBBBC
is(("ABBBBC" ~~ rx:i/ab+bc/ && $<>), "ABBBBC", 're_tests 392/0 (#502)', :todo<feature>);
# 280: 'ab{1,}?bc'i    ABBBBC    y    $&    ABBBBC
is(("ABBBBC" ~~ rx:i/ab**{1...}?bc/ && $<>), "ABBBBC", 're_tests 394/0 (#504)', :todo<feature>);
# 281: 'ab{1,3}?bc'i    ABBBBC    y    $&    ABBBBC
is(("ABBBBC" ~~ rx:i/ab**{1..3}?bc/ && $<>), "ABBBBC", 're_tests 396/0 (#506)', :todo<feature>);
# 282: 'ab{3,4}?bc'i    ABBBBC    y    $&    ABBBBC
is(("ABBBBC" ~~ rx:i/ab**{3..4}?bc/ && $<>), "ABBBBC", 're_tests 398/0 (#508)', :todo<feature>);
# 283: 'ab{4,5}?bc'i    ABBBBC    n    -    -
ok((not ("ABBBBC" ~~ rx:i/ab**{4..5}?bc/)), 're_tests 400  (#510)');
# 284: 'ab??bc'i    ABBC    y    $&    ABBC
is(("ABBC" ~~ rx:i/ab??bc/ && $<>), "ABBC", 're_tests 402/0 (#512)', :todo<feature>);
# 285: 'ab??bc'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/ab??bc/ && $<>), "ABC", 're_tests 404/0 (#514)', :todo<feature>);
# 286: 'ab{0,1}?bc'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/ab**{0..1}?bc/ && $<>), "ABC", 're_tests 406/0 (#516)', :todo<feature>);
# 287: 'ab??bc'i    ABBBBC    n    -    -
ok((not ("ABBBBC" ~~ rx:i/ab??bc/)), 're_tests 408  (#518)');
# 288: 'ab??c'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/ab??c/ && $<>), "ABC", 're_tests 410/0 (#520)', :todo<feature>);
# 289: 'ab{0,1}?c'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/ab**{0..1}?c/ && $<>), "ABC", 're_tests 412/0 (#522)', :todo<feature>);
# 290: '^abc$'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/^abc$/ && $<>), "ABC", 're_tests 414/0 (#524)', :todo<feature>);
# 291: '^abc$'i    ABCC    n    -    -
ok((not ("ABCC" ~~ rx:i/^abc$/)), 're_tests 416  (#526)');
# 292: '^abc'i    ABCC    y    $&    ABC
is(("ABCC" ~~ rx:i/^abc/ && $<>), "ABC", 're_tests 418/0 (#528)', :todo<feature>);
# 293: '^abc$'i    AABC    n    -    -
ok((not ("AABC" ~~ rx:i/^abc$/)), 're_tests 420  (#530)');
# 294: 'abc$'i    AABC    y    $&    ABC
is(("AABC" ~~ rx:i/abc$/ && $<>), "ABC", 're_tests 422/0 (#532)', :todo<feature>);
# 295: '^'i    ABC    y    $&    
is(("ABC" ~~ rx:i/^/ && $<>), "", 're_tests 424/0 (#534)');
# 296: '$'i    ABC    y    $&    
is(("ABC" ~~ rx:i/$/ && $<>), "", 're_tests 426/0 (#536)');
# 297: 'a.c'i    ABC    y    $&    ABC
is(("ABC" ~~ rx:i/a\Nc/ && $<>), "ABC", 're_tests 428/0 (#538)', :todo<feature>);
# 298: 'a.c'i    AXC    y    $&    AXC
is(("AXC" ~~ rx:i/a\Nc/ && $<>), "AXC", 're_tests 430/0 (#540)', :todo<feature>);
# 299: 'a.*?c'i    AXYZC    y    $&    AXYZC
is(("AXYZC" ~~ rx:i/a\N*?c/ && $<>), "AXYZC", 're_tests 432/0 (#542)', :todo<feature>);
# 300: 'a.*c'i    AXYZD    n    -    -
ok((not ("AXYZD" ~~ rx:i/a\N*c/)), 're_tests 434  (#544)');
# 301: 'a[bc]d'i    ABC    n    -    -
ok((not ("ABC" ~~ rx:i/a<[bc]>d/)), 're_tests 436  (#546)');
# 302: 'a[bc]d'i    ABD    y    $&    ABD
is(("ABD" ~~ rx:i/a<[bc]>d/ && $<>), "ABD", 're_tests 438/0 (#548)', :todo<feature>);
# 303: 'a[b-d]e'i    ABD    n    -    -
ok((not ("ABD" ~~ rx:i/a<[b..d]>e/)), 're_tests 440  (#550)');
# 304: 'a[b-d]e'i    ACE    y    $&    ACE
is(("ACE" ~~ rx:i/a<[b..d]>e/ && $<>), "ACE", 're_tests 442/0 (#552)', :todo<feature>);
# 305: 'a[b-d]'i    AAC    y    $&    AC
is(("AAC" ~~ rx:i/a<[b..d]>/ && $<>), "AC", 're_tests 444/0 (#554)', :todo<feature>);
# 306: 'a[-b]'i    A-    y    $&    A-
is(("A-" ~~ rx:i/a<[\-b]>/ && $<>), "A-", 're_tests 446/0 (#556)', :todo<feature>);
# 307: 'a[b-]'i    A-    y    $&    A-
is(("A-" ~~ rx:i/a<[b\-]>/ && $<>), "A-", 're_tests 448/0 (#558)', :todo<feature>);
# 308: 'a[b-a]'i    -    c    -    Invalid [] range "b-a"
# -- SKIPPED - TESTS ERROR MESSAGE
# 309: 'a[]b'i    -    c    -    Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 310: 'a['i    -    c    -    Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 311: 'a]'i    A]    y    $&    A]
is(("A]" ~~ rx:i/a]/ && $<>), "A]", 're_tests 450/0 (#560)', :todo<feature>);
# 312: 'a[]]b'i    A]B    y    $&    A]B
is(("A]B" ~~ rx:i/a<[\]]>b/ && $<>), "A]B", 're_tests 452/0 (#562)', :todo<feature>);
# 313: 'a[^bc]d'i    AED    y    $&    AED
is(("AED" ~~ rx:i/a<-[bc]>d/ && $<>), "AED", 're_tests 454/0 (#564)', :todo<feature>);
# 314: 'a[^bc]d'i    ABD    n    -    -
ok((not ("ABD" ~~ rx:i/a<-[bc]>d/)), 're_tests 456  (#566)');
# 315: 'a[^-b]c'i    ADC    y    $&    ADC
is(("ADC" ~~ rx:i/a<-[\-b]>c/ && $<>), "ADC", 're_tests 458/0 (#568)', :todo<feature>);
# 316: 'a[^-b]c'i    A-C    n    -    -
ok((not ("A-C" ~~ rx:i/a<-[\-b]>c/)), 're_tests 460  (#570)', :todo<feature>);
# 317: 'a[^]b]c'i    A]C    n    -    -
ok((not ("A]C" ~~ rx:i/a<-[\]b]>c/)), 're_tests 462  (#572)', :todo<feature>);
# 318: 'a[^]b]c'i    ADC    y    $&    ADC
is(("ADC" ~~ rx:i/a<-[\]b]>c/ && $<>), "ADC", 're_tests 464/0 (#574)', :todo<feature>);
# 319: 'ab|cd'i    ABC    y    $&    AB
is(("ABC" ~~ rx:i/ab|cd/ && $<>), "AB", 're_tests 466/0 (#576)', :todo<feature>);
# 320: 'ab|cd'i    ABCD    y    $&    AB
is(("ABCD" ~~ rx:i/ab|cd/ && $<>), "AB", 're_tests 468/0 (#578)', :todo<feature>);
# 321: '()ef'i    DEF    y    $&-$0    EF-
is(("DEF" ~~ rx:i/(<?null>)ef/ && $<>), "EF", 're_tests 470/0 (#581)', :todo<feature>);
is(("DEF" ~~ rx:i/(<?null>)ef/ && $0), "", 're_tests 470/1 (#582)');
# 322: '*a'i    -    c    -    Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 323: '(*)b'i    -    c    -    Quantifier follows nothing
# -- SKIPPED - TESTS ERROR MESSAGE
# 324: '$b'i    B    n    -    -
ok((not ("B" ~~ rx:i/$b/)), 're_tests 472  (#584)');
# 325: 'a\'i    -    c    -    Search pattern not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
# 326: 'a\(b'i    A(B    y    $&-$0    A(B-
is(("A(B" ~~ rx:i/a\(b/ && $<>), "A(B", 're_tests 474/0 (#587)', :todo<feature>);
is(("A(B" ~~ rx:i/a\(b/ && $0), "", 're_tests 474/1 (#588)');
# 327: 'a\(*b'i    AB    y    $&    AB
is(("AB" ~~ rx:i/a\(*b/ && $<>), "AB", 're_tests 476/0 (#590)', :todo<feature>);
# 328: 'a\(*b'i    A((B    y    $&    A((B
is(("A((B" ~~ rx:i/a\(*b/ && $<>), "A((B", 're_tests 478/0 (#592)', :todo<feature>);
# 329: 'a\\b'i    A\B    y    $&    A\B
is(("A\B" ~~ rx:i/a\\b/ && $<>), "A\B", 're_tests 480/0 (#594)', :todo<feature>);
# 330: 'abc)'i    -    c    -    Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 331: '(abc'i    -    c    -    Unmatched (
# -- SKIPPED - TESTS ERROR MESSAGE
# 332: '((a))'i    ABC    y    $&-$0-$1    A-A-A
is(("ABC" ~~ rx:i/((a))/ && $<>), "A", 're_tests 482/0 (#598)', :todo<feature>);
is(("ABC" ~~ rx:i/((a))/ && $0), "A", 're_tests 482/1 (#599)', :todo<feature>);
is(("ABC" ~~ rx:i/((a))/ && $1), "A", 're_tests 482/2 (#600)', :todo<feature>);
# 333: '(a)b(c)'i    ABC    y    $&-$0-$1    ABC-A-C
is(("ABC" ~~ rx:i/(a)b(c)/ && $<>), "ABC", 're_tests 484/0 (#604)', :todo<feature>);
is(("ABC" ~~ rx:i/(a)b(c)/ && $0), "A", 're_tests 484/1 (#605)', :todo<feature>);
is(("ABC" ~~ rx:i/(a)b(c)/ && $1), "C", 're_tests 484/2 (#606)', :todo<feature>);
# 334: 'a+b+c'i    AABBABC    y    $&    ABC
is(("AABBABC" ~~ rx:i/a+b+c/ && $<>), "ABC", 're_tests 486/0 (#608)', :todo<feature>);
# 335: 'a{1,}b{1,}c'i    AABBABC    y    $&    ABC
is(("AABBABC" ~~ rx:i/a**{1...}b**{1...}c/ && $<>), "ABC", 're_tests 488/0 (#610)', :todo<feature>);
# 336: 'a**'i    -    c    -    Nested quantifiers
# -- SKIPPED - TESTS ERROR MESSAGE
# 337: 'a.+?c'i    ABCABC    y    $&    ABC
is(("ABCABC" ~~ rx:i/a\N+?c/ && $<>), "ABC", 're_tests 490/0 (#612)', :todo<feature>);
# 338: 'a.*?c'i    ABCABC    y    $&    ABC
is(("ABCABC" ~~ rx:i/a\N*?c/ && $<>), "ABC", 're_tests 492/0 (#614)', :todo<feature>);
# 339: 'a.{0,5}?c'i    ABCABC    y    $&    ABC
is(("ABCABC" ~~ rx:i/a\N**{0..5}?c/ && $<>), "ABC", 're_tests 494/0 (#616)', :todo<feature>);
# 340: '(a+|b)*'i    AB    y    $&-$0    AB-B
is(("AB" ~~ rx:i/(a+|b)*/ && $<>), "AB", 're_tests 496/0 (#619)', :todo<feature>);
is(("AB" ~~ rx:i/(a+|b)*/ && $0), "B", 're_tests 496/1 (#620)', :todo<feature>);
# 341: '(a+|b){0,}'i    AB    y    $&-$0    AB-B
is(("AB" ~~ rx:i/(a+|b)**{0...}/ && $<>), "AB", 're_tests 498/0 (#623)', :todo<feature>);
is(("AB" ~~ rx:i/(a+|b)**{0...}/ && $0), "B", 're_tests 498/1 (#624)', :todo<feature>);
# 342: '(a+|b)+'i    AB    y    $&-$0    AB-B
is(("AB" ~~ rx:i/(a+|b)+/ && $<>), "AB", 're_tests 500/0 (#627)', :todo<feature>);
is(("AB" ~~ rx:i/(a+|b)+/ && $0), "B", 're_tests 500/1 (#628)', :todo<feature>);
# 343: '(a+|b){1,}'i    AB    y    $&-$0    AB-B
is(("AB" ~~ rx:i/(a+|b)**{1...}/ && $<>), "AB", 're_tests 502/0 (#631)', :todo<feature>);
is(("AB" ~~ rx:i/(a+|b)**{1...}/ && $0), "B", 're_tests 502/1 (#632)', :todo<feature>);
# 344: '(a+|b)?'i    AB    y    $&-$0    A-A
is(("AB" ~~ rx:i/(a+|b)?/ && $<>), "A", 're_tests 504/0 (#635)', :todo<feature>);
is(("AB" ~~ rx:i/(a+|b)?/ && $0), "A", 're_tests 504/1 (#636)', :todo<feature>);
# 345: '(a+|b){0,1}'i    AB    y    $&-$0    A-A
is(("AB" ~~ rx:i/(a+|b)**{0..1}/ && $<>), "A", 're_tests 506/0 (#639)', :todo<feature>);
is(("AB" ~~ rx:i/(a+|b)**{0..1}/ && $0), "A", 're_tests 506/1 (#640)', :todo<feature>);
# 346: '(a+|b){0,1}?'i    AB    y    $&-$0    -
is(("AB" ~~ rx:i/(a+|b)**{0..1}?/ && $<>), "", 're_tests 508/0 (#643)');
is(("AB" ~~ rx:i/(a+|b)**{0..1}?/ && $0), "", 're_tests 508/1 (#644)');
# 347: ')('i    -    c    -    Unmatched )
# -- SKIPPED - TESTS ERROR MESSAGE
# 348: '[^ab]*'i    CDE    y    $&    CDE
is(("CDE" ~~ rx:i/<-[ab]>*/ && $<>), "CDE", 're_tests 510/0 (#646)');
# 349: 'abc'i        n    -    -
ok((not ("" ~~ rx:i/abc/)), 're_tests 512  (#648)');
# 350: 'a*'i        y    $&    
is(("" ~~ rx:i/a*/ && $<>), "", 're_tests 514/0 (#650)');
# 351: '([abc])*d'i    ABBBCD    y    $&-$0    ABBBCD-C
is(("ABBBCD" ~~ rx:i/(<[abc]>)*d/ && $<>), "ABBBCD", 're_tests 516/0 (#653)', :todo<feature>);
is(("ABBBCD" ~~ rx:i/(<[abc]>)*d/ && $0), "C", 're_tests 516/1 (#654)', :todo<feature>);
# 352: '([abc])*bcd'i    ABCD    y    $&-$0    ABCD-A
is(("ABCD" ~~ rx:i/(<[abc]>)*bcd/ && $<>), "ABCD", 're_tests 518/0 (#657)', :todo<feature>);
is(("ABCD" ~~ rx:i/(<[abc]>)*bcd/ && $0), "A", 're_tests 518/1 (#658)', :todo<feature>);
# 353: 'a|b|c|d|e'i    E    y    $&    E
is(("E" ~~ rx:i/a|b|c|d|e/ && $<>), "E", 're_tests 520/0 (#660)', :todo<feature>);
# 354: '(a|b|c|d|e)f'i    EF    y    $&-$0    EF-E
is(("EF" ~~ rx:i/(a|b|c|d|e)f/ && $<>), "EF", 're_tests 522/0 (#663)', :todo<feature>);
is(("EF" ~~ rx:i/(a|b|c|d|e)f/ && $0), "E", 're_tests 522/1 (#664)', :todo<feature>);
# 355: 'abcd*efg'i    ABCDEFG    y    $&    ABCDEFG
is(("ABCDEFG" ~~ rx:i/abcd*efg/ && $<>), "ABCDEFG", 're_tests 524/0 (#666)', :todo<feature>);
# 356: 'ab*'i    XABYABBBZ    y    $&    AB
is(("XABYABBBZ" ~~ rx:i/ab*/ && $<>), "AB", 're_tests 526/0 (#668)', :todo<feature>);
# 357: 'ab*'i    XAYABBBZ    y    $&    A
is(("XAYABBBZ" ~~ rx:i/ab*/ && $<>), "A", 're_tests 528/0 (#670)', :todo<feature>);
# 358: '(ab|cd)e'i    ABCDE    y    $&-$0    CDE-CD
is(("ABCDE" ~~ rx:i/(ab|cd)e/ && $<>), "CDE", 're_tests 530/0 (#673)', :todo<feature>);
is(("ABCDE" ~~ rx:i/(ab|cd)e/ && $0), "CD", 're_tests 530/1 (#674)', :todo<feature>);
# 359: '[abhgefdc]ij'i    HIJ    y    $&    HIJ
is(("HIJ" ~~ rx:i/<[abhgefdc]>ij/ && $<>), "HIJ", 're_tests 532/0 (#676)', :todo<feature>);
# 360: '^(ab|cd)e'i    ABCDE    n    x$0y    XY
# SKIPPED: script doesn't understand `x$0y' yet
# SKIPPED: script doesn't understand `x$0y' yet
# 361: '(abc|)ef'i    ABCDEF    y    $&-$0    EF-
is(("ABCDEF" ~~ rx:i/(abc|<?null>)ef/ && $<>), "EF", 're_tests 534/0 (#679)', :todo<feature>);
is(("ABCDEF" ~~ rx:i/(abc|<?null>)ef/ && $0), "", 're_tests 534/1 (#680)');
# 362: '(a|b)c*d'i    ABCD    y    $&-$0    BCD-B
is(("ABCD" ~~ rx:i/(a|b)c*d/ && $<>), "BCD", 're_tests 536/0 (#683)', :todo<feature>);
is(("ABCD" ~~ rx:i/(a|b)c*d/ && $0), "B", 're_tests 536/1 (#684)', :todo<feature>);
# 363: '(ab|ab*)bc'i    ABC    y    $&-$0    ABC-A
is(("ABC" ~~ rx:i/(ab|ab*)bc/ && $<>), "ABC", 're_tests 538/0 (#687)', :todo<feature>);
is(("ABC" ~~ rx:i/(ab|ab*)bc/ && $0), "A", 're_tests 538/1 (#688)', :todo<feature>);
# 364: 'a([bc]*)c*'i    ABC    y    $&-$0    ABC-BC
is(("ABC" ~~ rx:i/a(<[bc]>*)c*/ && $<>), "ABC", 're_tests 540/0 (#691)', :todo<feature>);
is(("ABC" ~~ rx:i/a(<[bc]>*)c*/ && $0), "BC", 're_tests 540/1 (#692)', :todo<feature>);
# 365: 'a([bc]*)(c*d)'i    ABCD    y    $&-$0-$1    ABCD-BC-D
is(("ABCD" ~~ rx:i/a(<[bc]>*)(c*d)/ && $<>), "ABCD", 're_tests 542/0 (#696)', :todo<feature>);
is(("ABCD" ~~ rx:i/a(<[bc]>*)(c*d)/ && $0), "BC", 're_tests 542/1 (#697)', :todo<feature>);
is(("ABCD" ~~ rx:i/a(<[bc]>*)(c*d)/ && $1), "D", 're_tests 542/2 (#698)', :todo<feature>);
# 366: 'a([bc]+)(c*d)'i    ABCD    y    $&-$0-$1    ABCD-BC-D
is(("ABCD" ~~ rx:i/a(<[bc]>+)(c*d)/ && $<>), "ABCD", 're_tests 544/0 (#702)', :todo<feature>);
is(("ABCD" ~~ rx:i/a(<[bc]>+)(c*d)/ && $0), "BC", 're_tests 544/1 (#703)', :todo<feature>);
is(("ABCD" ~~ rx:i/a(<[bc]>+)(c*d)/ && $1), "D", 're_tests 544/2 (#704)', :todo<feature>);
# 367: 'a([bc]*)(c+d)'i    ABCD    y    $&-$0-$1    ABCD-B-CD
is(("ABCD" ~~ rx:i/a(<[bc]>*)(c+d)/ && $<>), "ABCD", 're_tests 546/0 (#708)', :todo<feature>);
is(("ABCD" ~~ rx:i/a(<[bc]>*)(c+d)/ && $0), "B", 're_tests 546/1 (#709)', :todo<feature>);
is(("ABCD" ~~ rx:i/a(<[bc]>*)(c+d)/ && $1), "CD", 're_tests 546/2 (#710)', :todo<feature>);
# 368: 'a[bcd]*dcdcde'i    ADCDCDE    y    $&    ADCDCDE
is(("ADCDCDE" ~~ rx:i/a<[bcd]>*dcdcde/ && $<>), "ADCDCDE", 're_tests 548/0 (#712)', :todo<feature>);
# 369: 'a[bcd]+dcdcde'i    ADCDCDE    n    -    -
ok((not ("ADCDCDE" ~~ rx:i/a<[bcd]>+dcdcde/)), 're_tests 550  (#714)');
# 370: '(ab|a)b*c'i    ABC    y    $&-$0    ABC-AB
is(("ABC" ~~ rx:i/(ab|a)b*c/ && $<>), "ABC", 're_tests 552/0 (#717)', :todo<feature>);
is(("ABC" ~~ rx:i/(ab|a)b*c/ && $0), "AB", 're_tests 552/1 (#718)', :todo<feature>);
# 371: '((a)(b)c)(d)'i    ABCD    y    $0-$1-$2-$3    ABC-A-B-D
# SKIPPED: script doesn't understand `$0-$1-$2-$3' yet
# SKIPPED: script doesn't understand `$0-$1-$2-$3' yet
# 372: '[a..zA..Z_][a..zA..Z0..9_]*'i    ALPHA    y    $&    ALPHA
is(("ALPHA" ~~ rx:i/<[a..zA..Z_]><[a..zA..Z0..9_]>*/ && $<>), "ALPHA", 're_tests 554/0 (#720)');
# 373: '^a(bc+|b[eh])g|.h$'i    ABH    y    $&-$0    BH-
is(("ABH" ~~ rx:i/^a(bc+|b<[eh]>)g|\Nh$/ && $<>), "BH", 're_tests 556/0 (#723)', :todo<feature>);
is(("ABH" ~~ rx:i/^a(bc+|b<[eh]>)g|\Nh$/ && $0), "", 're_tests 556/1 (#724)');
# 374: '(bc+d$|ef*g.|h?i(j|k))'i    EFFGZ    y    $&-$0-$1    EFFGZ-EFFGZ-
is(("EFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $<>), "EFFGZ", 're_tests 558/0 (#728)', :todo<feature>);
is(("EFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $0), "EFFGZ", 're_tests 558/1 (#729)', :todo<feature>);
is(("EFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $1), "", 're_tests 558/2 (#730)');
# 375: '(bc+d$|ef*g.|h?i(j|k))'i    IJ    y    $&-$0-$1    IJ-IJ-J
is(("IJ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $<>), "IJ", 're_tests 560/0 (#734)', :todo<feature>);
is(("IJ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $0), "IJ", 're_tests 560/1 (#735)', :todo<feature>);
is(("IJ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $1), "J", 're_tests 560/2 (#736)', :todo<feature>);
# 376: '(bc+d$|ef*g.|h?i(j|k))'i    EFFG    n    -    -
ok((not ("EFFG" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/)), 're_tests 562  (#738)');
# 377: '(bc+d$|ef*g.|h?i(j|k))'i    BCDD    n    -    -
ok((not ("BCDD" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/)), 're_tests 564  (#740)');
# 378: '(bc+d$|ef*g.|h?i(j|k))'i    REFFGZ    y    $&-$0-$1    EFFGZ-EFFGZ-
is(("REFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $<>), "EFFGZ", 're_tests 566/0 (#744)', :todo<feature>);
is(("REFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $0), "EFFGZ", 're_tests 566/1 (#745)', :todo<feature>);
is(("REFFGZ" ~~ rx:i/(bc+d$|ef*g\N|h?i(j|k))/ && $1), "", 're_tests 566/2 (#746)');
# 379: '((((((((((a))))))))))'i    A    y    $00    A
#is(("A" ~~ rx:i/((((((((((a))))))))))/ && $00), "A", 're_tests 568/10 (#748)');
# 380: '((((((((((a))))))))))\10'i    AA    y    $&    AA
#is(("AA" ~~ rx:i/((((((((((a))))))))))$00/ && $<>), "AA", 're_tests 570/0 (#750)');
# 381: '((((((((((a))))))))))${bang}'i    AA    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 382: '((((((((((a))))))))))${bang}'i    A!    y    $&    A!
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 383: '(((((((((a)))))))))'i    A    y    $&    A
is(("A" ~~ rx:i/(((((((((a)))))))))/ && $<>), "A", 're_tests 574/0 (#754)', :todo<bug>);
# 384: '(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))'i    A    y    $0    A
is(("A" ~~ rx:i/[[[[[[[[[(a)]]]]]]]]]/ && $0), "A", 're_tests 576/1 (#756)', :todo<bug>);
# 385: '(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))'i    C    y    $0    C
is(("C" ~~ rx:i/[[[[[[[[[(a|b|c)]]]]]]]]]/ && $0), "C", 're_tests 578/1 (#758)', :todo<bug>);
# 386: 'multiple words of text'i    UH-UH    n    -    -
ok((not ("UH-UH" ~~ rx:i/multiple<sp>words<sp>of<sp>text/)), 're_tests 580  (#760)');
# 387: 'multiple words'i    MULTIPLE WORDS, YEAH    y    $&    MULTIPLE WORDS
is(("MULTIPLE WORDS, YEAH" ~~ rx:i/multiple<sp>words/ && $<>), "MULTIPLE WORDS", 're_tests 582/0 (#762)', :todo<bug>);
# 388: '(.*)c(.*)'i    ABCDE    y    $&-$0-$1    ABCDE-AB-DE
is(("ABCDE" ~~ rx:i/(\N*)c(\N*)/ && $<>), "ABCDE", 're_tests 584/0 (#766)', :todo<bug>);
is(("ABCDE" ~~ rx:i/(\N*)c(\N*)/ && $0), "AB", 're_tests 584/1 (#767)', :todo<bug>);
is(("ABCDE" ~~ rx:i/(\N*)c(\N*)/ && $1), "DE", 're_tests 584/2 (#768)', :todo<bug>);
# 389: '\((.*), (.*)\)'i    (A, B)    y    ($1, $0)    (B, A)
# SKIPPED: script doesn't understand `($1, $0)' yet
# SKIPPED: script doesn't understand `($1, $0)' yet
# 390: '[k]'i    AB    n    -    -
ok((not ("AB" ~~ rx:i/<[k]>/)), 're_tests 586  (#770)');
# 391: 'abcd'i    ABCD    y    $&-\$&-\\$&    ABCD-$&-\ABCD
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# SKIPPED: script doesn't understand `$&-\$&-\\$&' yet
# 392: 'a(bc)d'i    ABCD    y    $0-\$0-\\$0    BC-$0-\BC
# SKIPPED: script doesn't understand `$0-\$0-\\$0' yet
# SKIPPED: script doesn't understand `$0-\$0-\\$0' yet
# 393: 'a[-]?c'i    AC    y    $&    AC
skip 1, "I'm not sure what this test should be.";
#is(("AC" ~~ rx:i/a<[..]>?c/ && $<>), "AC", 're_tests 588/0 (#772)');
# 394: '(abc)\1'i    ABCABC    y    $0    ABC
is(("ABCABC" ~~ rx:i/(abc)$0/ && $0), "ABC", 're_tests 590/1 (#774)', :todo<feature>);
# 395: '([a-c]*)\1'i    ABCABC    y    $0    ABC
is(("ABCABC" ~~ rx:i/(<[a..c]>*)$0/ && $0), "ABC", 're_tests 592/1 (#776)', :todo<feature>);
# 396: a(?!b).    abad    y    $&    ad
is(("abad" ~~ /a<!before b>\N/ && $<>), "ad", 're_tests 594/0 (#778)', :todo<feature>);
# 397: a(?=d).    abad    y    $&    ad
is(("abad" ~~ /a<before d>\N/ && $<>), "ad", 're_tests 596/0 (#780)', :todo<feature>);
# 398: a(?=c|d).    abad    y    $&    ad
is(("abad" ~~ /a<before c|d>\N/ && $<>), "ad", 're_tests 598/0 (#782)', :todo<feature>);
# 399: a(?:b|c|d)(.)    ace    y    $0    e
is(("ace" ~~ /a[b|c|d](\N)/ && $0), "e", 're_tests 600/1 (#784)');
# 400: a(?:b|c|d)*(.)    ace    y    $0    e
is(("ace" ~~ /a[b|c|d]*(\N)/ && $0), "e", 're_tests 602/1 (#786)');
# 401: a(?:b|c|d)+?(.)    ace    y    $0    e
is(("ace" ~~ /a[b|c|d]+?(\N)/ && $0), "e", 're_tests 604/1 (#788)');
# 402: a(?:b|c|d)+?(.)    acdbcdbe    y    $0    d
is(("acdbcdbe" ~~ /a[b|c|d]+?(\N)/ && $0), "d", 're_tests 606/1 (#790)');
# 403: a(?:b|c|d)+(.)    acdbcdbe    y    $0    e
is(("acdbcdbe" ~~ /a[b|c|d]+(\N)/ && $0), "e", 're_tests 608/1 (#792)');
# 404: a(?:b|c|d){2}(.)    acdbcdbe    y    $0    b
is(("acdbcdbe" ~~ /a[b|c|d]**{2}(\N)/ && $0), "b", 're_tests 610/1 (#794)');
# 405: a(?:b|c|d){4,5}(.)    acdbcdbe    y    $0    b
is(("acdbcdbe" ~~ /a[b|c|d]**{4..5}(\N)/ && $0), "b", 're_tests 612/1 (#796)');
# 406: a(?:b|c|d){4,5}?(.)    acdbcdbe    y    $0    d
is(("acdbcdbe" ~~ /a[b|c|d]**{4..5}?(\N)/ && $0), "d", 're_tests 614/1 (#798)');
# 407: ((foo)|(bar))*    foobar    y    $0-$1-$2    bar-foo-bar
# SKIPPED: script doesn't understand `$0-$1-$2' yet
# SKIPPED: script doesn't understand `$0-$1-$2' yet
# 408: :(?:    -    c    -    Sequence (? incomplete
# -- SKIPPED - TESTS ERROR MESSAGE
# 409: a(?:b|c|d){6,7}(.)    acdbcdbe    y    $0    e
is(("acdbcdbe" ~~ /a[b|c|d]**{6..7}(\N)/ && $0), "e", 're_tests 616/1 (#800)');
# 410: a(?:b|c|d){6,7}?(.)    acdbcdbe    y    $0    e
is(("acdbcdbe" ~~ /a[b|c|d]**{6..7}?(\N)/ && $0), "e", 're_tests 618/1 (#802)');
# 411: a(?:b|c|d){5,6}(.)    acdbcdbe    y    $0    e
is(("acdbcdbe" ~~ /a[b|c|d]**{5..6}(\N)/ && $0), "e", 're_tests 620/1 (#804)');
# 412: a(?:b|c|d){5,6}?(.)    acdbcdbe    y    $0    b
is(("acdbcdbe" ~~ /a[b|c|d]**{5..6}?(\N)/ && $0), "b", 're_tests 622/1 (#806)');
# 413: a(?:b|c|d){5,7}(.)    acdbcdbe    y    $0    e
is(("acdbcdbe" ~~ /a[b|c|d]**{5..7}(\N)/ && $0), "e", 're_tests 624/1 (#808)');
# 414: a(?:b|c|d){5,7}?(.)    acdbcdbe    y    $0    b
is(("acdbcdbe" ~~ /a[b|c|d]**{5..7}?(\N)/ && $0), "b", 're_tests 626/1 (#810)');
# 415: a(?:b|(c|e){1,2}?|d)+?(.)    ace    y    $0$1    ce
# SKIPPED: script doesn't understand `$0$1' yet
# SKIPPED: script doesn't understand `$0$1' yet
# 416: ^(.+)?B    AB    y    $0    A
is(("AB" ~~ /^(\N+)?B/ && $0), "A", 're_tests 628/1 (#812)');
# 417: ^([^a..z])|(\^)$    .    y    $0    .
is(("." ~~ /^(<-[a..z]>)|(\^)$/ && $0), ".", 're_tests 630/1 (#814)');
# 418: ^[<>]&    <&OUT    y    $&    <&
is(("<&OUT" ~~ /^<[<>]>\&/ && $<>), "<&", 're_tests 632/0 (#816)');
# 419: ^(a\1?){4}$    aaaaaaaaaa    y    $0    aaaa
is(("aaaaaaaaaa" ~~ /^(a$0?)**{4}$/ && $0), "aaaa", 're_tests 634/1 (#818)', :todo<feature>);
# 420: ^(a\1?){4}$    aaaaaaaaa    n    -    -
ok((not ("aaaaaaaaa" ~~ /^(a$0?)**{4}$/)), 're_tests 636  (#820)');
# 421: ^(a\1?){4}$    aaaaaaaaaaa    n    -    -
ok((not ("aaaaaaaaaaa" ~~ /^(a$0?)**{4}$/)), 're_tests 638  (#822)');
# 422: ^(a(?(1)\1)){4}$    aaaaaaaaaa    y    $0    aaaa
is(("aaaaaaaaaa" ~~ /^(a[ <(defined $0)> :: $0 ])**{4}$/ && $0), "aaaa", 're_tests 640/1 (#824)', :todo<feature>);
# 423: ^(a(?(1)\1)){4}$    aaaaaaaaa    n    -    -
ok((not ("aaaaaaaaa" ~~ /^(a[ <(defined $0)> :: $0 ])**{4}$/)), 're_tests 642  (#826)');
# 424: ^(a(?(1)\1)){4}$    aaaaaaaaaaa    n    -    -
ok((not ("aaaaaaaaaaa" ~~ /^(a[ <(defined $0)> :: $0 ])**{4}$/)), 're_tests 644  (#828)');
# 425: ((a{4})+)    aaaaaaaaa    y    $0    aaaaaaaa
is(("aaaaaaaaa" ~~ /((a**{4})+)/ && $0), "aaaaaaaa", 're_tests 646/1 (#830)');
# 426: (((aa){2})+)    aaaaaaaaaa    y    $0    aaaaaaaa
is(("aaaaaaaaaa" ~~ /(((aa)**{2})+)/ && $0), "aaaaaaaa", 're_tests 648/1 (#832)');
# 427: (((a{2}){2})+)    aaaaaaaaaa    y    $0    aaaaaaaa
is(("aaaaaaaaaa" ~~ /(((a**{2})**{2})+)/ && $0), "aaaaaaaa", 're_tests 650/1 (#834)');
# 428: (?:(f)(o)(o)|(b)(a)(r))*    foobar    y    $0:$1:$2:$3:$4:$5    f:o:o:b:a:r
# SKIPPED: script doesn't understand `$0:$1:$2:$3:$4:$5' yet
# SKIPPED: script doesn't understand `$0:$1:$2:$3:$4:$5' yet
# 429: (?<=a)b    ab    y    $&    b
is(("ab" ~~ /<after a>b/ && $<>), "b", 're_tests 652/0 (#836)', :todo<feature>);
# 430: (?<=a)b    cb    n    -    -
ok((not ("cb" ~~ /<after a>b/)), 're_tests 654  (#838)');
# 431: (?<=a)b    b    n    -    -
ok((not ("b" ~~ /<after a>b/)), 're_tests 656  (#840)');
# 432: (?<!c)b    ab    y    $&    b
is(("ab" ~~ /<!after c>b/ && $<>), "b", 're_tests 658/0 (#842)', :todo<feature>);
# 433: (?<!c)b    cb    n    -    -
ok((not ("cb" ~~ /<!after c>b/)), 're_tests 660  (#844)');
# 434: (?<!c)b    b    y    -    -
ok(("b" ~~ /<!after c>b/), 're_tests 662  (#846)', :todo<feature>);
# 435: (?<!c)b    b    y    $&    b
is(("b" ~~ /<!after c>b/ && $<>), "b", 're_tests 664/0 (#848)', :todo<feature>);
# 436: (?<%)b    -    c    -    Sequence (?<%...) not recognized
# -- SKIPPED - TESTS ERROR MESSAGE
# 437: (?:..)*a    aba    y    $&    aba
is(("aba" ~~ /[\N\N]*a/ && $<>), "aba", 're_tests 666/0 (#850)');
# 438: (?:..)*?a    aba    y    $&    a
is(("aba" ~~ /[\N\N]*?a/ && $<>), "a", 're_tests 668/0 (#852)');
# 439: ^(?:b|a(?=(.)))*\1    abc    y    $&    ab
is(("abc" ~~ /^[b|a<before (\N)>]*$0/ && $<>), "ab", 're_tests 670/0 (#854)', :todo<feature>);
# 440: ^(){3,5}    abc    y    a$0    a
# SKIPPED: script doesn't understand `a$0' yet
# SKIPPED: script doesn't understand `a$0' yet
# 441: ^(a+)*ax    aax    y    $0    a
is(("aax" ~~ /^(a+)*ax/ && $0), "a", 're_tests 672/1 (#856)');
# 442: ^((a|b)+)*ax    aax    y    $0    a
is(("aax" ~~ /^((a|b)+)*ax/ && $0), "a", 're_tests 674/1 (#858)');
# 443: ^((a|bc)+)*ax    aax    y    $0    a
is(("aax" ~~ /^((a|bc)+)*ax/ && $0), "a", 're_tests 676/1 (#860)');
# 444: (a|x)*ab    cab    y    y$0    y
# SKIPPED: script doesn't understand `y$0' yet
# SKIPPED: script doesn't understand `y$0' yet
# 445: (a)*ab    cab    y    y$0    y
# SKIPPED: script doesn't understand `y$0' yet
# SKIPPED: script doesn't understand `y$0' yet
# 446: (?:(?i)a)b    ab    y    $&    ab
is(("ab" ~~ /[:i a]b/ && $<>), "ab", 're_tests 678/0 (#862)');
# 447: ((?i)a)b    ab    y    $&:$0    ab:a
is(("ab" ~~ /(:i a)b/ && $<>), "ab", 're_tests 680/0 (#865)');
is(("ab" ~~ /(:i a)b/ && $0), "a", 're_tests 680/1 (#866)');
# 448: (?:(?i)a)b    Ab    y    $&    Ab
is(("Ab" ~~ /[:i a]b/ && $<>), "Ab", 're_tests 682/0 (#868)', :todo<feature>);
# 449: ((?i)a)b    Ab    y    $&:$0    Ab:A
is(("Ab" ~~ /(:i a)b/ && $<>), "Ab", 're_tests 684/0 (#871)', :todo<feature>);
is(("Ab" ~~ /(:i a)b/ && $0), "A", 're_tests 684/1 (#872)', :todo<feature>);
# 450: (?:(?i)a)b    aB    n    -    -
ok((not ("aB" ~~ /[:i a]b/)), 're_tests 686  (#874)');
# 451: ((?i)a)b    aB    n    -    -
ok((not ("aB" ~~ /(:i a)b/)), 're_tests 688  (#876)');
# 452: (?i:a)b    ab    y    $&    ab
is(("ab" ~~ /[:i a]b/ && $<>), "ab", 're_tests 690/0 (#878)');
# 453: ((?i:a))b    ab    y    $&:$0    ab:a
is(("ab" ~~ /([:i a])b/ && $<>), "ab", 're_tests 692/0 (#881)');
is(("ab" ~~ /([:i a])b/ && $0), "a", 're_tests 692/1 (#882)');
# 454: (?i:a)b    Ab    y    $&    Ab
is(("Ab" ~~ /[:i a]b/ && $<>), "Ab", 're_tests 694/0 (#884)', :todo<feature>);
# 455: ((?i:a))b    Ab    y    $&:$0    Ab:A
is(("Ab" ~~ /([:i a])b/ && $<>), "Ab", 're_tests 696/0 (#887)', :todo<feature>);
is(("Ab" ~~ /([:i a])b/ && $0), "A", 're_tests 696/1 (#888)', :todo<feature>);
# 456: (?i:a)b    aB    n    -    -
ok((not ("aB" ~~ /[:i a]b/)), 're_tests 698  (#890)');
# 457: ((?i:a))b    aB    n    -    -
ok((not ("aB" ~~ /([:i a])b/)), 're_tests 700  (#892)');
# 458: '(?:(?-i)a)b'i    ab    y    $&    ab
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 459: '((?-i)a)b'i    ab    y    $&:$0    ab:a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 460: '(?:(?-i)a)b'i    aB    y    $&    aB
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 461: '((?-i)a)b'i    aB    y    $&:$0    aB:a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 462: '(?:(?-i)a)b'i    Ab    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 463: '((?-i)a)b'i    Ab    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 464: '(?:(?-i)a)b'i    aB    y    $&    aB
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 465: '((?-i)a)b'i    aB    y    $0    a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 466: '(?:(?-i)a)b'i    AB    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 467: '((?-i)a)b'i    AB    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 468: '(?-i:a)b'i    ab    y    $&    ab
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 469: '((?-i:a))b'i    ab    y    $&:$0    ab:a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 470: '(?-i:a)b'i    aB    y    $&    aB
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 471: '((?-i:a))b'i    aB    y    $&:$0    aB:a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 472: '(?-i:a)b'i    Ab    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 473: '((?-i:a))b'i    Ab    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 474: '(?-i:a)b'i    aB    y    $&    aB
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 475: '((?-i:a))b'i    aB    y    $0    a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 476: '(?-i:a)b'i    AB    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 477: '((?-i:a))b'i    AB    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 478: '((?-i:a.))b'i    a\nB    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 479: '((?s-i:a.))b'i    a\nB    y    $0    a\n
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 480: '((?s-i:a.))b'i    B\nB    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 481: (?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))    cabbbb    y    $&    cabbbb
is(("cabbbb" ~~ /[c|d][][a[][b][b[]][b[][b]]]/ && $<>), "cabbbb", 're_tests 725/0 (#921)', :todo<bug>);
# 482: (?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))    caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb    y    $&    caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
is(("caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" ~~ /[c|d][][aaaaaaaa[][bbbbbbbb][bbbbbbbb[]][bbbbbbbb[][bbbbbbbb]]]/ && $<>), "caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", 're_tests 727/0 (#923)', :todo<bug>);
# 483: '(ab)\d\1'i    Ab4ab    y    $0    Ab
is(("Ab4ab" ~~ rx:i/(ab)\d$0/ && $0), "Ab", 're_tests 729/1 (#925)', :todo<bug>);
# 484: '(ab)\d\1'i    ab4Ab    y    $0    ab
is(("ab4Ab" ~~ rx:i/(ab)\d$0/ && $0), "ab", 're_tests 731/1 (#927)', :todo<bug>);
# 485: foo\w*\d{4}baz    foobar1234baz    y    $&    foobar1234baz
is(("foobar1234baz" ~~ /foo\w*\d**{4}baz/ && $<>), "foobar1234baz", 're_tests 733/0 (#929)');
# 486: a(?{})b    cabd    y    $&    ab
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 487: a(?{)b    -    c    -    Sequence (?{...}) not terminated or not {}-balanced
# -- SKIPPED - TESTS ERROR MESSAGE
# 488: a(?{{})b    -    c    -    Sequence (?{...}) not terminated or not {}-balanced
# -- SKIPPED - TESTS ERROR MESSAGE
# 489: a(?{}})b    -    c    -    
# -- SKIPPED - TESTS ERROR MESSAGE
# 490: a(?{"{"})b    -    c    -    Sequence (?{...}) not terminated or not {}-balanced
# -- SKIPPED - TESTS ERROR MESSAGE
# 491: a(?{"\{"})b    cabd    y    $&    ab
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 492: a(?{"{"}})b    -    c    -    Unmatched right curly bracket
# -- SKIPPED - TESTS ERROR MESSAGE
# 493: a(?{$bl="\{"}).b    caxbd    y    $bl    {
# SKIPPED: script doesn't understand `$bl' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 494: x(~~)*(?:(?:F)?)?    x~~    y    -    -
ok(("x~~" ~~ /x(~~)*[[F]?]?/), 're_tests 737  (#933)');
# 495: ^a(?#xxx){3}c    aaac    y    $&    aaac
# -- SKIPPED - p5re_to_p6rule doesn't support `(?#...' yet
# 496: '^a (?#xxx) (?#yyy) {3}c'x    aaac    y    $&    aaac
# -- SKIPPED - p5re_to_p6rule doesn't support `(?#...' yet
# 497: (?<![cd])b    dbcb    n    -    -
ok((not ("dbcb" ~~ /<!after <[cd]>>b/)), 're_tests 741  (#937)');
# 498: (?<![cd])[ab]    dbaacb    y    $&    a
is(("dbaacb" ~~ /<!after <[cd]>><[ab]>/ && $<>), "a", 're_tests 743/0 (#939)', :todo<feature>);
# 499: (?<!(c|d))b    dbcb    n    -    -
ok((not ("dbcb" ~~ /<!after (c|d)>b/)), 're_tests 745  (#941)');
# 500: (?<!(c|d))[ab]    dbaacb    y    $&    a
is(("dbaacb" ~~ /<!after (c|d)><[ab]>/ && $<>), "a", 're_tests 747/0 (#943)', :todo<feature>);
# 501: (?<!cd)[ab]    cdaccb    y    $&    b
is(("cdaccb" ~~ /<!after cd><[ab]>/ && $<>), "b", 're_tests 749/0 (#945)', :todo<feature>);
# 502: ^(?:a?b?)*$    a--    n    -    -
#ok((not ("a--" ~~ /^[a?b?]*$/)), 're_tests 751  (#947)'); # XXX PGE BUG
# 503: ((?s)^a(.))((?m)^b$)    a\nb\nc\n    y    $0;$1;$2    a\n;\n;b
# SKIPPED: script doesn't understand `$0;$1;$2' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 504: ((?m)^b$)    a\nb\nc\n    y    $0    b
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 505: (?m)^b    a\nb\n    y    $&    b
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 506: (?m)^(b)    a\nb\n    y    $0    b
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 507: ((?m)^b)    a\nb\n    y    $0    b
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 508: \n((?m)^b)    a\nb\n    y    $0    b
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 509: ((?s).)c(?!.)    a\nb\nc\n    y    $0    \n
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 510: ((?s).)c(?!.)    a\nb\nc\n    y    $0:$&    \n:\nc
# SKIPPED: script doesn't understand `$0:$&' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 511: ((?s)b.)c(?!.)    a\nb\nc\n    y    $0    b\n
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 512: ((?s)b.)c(?!.)    a\nb\nc\n    y    $0:$&    b\n:b\nc
# SKIPPED: script doesn't understand `$0:$&' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?s...' yet
# 513: ^b    a\nb\nc\n    n    -    -
ok((not ("a\nb\nc\n" ~~ /^b/)), 're_tests 760  (#956)');
# 514: ()^b    a\nb\nc\n    n    -    -
ok((not ("a\nb\nc\n" ~~ /(<?null>)^b/)), 're_tests 762  (#958)');
# 515: ((?m)^b)    a\nb\nc\n    y    $0    b
# -- SKIPPED - p5re_to_p6rule doesn't support `(?m)' yet
# 516: (?(1)a|b)    a    n    -    -
ok((not ("a" ~~ /[ <(defined $0)> :: a|b ]/)), 're_tests 765  (#961)');
# 517: (?(1)b|a)    a    y    $&    a
is(("a" ~~ /[ <(defined $0)> :: b|a ]/ && $<>), "a", 're_tests 767/0 (#963)', :todo<feature>);
# 518: (x)?(?(1)a|b)    a    n    -    -
ok((not ("a" ~~ /(x)?[ <(defined $0)> :: a|b ]/)), 're_tests 769  (#965)');
# 519: (x)?(?(1)b|a)    a    y    $&    a
is(("a" ~~ /(x)?[ <(defined $0)> :: b|a ]/ && $<>), "a", 're_tests 771/0 (#967)', :todo<feature>);
# 520: ()?(?(1)b|a)    a    y    $&    a
is(("a" ~~ /(<?null>)?[ <(defined $0)> :: b|a ]/ && $<>), "a", 're_tests 773/0 (#969)', :todo<feature>);
# 521: ()(?(1)b|a)    a    n    -    -
ok((not ("a" ~~ /(<?null>)[ <(defined $0)> :: b|a ]/)), 're_tests 775  (#971)');
# 522: ()?(?(1)a|b)    a    y    $&    a
is(("a" ~~ /(<?null>)?[ <(defined $0)> :: a|b ]/ && $<>), "a", 're_tests 777/0 (#973)', :todo<feature>);
# 523: ^(\()?blah(?(1)(\)))$    (blah)    y    $1    )
is(("(blah)" ~~ /^(\()?blah[ <(defined $0)> :: (\)) ]$/ && $1), ")", 're_tests 779/2 (#975)', :todo<feature>);
# 524: ^(\()?blah(?(1)(\)))$    blah    y    ($1)    ()
# SKIPPED: script doesn't understand `($1)' yet
# SKIPPED: script doesn't understand `($1)' yet
# 525: ^(\()?blah(?(1)(\)))$    blah)    n    -    -
ok((not ("blah)" ~~ /^(\()?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 781  (#977)');
# 526: ^(\()?blah(?(1)(\)))$    (blah    n    -    -
ok((not ("(blah" ~~ /^(\()?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 783  (#979)');
# 527: ^(\(+)?blah(?(1)(\)))$    (blah)    y    $1    )
is(("(blah)" ~~ /^(\(+)?blah[ <(defined $0)> :: (\)) ]$/ && $1), ")", 're_tests 785/2 (#981)', :todo<feature>);
# 528: ^(\(+)?blah(?(1)(\)))$    blah    y    ($1)    ()
# SKIPPED: script doesn't understand `($1)' yet
# SKIPPED: script doesn't understand `($1)' yet
# 529: ^(\(+)?blah(?(1)(\)))$    blah)    n    -    -
ok((not ("blah)" ~~ /^(\(+)?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 787  (#983)');
# 530: ^(\(+)?blah(?(1)(\)))$    (blah    n    -    -
ok((not ("(blah" ~~ /^(\(+)?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 789  (#985)');
# 531: (?(1?)a|b)    a    c    -    Switch condition not recognized
# -- SKIPPED - TESTS ERROR MESSAGE
# 532: (?(1)a|b|c)    a    c    -    Switch (?(condition)... contains too many branches
# -- SKIPPED - TESTS ERROR MESSAGE
# 533: (?(?{0})a|b)    a    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 534: (?(?{0})b|a)    a    y    $&    a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 535: (?(?{1})b|a)    a    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 536: (?(?{1})a|b)    a    y    $&    a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 537: (?(?!a)a|b)    a    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 538: (?(?!a)b|a)    a    y    $&    a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 539: (?(?=a)b|a)    a    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 540: (?(?=a)a|b)    a    y    $&    a
# -- SKIPPED - p5re_to_p6rule doesn't support `(?(...' yet
# 541: (?=(a+?))(\1ab)    aaab    y    $1    aab
is(("aaab" ~~ /<before (a+?)>($0ab)/ && $1), "aab", 're_tests 799/2 (#995)', :todo<feature>);
# 542: ^(?=(a+?))\1ab    aaab    n    -    -
ok((not ("aaab" ~~ /^<before (a+?)>$0ab/)), 're_tests 801  (#997)');
# 543: (\w+:)+    one:    y    $0    one:
is(("one:" ~~ /(\w+\:)+/ && $0), "one:", 're_tests 803/1 (#999)');
# 544: $(?<=^(a))    a    y    $0    a
is(("a" ~~ /$<after ^(a)>/ && $0), "a", 're_tests 805/1 (#1001)', :todo<feature>);
# 545: (?=(a+?))(\1ab)    aaab    y    $1    aab
is(("aaab" ~~ /<before (a+?)>($0ab)/ && $1), "aab", 're_tests 807/2 (#1003)', :todo<feature>);
# 546: ^(?=(a+?))\1ab    aaab    n    -    -
ok((not ("aaab" ~~ /^<before (a+?)>$0ab/)), 're_tests 809  (#1005)');
# 547: ([\w:]+::)?(\w+)$    abcd:    n    -    -
ok((not ("abcd:" ~~ /(<[\w:]>+\:\:)?(\w+)$/)), 're_tests 811  (#1007)');
# 548: ([\w:]+::)?(\w+)$    abcd    y    $0-$1    -abcd
is(("abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $0), "", 're_tests 813/1 (#1010)');
is(("abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $1), "abcd", 're_tests 813/2 (#1011)');
# 549: ([\w:]+::)?(\w+)$    xy:z:::abcd    y    $0-$1    xy:z:::-abcd
is(("xy:z:::abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $0), "xy:z:::", 're_tests 815/1 (#1014)', :todo<bug>);
is(("xy:z:::abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $1), "abcd", 're_tests 815/2 (#1015)');
# 550: ^[^bcd]*(c+)    aexycd    y    $0    c
is(("aexycd" ~~ /^<-[bcd]>*(c+)/ && $0), "c", 're_tests 817/1 (#1017)');
# 551: (a*)b+    caab    y    $0    aa
is(("caab" ~~ /(a*)b+/ && $0), "aa", 're_tests 819/1 (#1019)');
# 552: ([\w:]+::)?(\w+)$    abcd:    n    -    -
ok((not ("abcd:" ~~ /(<[\w:]>+\:\:)?(\w+)$/)), 're_tests 821  (#1021)');
# 553: ([\w:]+::)?(\w+)$    abcd    y    $0-$1    -abcd
is(("abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $0), "", 're_tests 823/1 (#1024)');
is(("abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $1), "abcd", 're_tests 823/2 (#1025)');
# 554: ([\w:]+::)?(\w+)$    xy:z:::abcd    y    $0-$1    xy:z:::-abcd
is(("xy:z:::abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $0), "xy:z:::", 're_tests 825/1 (#1028)', :todo<bug>);
is(("xy:z:::abcd" ~~ /(<[\w:]>+\:\:)?(\w+)$/ && $1), "abcd", 're_tests 825/2 (#1029)');
# 555: ^[^bcd]*(c+)    aexycd    y    $0    c
is(("aexycd" ~~ /^<-[bcd]>*(c+)/ && $0), "c", 're_tests 827/1 (#1031)');
# 556: (?{$a=2})a*aa(?{local$a=$a+1})k*c(?{$b=$a})    yaaxxaaaacd    y    $b    3
# SKIPPED: script doesn't understand `$b' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 557: (?{$a=2})(a(?{local$a=$a+1}))*aak*c(?{$b=$a})    yaaxxaaaacd    y    $b    4
# SKIPPED: script doesn't understand `$b' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 558: (>a+)ab    aaab    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 559: (?>a+)b    aaab    y    -    -
ok(("aaab" ~~ /[a+]:b/), 're_tests 830  (#1034)');
# 560: ([[:]+)    a:[b]:    y    $0    :[
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 561: ([[=]+)    a=[b]=    y    $0    =[
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 562: ([[.]+)    a.[b].    y    $0    .[
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 563: [a[:xyz:    -    c    -    Unmatched [
# -- SKIPPED - TESTS ERROR MESSAGE
# 564: [a[:xyz:]    -    c    -    POSIX class [:xyz:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 565: [a[:]b[:c]    abc    y    $&    abc
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 566: ([a[:xyz:]b]+)    pbaq    c    -    POSIX class [:xyz:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 567: [a[:]b[:c]    abc    y    $&    abc
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 568: ([[:alpha:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 569: ([[:alnum:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 570: ([[:ascii:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy__--  ${nulnul}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 571: ([[:cntrl:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ${nulnul}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 572: ([[:digit:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 573: ([[:graph:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy__--
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 574: ([[:lower:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    cd
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 575: ([[:print:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy__--  
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 576: ([[:punct:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    __--
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 577: ([[:space:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0      
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 578: ([[:word:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy__
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 579: ([[:upper:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    AB
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 580: ([[:xdigit:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 581: ([[:^alpha:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 582: ([[:^alnum:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    __--  ${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 583: ([[:^ascii:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 584: ([[:^cntrl:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy__--  
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 585: ([[:^digit:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 586: ([[:^lower:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    AB
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 587: ([[:^print:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 588: ([[:^punct:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 589: ([[:^space:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    ABcd01Xy__--
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 590: ([[:^word:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    --  ${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 591: ([[:^upper:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    cd01
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 592: ([[:^xdigit:]]+)    ABcd01Xy__--  ${nulnul}${ffff}    y    $0    Xy__--  ${nulnul}${ffff}
# -- SKIPPED - p5re_to_p6rule doesn't support string "ABcd01Xy__--  ${nulnul}${ffff}" yet
# 593: [[:foo:]]    -    c    -    POSIX class [:foo:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 594: [[:^foo:]]    -    c    -    POSIX class [:^foo:] unknown
# -- SKIPPED - TESTS ERROR MESSAGE
# 595: ((?>a+)b)    aaab    y    $0    aaab
is(("aaab" ~~ /([a+]:b)/ && $0), "aaab", 're_tests 837/1 (#1041)');
# 596: (?>(a+))b    aaab    y    $0    aaa
is(("aaab" ~~ /[(a+)]:b/ && $0), "aaa", 're_tests 839/1 (#1043)');
# 597: ((?>[^()]+)|\([^()]*\))+    ((abc(ade)ufh()()x    y    $&    abc(ade)ufh()()x
is(("((abc(ade)ufh()()x" ~~ /([<-[()]>+]:|\(<-[()]>*\))+/ && $<>), "abc(ade)ufh()()x", 're_tests 841/0 (#1045)');
# 598: (?<=x+)y    -    c    -    Variable length lookbehind not implemented
# -- SKIPPED - TESTS ERROR MESSAGE
# 599: a{37,17}    -    c    -    Can't do {n,m} with n > m
# -- SKIPPED - TESTS ERROR MESSAGE
# 600: \Z    a\nb\n    y    $-[0]    3
is(("a\nb\n" ~~ /\n?$/ && $/.from), 3, 're_tests 843/0 (#1047)');
# 601: \z    a\nb\n    y    $-[0]    4
is(("a\nb\n" ~~ /$/ && $/.from), 4, 're_tests 845/0 (#1049)');
# 602: $    a\nb\n    y    $-[0]    3
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 603: \Z    b\na\n    y    $-[0]    3
is(("b\na\n" ~~ /\n?$/ && $/.from), 3, 're_tests 848/0 (#1052)');
# 604: \z    b\na\n    y    $-[0]    4
is(("b\na\n" ~~ /$/ && $/.from), 4, 're_tests 850/0 (#1054)');
# 605: $    b\na\n    y    $-[0]    3
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 606: \Z    b\na    y    $-[0]    3
is(("b\na" ~~ /\n?$/ && $/.from), 3, 're_tests 853/0 (#1057)');
# 607: \z    b\na    y    $-[0]    3
is(("b\na" ~~ /$/ && $/.from), 3, 're_tests 855/0 (#1059)');
# 608: $    b\na    y    $-[0]    3
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 609: '\Z'm    a\nb\n    y    $-[0]    3
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 610: '\z'm    a\nb\n    y    $-[0]    4
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 611: '$'m    a\nb\n    y    $-[0]    1
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 612: '\Z'm    b\na\n    y    $-[0]    3
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 613: '\z'm    b\na\n    y    $-[0]    4
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 614: '$'m    b\na\n    y    $-[0]    1
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 615: '\Z'm    b\na    y    $-[0]    3
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 616: '\z'm    b\na    y    $-[0]    3
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 617: '$'m    b\na    y    $-[0]    1
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 618: a\Z    a\nb\n    n    -    -
ok((not ("a\nb\n" ~~ /a\n?$/)), 're_tests 867  (#1071)');
# 619: a\z    a\nb\n    n    -    -
ok((not ("a\nb\n" ~~ /a$/)), 're_tests 869  (#1073)');
# 620: a$    a\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 621: a\Z    b\na\n    y    $-[0]    2
is(("b\na\n" ~~ /a\n?$/ && $/.from), 2, 're_tests 872/0 (#1076)');
# 622: a\z    b\na\n    n    -    -
ok((not ("b\na\n" ~~ /a$/)), 're_tests 874  (#1078)');
# 623: a$    b\na\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 624: a\Z    b\na    y    $-[0]    2
is(("b\na" ~~ /a\n?$/ && $/.from), 2, 're_tests 877/0 (#1081)');
# 625: a\z    b\na    y    $-[0]    2
is(("b\na" ~~ /a$/ && $/.from), 2, 're_tests 879/0 (#1083)');
# 626: a$    b\na    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 627: 'a\Z'm    a\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 628: 'a\z'm    a\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 629: 'a$'m    a\nb\n    y    $-[0]    0
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 630: 'a\Z'm    b\na\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 631: 'a\z'm    b\na\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 632: 'a$'m    b\na\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 633: 'a\Z'm    b\na    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 634: 'a\z'm    b\na    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 635: 'a$'m    b\na    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 636: aa\Z    aa\nb\n    n    -    -
ok((not ("aa\nb\n" ~~ /aa\n?$/)), 're_tests 891  (#1095)');
# 637: aa\z    aa\nb\n    n    -    -
ok((not ("aa\nb\n" ~~ /aa$/)), 're_tests 893  (#1097)');
# 638: aa$    aa\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 639: aa\Z    b\naa\n    y    $-[0]    2
is(("b\naa\n" ~~ /aa\n?$/ && $/.from), 2, 're_tests 896/0 (#1100)');
# 640: aa\z    b\naa\n    n    -    -
ok((not ("b\naa\n" ~~ /aa$/)), 're_tests 898  (#1102)');
# 641: aa$    b\naa\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 642: aa\Z    b\naa    y    $-[0]    2
is(("b\naa" ~~ /aa\n?$/ && $/.from), 2, 're_tests 901/0 (#1105)');
# 643: aa\z    b\naa    y    $-[0]    2
is(("b\naa" ~~ /aa$/ && $/.from), 2, 're_tests 903/0 (#1107)');
# 644: aa$    b\naa    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 645: 'aa\Z'm    aa\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 646: 'aa\z'm    aa\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 647: 'aa$'m    aa\nb\n    y    $-[0]    0
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 648: 'aa\Z'm    b\naa\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 649: 'aa\z'm    b\naa\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 650: 'aa$'m    b\naa\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 651: 'aa\Z'm    b\naa    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 652: 'aa\z'm    b\naa    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 653: 'aa$'m    b\naa    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 654: aa\Z    ac\nb\n    n    -    -
ok((not ("ac\nb\n" ~~ /aa\n?$/)), 're_tests 915  (#1119)');
# 655: aa\z    ac\nb\n    n    -    -
ok((not ("ac\nb\n" ~~ /aa$/)), 're_tests 917  (#1121)');
# 656: aa$    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 657: aa\Z    b\nac\n    n    -    -
ok((not ("b\nac\n" ~~ /aa\n?$/)), 're_tests 920  (#1124)');
# 658: aa\z    b\nac\n    n    -    -
ok((not ("b\nac\n" ~~ /aa$/)), 're_tests 922  (#1126)');
# 659: aa$    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 660: aa\Z    b\nac    n    -    -
ok((not ("b\nac" ~~ /aa\n?$/)), 're_tests 925  (#1129)');
# 661: aa\z    b\nac    n    -    -
ok((not ("b\nac" ~~ /aa$/)), 're_tests 927  (#1131)');
# 662: aa$    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 663: 'aa\Z'm    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 664: 'aa\z'm    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 665: 'aa$'m    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 666: 'aa\Z'm    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 667: 'aa\z'm    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 668: 'aa$'m    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 669: 'aa\Z'm    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 670: 'aa\z'm    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 671: 'aa$'m    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 672: aa\Z    ca\nb\n    n    -    -
ok((not ("ca\nb\n" ~~ /aa\n?$/)), 're_tests 939  (#1143)');
# 673: aa\z    ca\nb\n    n    -    -
ok((not ("ca\nb\n" ~~ /aa$/)), 're_tests 941  (#1145)');
# 674: aa$    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 675: aa\Z    b\nca\n    n    -    -
ok((not ("b\nca\n" ~~ /aa\n?$/)), 're_tests 944  (#1148)');
# 676: aa\z    b\nca\n    n    -    -
ok((not ("b\nca\n" ~~ /aa$/)), 're_tests 946  (#1150)');
# 677: aa$    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 678: aa\Z    b\nca    n    -    -
ok((not ("b\nca" ~~ /aa\n?$/)), 're_tests 949  (#1153)');
# 679: aa\z    b\nca    n    -    -
ok((not ("b\nca" ~~ /aa$/)), 're_tests 951  (#1155)');
# 680: aa$    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 681: 'aa\Z'm    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 682: 'aa\z'm    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 683: 'aa$'m    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 684: 'aa\Z'm    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 685: 'aa\z'm    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 686: 'aa$'m    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 687: 'aa\Z'm    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 688: 'aa\z'm    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 689: 'aa$'m    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 690: ab\Z    ab\nb\n    n    -    -
ok((not ("ab\nb\n" ~~ /ab\n?$/)), 're_tests 963  (#1167)');
# 691: ab\z    ab\nb\n    n    -    -
ok((not ("ab\nb\n" ~~ /ab$/)), 're_tests 965  (#1169)');
# 692: ab$    ab\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 693: ab\Z    b\nab\n    y    $-[0]    2
is(("b\nab\n" ~~ /ab\n?$/ && $/.from), 2, 're_tests 968/0 (#1172)');
# 694: ab\z    b\nab\n    n    -    -
ok((not ("b\nab\n" ~~ /ab$/)), 're_tests 970  (#1174)');
# 695: ab$    b\nab\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 696: ab\Z    b\nab    y    $-[0]    2
is(("b\nab" ~~ /ab\n?$/ && $/.from), 2, 're_tests 973/0 (#1177)');
# 697: ab\z    b\nab    y    $-[0]    2
is(("b\nab" ~~ /ab$/ && $/.from), 2, 're_tests 975/0 (#1179)');
# 698: ab$    b\nab    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 699: 'ab\Z'm    ab\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 700: 'ab\z'm    ab\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 701: 'ab$'m    ab\nb\n    y    $-[0]    0
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 702: 'ab\Z'm    b\nab\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 703: 'ab\z'm    b\nab\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 704: 'ab$'m    b\nab\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 705: 'ab\Z'm    b\nab    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 706: 'ab\z'm    b\nab    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 707: 'ab$'m    b\nab    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 708: ab\Z    ac\nb\n    n    -    -
ok((not ("ac\nb\n" ~~ /ab\n?$/)), 're_tests 987  (#1191)');
# 709: ab\z    ac\nb\n    n    -    -
ok((not ("ac\nb\n" ~~ /ab$/)), 're_tests 989  (#1193)');
# 710: ab$    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 711: ab\Z    b\nac\n    n    -    -
ok((not ("b\nac\n" ~~ /ab\n?$/)), 're_tests 992  (#1196)');
# 712: ab\z    b\nac\n    n    -    -
ok((not ("b\nac\n" ~~ /ab$/)), 're_tests 994  (#1198)');
# 713: ab$    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 714: ab\Z    b\nac    n    -    -
ok((not ("b\nac" ~~ /ab\n?$/)), 're_tests 997  (#1201)');
# 715: ab\z    b\nac    n    -    -
ok((not ("b\nac" ~~ /ab$/)), 're_tests 999  (#1203)');
# 716: ab$    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 717: 'ab\Z'm    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 718: 'ab\z'm    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 719: 'ab$'m    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 720: 'ab\Z'm    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 721: 'ab\z'm    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 722: 'ab$'m    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 723: 'ab\Z'm    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 724: 'ab\z'm    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 725: 'ab$'m    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 726: ab\Z    ca\nb\n    n    -    -
ok((not ("ca\nb\n" ~~ /ab\n?$/)), 're_tests 1011  (#1215)');
# 727: ab\z    ca\nb\n    n    -    -
ok((not ("ca\nb\n" ~~ /ab$/)), 're_tests 1013  (#1217)');
# 728: ab$    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 729: ab\Z    b\nca\n    n    -    -
ok((not ("b\nca\n" ~~ /ab\n?$/)), 're_tests 1016  (#1220)');
# 730: ab\z    b\nca\n    n    -    -
ok((not ("b\nca\n" ~~ /ab$/)), 're_tests 1018  (#1222)');
# 731: ab$    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 732: ab\Z    b\nca    n    -    -
ok((not ("b\nca" ~~ /ab\n?$/)), 're_tests 1021  (#1225)');
# 733: ab\z    b\nca    n    -    -
ok((not ("b\nca" ~~ /ab$/)), 're_tests 1023  (#1227)');
# 734: ab$    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 735: 'ab\Z'm    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 736: 'ab\z'm    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 737: 'ab$'m    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 738: 'ab\Z'm    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 739: 'ab\z'm    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 740: 'ab$'m    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 741: 'ab\Z'm    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 742: 'ab\z'm    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 743: 'ab$'m    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 744: abb\Z    abb\nb\n    n    -    -
ok((not ("abb\nb\n" ~~ /abb\n?$/)), 're_tests 1035  (#1239)');
# 745: abb\z    abb\nb\n    n    -    -
ok((not ("abb\nb\n" ~~ /abb$/)), 're_tests 1037  (#1241)');
# 746: abb$    abb\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 747: abb\Z    b\nabb\n    y    $-[0]    2
is(("b\nabb\n" ~~ /abb\n?$/ && $/.from), 2, 're_tests 1040/0 (#1244)');
# 748: abb\z    b\nabb\n    n    -    -
ok((not ("b\nabb\n" ~~ /abb$/)), 're_tests 1042  (#1246)');
# 749: abb$    b\nabb\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 750: abb\Z    b\nabb    y    $-[0]    2
is(("b\nabb" ~~ /abb\n?$/ && $/.from), 2, 're_tests 1045/0 (#1249)');
# 751: abb\z    b\nabb    y    $-[0]    2
is(("b\nabb" ~~ /abb$/ && $/.from), 2, 're_tests 1047/0 (#1251)');
# 752: abb$    b\nabb    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 753: 'abb\Z'm    abb\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 754: 'abb\z'm    abb\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 755: 'abb$'m    abb\nb\n    y    $-[0]    0
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 756: 'abb\Z'm    b\nabb\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 757: 'abb\z'm    b\nabb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 758: 'abb$'m    b\nabb\n    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 759: 'abb\Z'm    b\nabb    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 760: 'abb\z'm    b\nabb    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 761: 'abb$'m    b\nabb    y    $-[0]    2
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 762: abb\Z    ac\nb\n    n    -    -
ok((not ("ac\nb\n" ~~ /abb\n?$/)), 're_tests 1059  (#1263)');
# 763: abb\z    ac\nb\n    n    -    -
ok((not ("ac\nb\n" ~~ /abb$/)), 're_tests 1061  (#1265)');
# 764: abb$    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 765: abb\Z    b\nac\n    n    -    -
ok((not ("b\nac\n" ~~ /abb\n?$/)), 're_tests 1064  (#1268)');
# 766: abb\z    b\nac\n    n    -    -
ok((not ("b\nac\n" ~~ /abb$/)), 're_tests 1066  (#1270)');
# 767: abb$    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 768: abb\Z    b\nac    n    -    -
ok((not ("b\nac" ~~ /abb\n?$/)), 're_tests 1069  (#1273)');
# 769: abb\z    b\nac    n    -    -
ok((not ("b\nac" ~~ /abb$/)), 're_tests 1071  (#1275)');
# 770: abb$    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 771: 'abb\Z'm    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 772: 'abb\z'm    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 773: 'abb$'m    ac\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 774: 'abb\Z'm    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 775: 'abb\z'm    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 776: 'abb$'m    b\nac\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 777: 'abb\Z'm    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 778: 'abb\z'm    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 779: 'abb$'m    b\nac    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 780: abb\Z    ca\nb\n    n    -    -
ok((not ("ca\nb\n" ~~ /abb\n?$/)), 're_tests 1083  (#1287)');
# 781: abb\z    ca\nb\n    n    -    -
ok((not ("ca\nb\n" ~~ /abb$/)), 're_tests 1085  (#1289)');
# 782: abb$    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 783: abb\Z    b\nca\n    n    -    -
ok((not ("b\nca\n" ~~ /abb\n?$/)), 're_tests 1088  (#1292)');
# 784: abb\z    b\nca\n    n    -    -
ok((not ("b\nca\n" ~~ /abb$/)), 're_tests 1090  (#1294)');
# 785: abb$    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 786: abb\Z    b\nca    n    -    -
ok((not ("b\nca" ~~ /abb\n?$/)), 're_tests 1093  (#1297)');
# 787: abb\z    b\nca    n    -    -
ok((not ("b\nca" ~~ /abb$/)), 're_tests 1095  (#1299)');
# 788: abb$    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 789: 'abb\Z'm    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 790: 'abb\z'm    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 791: 'abb$'m    ca\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 792: 'abb\Z'm    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 793: 'abb\z'm    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 794: 'abb$'m    b\nca\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 795: 'abb\Z'm    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 796: 'abb\z'm    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 797: 'abb$'m    b\nca    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 798: (^|x)(c)    ca    y    $1    c
is(("ca" ~~ /(^|x)(c)/ && $1), "c", 're_tests 1107/2 (#1311)');
# 799: a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz    x    n    -    -
ok((not ("x" ~~ /a*abc?xyz+pqr**{3}ab**{2...}xy**{4..5}pq**{0..6}AB**{0...}zz/)), 're_tests 1109  (#1313)');
# 800: a(?{$a=2;$b=3;($b)=$a})b    yabz    y    $b    2
# SKIPPED: script doesn't understand `$b' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?{...' yet
# 801: round\(((?>[^()]+))\)    _I(round(xs * sz),1)    y    $0    xs * sz
is(("_I(round(xs * sz),1)" ~~ /round\(([<-[()]>+]:)\)/ && $0), "xs * sz", 're_tests 1111/1 (#1315)');
# 802: '((?x:.) )'    x     y    $0-    x -
# SKIPPED: script doesn't understand `$0-' yet
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 803: '((?-x:.) )'x    x     y    $0-    x-
# SKIPPED: script doesn't understand `$0-' yet
# -- SKIPPED - p5re_to_p6rule doesn't support `(?-...' yet
# 804: foo.bart    foo.bart    y    -    -
ok(("foo.bart" ~~ /foo\Nbart/), 're_tests 1113  (#1317)');
# 805: '^d[x][x][x]'m    abcd\ndxxx    y    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 806: .X(.+)+X    bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
ok(("bXcXa" ~~ /\NX(\N+)+X/), 're_tests 1116  (#1320)');
# 807: .X(.+)+XX    bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
ok(("bXcXXa" ~~ /\NX(\N+)+XX/), 're_tests 1118  (#1322)');
# 808: .XX(.+)+X    bXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
ok(("bXXcXa" ~~ /\NXX(\N+)+X/), 're_tests 1120  (#1324)');
# 809: .X(.+)+X    bXXa    n    -    -
ok((not ("bXXa" ~~ /\NX(\N+)+X/)), 're_tests 1122  (#1326)');
# 810: .X(.+)+XX    bXXXa    n    -    -
ok((not ("bXXXa" ~~ /\NX(\N+)+XX/)), 're_tests 1124  (#1328)');
# 811: .XX(.+)+X    bXXXa    n    -    -
ok((not ("bXXXa" ~~ /\NXX(\N+)+X/)), 're_tests 1126  (#1330)');
# 812: .X(.+)+[X]    bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
fail("PGE nonterminates", :todo<bug>);
#ok(("bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ /\NX(\N+)+<[X]>/), 're_tests 1128  (#1332)');
# 813: .X(.+)+[X][X]    bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
fail("PGE nonterminates", :todo<bug>);
#ok(("bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ /\NX(\N+)+<[X]><[X]>/), 're_tests 1130  (#1334)');
# 814: .XX(.+)+[X]    bXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
fail("PGE nonterminates", :todo<bug>);
#ok(("bXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ /\NXX(\N+)+<[X]>/), 're_tests 1132  (#1336)');
# 815: .X(.+)+[X]    bXXa    n    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok((not ("bXXa" ~~ /\NX(\N+)+<[X]>/)), 're_tests 1134  (#1338)');
# 816: .X(.+)+[X][X]    bXXXa    n    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok((not ("bXXXa" ~~ /\NX(\N+)+<[X]><[X]>/)), 're_tests 1136  (#1340)');
# 817: .XX(.+)+[X]    bXXXa    n    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok((not ("bXXXa" ~~ /\NXX(\N+)+<[X]>/)), 're_tests 1138  (#1342)');
# 818: .[X](.+)+[X]    bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok(("bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ /\N<[X]>(\N+)+<[X]>/), 're_tests 1140  (#1344)');
# 819: .[X](.+)+[X][X]    bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok(("bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ /\N<[X]>(\N+)+<[X]><[X]>/), 're_tests 1142  (#1346)');
# 820: .[X][X](.+)+[X]    bXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa    y    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok(("bXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~~ /\N<[X]><[X]>(\N+)+<[X]>/), 're_tests 1144  (#1348)');
# 821: .[X](.+)+[X]    bXXa    n    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok((not ("bXXa" ~~ /\N<[X]>(\N+)+<[X]>/)), 're_tests 1146  (#1350)');
# 822: .[X](.+)+[X][X]    bXXXa    n    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok((not ("bXXXa" ~~ /\N<[X]>(\N+)+<[X]><[X]>/)), 're_tests 1148  (#1352)');
# 823: .[X][X](.+)+[X]    bXXXa    n    -    -
fail("PGE probably nonterminates", :todo<bug>);
#ok((not ("bXXXa" ~~ /\N<[X]><[X]>(\N+)+<[X]>/)), 're_tests 1150  (#1354)');
# 824: tt+$    xxxtt    y    -    -
ok(("xxxtt" ~~ /tt+$/), 're_tests 1152  (#1356)');
# 825: ([a..\d]+)    za-9z    y    $0    a-9
is(("za-9z" ~~ /(<[a..\d]>+)/ && $0), "a-9", 're_tests 1154/1 (#1358)', :todo<bug>);
# 826: ([\d..z]+)    a0-za    y    $0    0-z
is(("a0-za" ~~ /(<[\d..z]>+)/ && $0), "0-z", 're_tests 1156/1 (#1360)', :todo<bug>);
# 827: ([\d..\s]+)    a0- z    y    $0    0- 
is(("a0- z" ~~ /(<[\d..\s]>+)/ && $0), "0- ", 're_tests 1158/1 (#1362)', :todo<bug>);
# 828: ([a-[:digit:]]+)    za-9z    y    $0    a-9
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 829: ([[:digit:]-z]+)    =0-z=    y    $0    0-z
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 830: ([[:digit:]-[:alpha:]]+)    =0-z=    y    $0    0-z
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 831: \GX.*X    aaaXbX    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support this yet
# 832: (\d+\.\d+)    3.1415926    y    $0    3.1415926
is(("3.1415926" ~~ /(\d+\.\d+)/ && $0), "3.1415926", 're_tests 1164/1 (#1368)');
# 833: (\ba.{0,10}br)    have a web browser    y    $0    a web br
is(("have a web browser" ~~ /(\ba\N**{0..10}br)/ && $0), "a web br", 're_tests 1166/1 (#1370)');
# 834: '\.c(pp|xx|c)?$'i    Changes    n    -    -
ok((not ("Changes" ~~ rx:i/\.c(pp|xx|c)?$/)), 're_tests 1168  (#1372)');
# 835: '\.c(pp|xx|c)?$'i    IO.c    y    -    -
ok(("IO.c" ~~ rx:i/\.c(pp|xx|c)?$/), 're_tests 1170  (#1374)');
# 836: '(\.c(pp|xx|c)?$)'i    IO.c    y    $0    .c
is(("IO.c" ~~ rx:i/(\.c(pp|xx|c)?$)/ && $0), ".c", 're_tests 1172/1 (#1376)');
# 837: ^([a..z]:)    C:/    n    -    -
ok((not ("C:/" ~~ /^(<[a..z]>\:)/)), 're_tests 1174  (#1378)');
# 838: '^\S\s+aa$'m    \nx aa    y    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 839: (^|a)b    ab    y    -    -
ok(("ab" ~~ /(^|a)b/), 're_tests 1177  (#1381)');
# 840: ^([ab]*?)(b)?(c)$    abac    y    -$1-    --
# SKIPPED: script doesn't understand `-$1-' yet
# SKIPPED: script doesn't understand `-$1-' yet
# 841: (\w)?(abc)\1b    abcab    n    -    -
ok((not ("abcab" ~~ /(\w)?(abc)$0b/)), 're_tests 1179  (#1383)');
# 842: ^(?:.,){2}c    a,b,c    y    -    -
ok(("a,b,c" ~~ /^[\N,]**{2}c/), 're_tests 1181  (#1385)');
# 843: ^(.,){2}c    a,b,c    y    $0    b,
is(("a,b,c" ~~ /^(\N,)**{2}c/ && $0), "b,", 're_tests 1183/1 (#1387)');
# 844: ^(?:[^,]*,){2}c    a,b,c    y    -    -
ok(("a,b,c" ~~ /^[<-[,]>*,]**{2}c/), 're_tests 1185  (#1389)');
# 845: ^([^,]*,){2}c    a,b,c    y    $0    b,
is(("a,b,c" ~~ /^(<-[,]>*,)**{2}c/ && $0), "b,", 're_tests 1187/1 (#1391)');
# 846: ^([^,]*,){3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>*,)**{3}d/ && $0), "c,", 're_tests 1189/1 (#1393)');
# 847: ^([^,]*,){3,}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>*,)**{3...}d/ && $0), "c,", 're_tests 1191/1 (#1395)');
# 848: ^([^,]*,){0,3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>*,)**{0..3}d/ && $0), "c,", 're_tests 1193/1 (#1397)');
# 849: ^([^,]{1,3},){3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{1..3},)**{3}d/ && $0), "c,", 're_tests 1195/1 (#1399)');
# 850: ^([^,]{1,3},){3,}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{1..3},)**{3...}d/ && $0), "c,", 're_tests 1197/1 (#1401)');
# 851: ^([^,]{1,3},){0,3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{1..3},)**{0..3}d/ && $0), "c,", 're_tests 1199/1 (#1403)');
# 852: ^([^,]{1,},){3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{1...},)**{3}d/ && $0), "c,", 're_tests 1201/1 (#1405)');
# 853: ^([^,]{1,},){3,}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{1...},)**{3...}d/ && $0), "c,", 're_tests 1203/1 (#1407)');
# 854: ^([^,]{1,},){0,3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{1...},)**{0..3}d/ && $0), "c,", 're_tests 1205/1 (#1409)');
# 855: ^([^,]{0,3},){3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{0..3},)**{3}d/ && $0), "c,", 're_tests 1207/1 (#1411)');
# 856: ^([^,]{0,3},){3,}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{0..3},)**{3...}d/ && $0), "c,", 're_tests 1209/1 (#1413)');
# 857: ^([^,]{0,3},){0,3}d    aaa,b,c,d    y    $0    c,
is(("aaa,b,c,d" ~~ /^(<-[,]>**{0..3},)**{0..3}d/ && $0), "c,", 're_tests 1211/1 (#1415)');
# 858: (?i)        y    -    -
fail("PGE segfault", :todo<bug>);
#ok(("" ~~ /:i /), 're_tests 1213  (#1417)');
# 859: '(?!\A)x'm    a\nxb\n    y    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 860: ^(a(b)?)+$    aba    y    -$0-$1-    -a--
# SKIPPED: script doesn't understand `-$0-$1-' yet
# SKIPPED: script doesn't understand `-$0-$1-' yet
# 861: ^(aa(bb)?)+$    aabbaa    y    -$0-$1-    -aa--
# SKIPPED: script doesn't understand `-$0-$1-' yet
# SKIPPED: script doesn't understand `-$0-$1-' yet
# 862: '^.{9}abc.*\n'm    123\nabcabcabcabc\n    y    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 863: ^(a)?a$    a    y    -$0-    --
# SKIPPED: script doesn't understand `-$0-' yet
# SKIPPED: script doesn't understand `-$0-' yet
# 864: ^(a)?(?(1)a|b)+$    a    n    -    -
ok((not ("a" ~~ /^(a)?[ <(defined $0)> :: a|b ]+$/)), 're_tests 1217  (#1421)');
# 865: ^(a\1?)(a\1?)(a\2?)(a\3?)$    aaaaaa    y    $0,$1,$2,$3    a,aa,a,aa
# SKIPPED: script doesn't understand `$0,$1,$2,$3' yet
# SKIPPED: script doesn't understand `$0,$1,$2,$3' yet
# 866: ^(a\1?){4}$    aaaaaa    y    $0    aa
is(("aaaaaa" ~~ /^(a$0?)**{4}$/ && $0), "aa", 're_tests 1219/1 (#1423)', :todo<feature>);
# 867: ^(0+)?(?:x(1))?    x1    y    -    -
ok(("x1" ~~ /^(0+)?[x(1)]?/), 're_tests 1221  (#1425)');
# 868: ^([0..9a..fA..F]+)(?:x([0..9a..fA..F]+)?)(?:x([0..9a..fA..F]+))?    012cxx0190    y    -    -
ok(("012cxx0190" ~~ /^(<[0..9a..fA..F]>+)[x(<[0..9a..fA..F]>+)?][x(<[0..9a..fA..F]>+)]?/), 're_tests 1223  (#1427)');
# 869: ^(b+?|a){1,2}c    bbbac    y    $0    a
is(("bbbac" ~~ /^(b+?|a)**{1..2}c/ && $0), "a", 're_tests 1225/1 (#1429)');
# 870: ^(b+?|a){1,2}c    bbbbac    y    $0    a
is(("bbbbac" ~~ /^(b+?|a)**{1..2}c/ && $0), "a", 're_tests 1227/1 (#1431)');
# 871: \((\w\. \w+)\)    cd. (A. Tw)    y    -$0-    -A. Tw-
# SKIPPED: script doesn't understand `-$0-' yet
# SKIPPED: script doesn't understand `-$0-' yet
# 872: ((?:aaaa|bbbb)cccc)?    aaaacccc    y    -    -
ok(("aaaacccc" ~~ /([aaaa|bbbb]cccc)?/), 're_tests 1229  (#1433)');
# 873: ((?:aaaa|bbbb)cccc)?    bbbbcccc    y    -    -
ok(("bbbbcccc" ~~ /([aaaa|bbbb]cccc)?/), 're_tests 1231  (#1435)');
# 874: (a)?(a)+    a    y    $0:$1    :a    -
is(("a" ~~ /(a)?(a)+/ && $0), "", 're_tests 1233/1 (#1438)');
is(("a" ~~ /(a)?(a)+/ && $1), "a", 're_tests 1233/2 (#1439)');
# 875: (ab)?(ab)+    ab    y    $0:$1    :ab    -
is(("ab" ~~ /(ab)?(ab)+/ && $0), "", 're_tests 1235/1 (#1442)');
is(("ab" ~~ /(ab)?(ab)+/ && $1), "ab", 're_tests 1235/2 (#1443)');
# 876: (abc)?(abc)+    abc    y    $0:$1    :abc    -
is(("abc" ~~ /(abc)?(abc)+/ && $0), "", 're_tests 1237/1 (#1446)');
is(("abc" ~~ /(abc)?(abc)+/ && $1), "abc", 're_tests 1237/2 (#1447)');
# 877: 'b\s^'m    a\nb\n    n    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `'...'m' yet
# 878: \ba    a    y    -    -
ok(("a" ~~ /\ba/), 're_tests 1240  (#1450)');
# 879: ^(a(??{"(?!)"})|(a)(?{1}))b    ab    y    $1    a    # [ID 20010811.006]
# -- SKIPPED - p5re_to_p6rule doesn't support `(??{...' yet
# 880: ab(?i)cd    AbCd    n    -    -    # [ID 20010809.023]
ok((not ("AbCd" ~~ /ab:i cd/)), 're_tests 1243  (#1453)');
# 881: ab(?i)cd    abCd    y    -    -
ok(("abCd" ~~ /ab:i cd/), 're_tests 1245  (#1455)', :todo<feature>);
# 882: (A|B)*(?(1)(CD)|(CD))    CD    y    $1-$2    -CD
is(("CD" ~~ /(A|B)*[ <(defined $0)> :: (CD)|(CD) ]/ && $1), "", 're_tests 1247/2 (#1458)');
is(("CD" ~~ /(A|B)*[ <(defined $0)> :: (CD)|(CD) ]/ && $2), "CD", 're_tests 1247/3 (#1459)', :todo<feature>);
# 883: (A|B)*(?(1)(CD)|(CD))    ABCD    y    $1-$2    CD-
is(("ABCD" ~~ /(A|B)*[ <(defined $0)> :: (CD)|(CD) ]/ && $1), "CD", 're_tests 1249/2 (#1462)', :todo<feature>);
is(("ABCD" ~~ /(A|B)*[ <(defined $0)> :: (CD)|(CD) ]/ && $2), "", 're_tests 1249/3 (#1463)');
# 884: (A|B)*?(?(1)(CD)|(CD))    CD    y    $1-$2    -CD    # [ID 20010803.016]
is(("CD" ~~ /(A|B)*?[ <(defined $0)> :: (CD)|(CD) ]/ && $1), "", 're_tests 1251/2 (#1466)');
is(("CD" ~~ /(A|B)*?[ <(defined $0)> :: (CD)|(CD) ]/ && $2), "CD", 're_tests 1251/3 (#1467)', :todo<feature>);
# 885: (A|B)*?(?(1)(CD)|(CD))    ABCD    y    $1-$2    CD-
is(("ABCD" ~~ /(A|B)*?[ <(defined $0)> :: (CD)|(CD) ]/ && $1), "CD", 're_tests 1253/2 (#1470)', :todo<feature>);
is(("ABCD" ~~ /(A|B)*?[ <(defined $0)> :: (CD)|(CD) ]/ && $2), "", 're_tests 1253/3 (#1471)');
# 886: '^(o)(?!.*\1)'i    Oo    n    -    -
ok((not ("Oo" ~~ rx:i/^(o)<!before \N*$0>/)), 're_tests 1255  (#1473)');
# 887: (.*)\d+\1    abc12bc    y    $0    bc
is(("abc12bc" ~~ /(\N*)\d+$0/ && $0), "bc", 're_tests 1257/1 (#1475)');
# 888: (?m:(foo\s*$))    foo\n bar    y    $0    foo
# -- SKIPPED - p5re_to_p6rule doesn't support `$ with \n in str' yet
# 889: (.*)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)c/ && $0), "ab", 're_tests 1260/1 (#1478)');
# 890: (.*)(?=c)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<before c>/ && $0), "ab", 're_tests 1262/1 (#1480)', :todo<feature>);
# 891: (.*)(?=c)c    abcd    yB    $0    ab
is(("abcd" ~~ /(\N*)<before c>c/ && $0), "ab", 're_tests 1264/1 (#1482)', :todo<feature>);
# 892: (.*)(?=b|c)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<before b|c>/ && $0), "ab", 're_tests 1266/1 (#1484)', :todo<feature>);
# 893: (.*)(?=b|c)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<before b|c>c/ && $0), "ab", 're_tests 1268/1 (#1486)', :todo<feature>);
# 894: (.*)(?=c|b)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<before c|b>/ && $0), "ab", 're_tests 1270/1 (#1488)', :todo<feature>);
# 895: (.*)(?=c|b)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<before c|b>c/ && $0), "ab", 're_tests 1272/1 (#1490)', :todo<feature>);
# 896: (.*)(?=[bc])    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<before <[bc]>>/ && $0), "ab", 're_tests 1274/1 (#1492)', :todo<feature>);
# 897: (.*)(?=[bc])c    abcd    yB    $0    ab
is(("abcd" ~~ /(\N*)<before <[bc]>>c/ && $0), "ab", 're_tests 1276/1 (#1494)', :todo<feature>);
# 898: (.*)(?<=b)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<after b>/ && $0), "ab", 're_tests 1278/1 (#1496)', :todo<feature>);
# 899: (.*)(?<=b)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<after b>c/ && $0), "ab", 're_tests 1280/1 (#1498)', :todo<feature>);
# 900: (.*)(?<=b|c)    abcd    y    $0    abc
is(("abcd" ~~ /(\N*)<after b|c>/ && $0), "abc", 're_tests 1282/1 (#1500)', :todo<feature>);
# 901: (.*)(?<=b|c)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<after b|c>c/ && $0), "ab", 're_tests 1284/1 (#1502)', :todo<feature>);
# 902: (.*)(?<=c|b)    abcd    y    $0    abc
is(("abcd" ~~ /(\N*)<after c|b>/ && $0), "abc", 're_tests 1286/1 (#1504)', :todo<feature>);
# 903: (.*)(?<=c|b)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<after c|b>c/ && $0), "ab", 're_tests 1288/1 (#1506)', :todo<feature>);
# 904: (.*)(?<=[bc])    abcd    y    $0    abc
is(("abcd" ~~ /(\N*)<after <[bc]>>/ && $0), "abc", 're_tests 1290/1 (#1508)', :todo<feature>);
# 905: (.*)(?<=[bc])c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*)<after <[bc]>>c/ && $0), "ab", 're_tests 1292/1 (#1510)', :todo<feature>);
# 906: (.*?)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)c/ && $0), "ab", 're_tests 1294/1 (#1512)');
# 907: (.*?)(?=c)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<before c>/ && $0), "ab", 're_tests 1296/1 (#1514)', :todo<feature>);
# 908: (.*?)(?=c)c    abcd    yB    $0    ab
is(("abcd" ~~ /(\N*?)<before c>c/ && $0), "ab", 're_tests 1298/1 (#1516)', :todo<feature>);
# 909: (.*?)(?=b|c)    abcd    y    $0    a
is(("abcd" ~~ /(\N*?)<before b|c>/ && $0), "a", 're_tests 1300/1 (#1518)', :todo<feature>);
# 910: (.*?)(?=b|c)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<before b|c>c/ && $0), "ab", 're_tests 1302/1 (#1520)', :todo<feature>);
# 911: (.*?)(?=c|b)    abcd    y    $0    a
is(("abcd" ~~ /(\N*?)<before c|b>/ && $0), "a", 're_tests 1304/1 (#1522)', :todo<feature>);
# 912: (.*?)(?=c|b)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<before c|b>c/ && $0), "ab", 're_tests 1306/1 (#1524)', :todo<feature>);
# 913: (.*?)(?=[bc])    abcd    y    $0    a
is(("abcd" ~~ /(\N*?)<before <[bc]>>/ && $0), "a", 're_tests 1308/1 (#1526)', :todo<feature>);
# 914: (.*?)(?=[bc])c    abcd    yB    $0    ab
is(("abcd" ~~ /(\N*?)<before <[bc]>>c/ && $0), "ab", 're_tests 1310/1 (#1528)', :todo<feature>);
# 915: (.*?)(?<=b)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after b>/ && $0), "ab", 're_tests 1312/1 (#1530)', :todo<feature>);
# 916: (.*?)(?<=b)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after b>c/ && $0), "ab", 're_tests 1314/1 (#1532)', :todo<feature>);
# 917: (.*?)(?<=b|c)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after b|c>/ && $0), "ab", 're_tests 1316/1 (#1534)', :todo<feature>);
# 918: (.*?)(?<=b|c)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after b|c>c/ && $0), "ab", 're_tests 1318/1 (#1536)', :todo<feature>);
# 919: (.*?)(?<=c|b)    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after c|b>/ && $0), "ab", 're_tests 1320/1 (#1538)', :todo<feature>);
# 920: (.*?)(?<=c|b)c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after c|b>c/ && $0), "ab", 're_tests 1322/1 (#1540)', :todo<feature>);
# 921: (.*?)(?<=[bc])    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after <[bc]>>/ && $0), "ab", 're_tests 1324/1 (#1542)', :todo<feature>);
# 922: (.*?)(?<=[bc])c    abcd    y    $0    ab
is(("abcd" ~~ /(\N*?)<after <[bc]>>c/ && $0), "ab", 're_tests 1326/1 (#1544)', :todo<feature>);
# 923: 2(]*)?$\1    2    y    $&    2
is(("2" ~~ /2(]*)?$0/ && $<>), "2", 're_tests 1328/0 (#1546)', :todo<bug>);
# 924: (??{})    x    y    -    -
# -- SKIPPED - p5re_to_p6rule doesn't support `(??{...' yet
# 925: a(b)??    abc    y    <$0>    <>    # undef [perl #16773]
# SKIPPED: script doesn't understand `<$0>' yet
# SKIPPED: script doesn't understand `<$0>' yet
# 926: (\d{1,3}\.){3,}    128.134.142.8    y    <$0>    <142.>    # [perl #18019]
# SKIPPED: script doesn't understand `<$0>' yet
# SKIPPED: script doesn't understand `<$0>' yet
# 927: ^.{3,4}(.+)\1\z    foobarbar    y    $0    bar    # 16 tests for [perl #23171]
is(("foobarbar" ~~ /^\N**{3..4}(\N+)$0$/ && $0), "bar", 're_tests 1331/1 (#1549)');
# 928: ^(?:f|o|b){3,4}(.+)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{3..4}(\N+)$0$/ && $0), "bar", 're_tests 1333/1 (#1551)');
# 929: ^.{3,4}((?:b|a|r)+)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^\N**{3..4}([b|a|r]+)$0$/ && $0), "bar", 're_tests 1335/1 (#1553)');
# 930: ^(?:f|o|b){3,4}((?:b|a|r)+)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{3..4}([b|a|r]+)$0$/ && $0), "bar", 're_tests 1337/1 (#1555)');
# 931: ^.{3,4}(.+?)\1\z    foobarbar    y    $0    bar
#is(("foobarbar" ~~ /^\N**{3..4}(\N+?)$0$/ && $0), "bar", 're_tests 1339/1 (#1557)'); ## XXX PGE BUG
# 932: ^(?:f|o|b){3,4}(.+?)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{3..4}(\N+?)$0$/ && $0), "bar", 're_tests 1341/1 (#1559)');
# 933: ^.{3,4}((?:b|a|r)+?)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^\N**{3..4}([b|a|r]+?)$0$/ && $0), "bar", 're_tests 1343/1 (#1561)');
# 934: ^(?:f|o|b){3,4}((?:b|a|r)+?)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{3..4}([b|a|r]+?)$0$/ && $0), "bar", 're_tests 1345/1 (#1563)');
# 935: ^.{2,3}?(.+)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^\N**{2..3}?(\N+)$0$/ && $0), "bar", 're_tests 1347/1 (#1565)');
# 936: ^(?:f|o|b){2,3}?(.+)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{2..3}?(\N+)$0$/ && $0), "bar", 're_tests 1349/1 (#1567)');
# 937: ^.{2,3}?((?:b|a|r)+)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^\N**{2..3}?([b|a|r]+)$0$/ && $0), "bar", 're_tests 1351/1 (#1569)');
# 938: ^(?:f|o|b){2,3}?((?:b|a|r)+)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{2..3}?([b|a|r]+)$0$/ && $0), "bar", 're_tests 1353/1 (#1571)');
# 939: ^.{2,3}?(.+?)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^\N**{2..3}?(\N+?)$0$/ && $0), "bar", 're_tests 1355/1 (#1573)');
# 940: ^(?:f|o|b){2,3}?(.+?)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{2..3}?(\N+?)$0$/ && $0), "bar", 're_tests 1357/1 (#1575)');
# 941: ^.{2,3}?((?:b|a|r)+?)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^\N**{2..3}?([b|a|r]+?)$0$/ && $0), "bar", 're_tests 1359/1 (#1577)');
# 942: ^(?:f|o|b){2,3}?((?:b|a|r)+?)\1\z    foobarbar    y    $0    bar
is(("foobarbar" ~~ /^[f|o|b]**{2..3}?([b|a|r]+?)$0$/ && $0), "bar", 're_tests 1361/1 (#1579)');
# 943: .*a(?!(b|cd)*e).*f    ......abef    n    -    -    # [perl #23030]
ok((not ("......abef" ~~ /\N*a<!before (b|cd)*e>\N*f/)), 're_tests 1363  (#1581)');
# 944: x(?#    x    c    -    Sequence (?#... not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
# 945: :x(?#:    x    c    -    Sequence (?#... not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
