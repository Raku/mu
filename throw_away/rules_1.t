#!/usr/bin/pugs

use v6;
use Test;

plan 126;

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
is(("abc" ~~ /((a))/ && $/[0][0]), "a", 're_tests 208/2 (#246)', :todo<bug>);
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

