#!/usr/bin/pugs

use v6;
use Test;

plan 142;

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
is(("ab" ~~ /(a+|b)*/ && $/[0][1].from), 1, 're_tests 222/1 (#266)');
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
is(("abcd" ~~ /(<[abc]>)*bcd/ && $0[0]), "a", 're_tests 242/1 (#300)');
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
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[0].from), 0, 're_tests 302/2 (#386)');
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[0][0].from), 0, 're_tests 302/3 (#386)');
# 218: ((a)(b)c)(d)    abcd    y    $-[3]    1
# 219: ((a)(b)c)(d)    abcd    y    $+[3]    2 # SKIP
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[0][1].from), 1, 're_tests 304/3 (#388)');
# 220: ((a)(b)c)(d)    abcd    y    $-[4]    3
# 221: ((a)(b)c)(d)    abcd    y    $+[4]    4 # SKIP
is(("abcd" ~~ /((a)(b)c)(d)/ && $/[1].from), 3, 're_tests 306/4 (#390)');
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
is(("ij" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $/[0][0]), "j", 're_tests 314/2 (#408)');
# 226: (bc+d$|ef*g.|h?i(j|k))    effg    n    -    -
ok((not ("effg" ~~ /(bc+d$|ef*g\N|h?i(j|k))/)), 're_tests 316  (#410)');
# 227: (bc+d$|ef*g.|h?i(j|k))    bcdd    n    -    -
ok((not ("bcdd" ~~ /(bc+d$|ef*g\N|h?i(j|k))/)), 're_tests 318  (#412)');
# 228: (bc+d$|ef*g.|h?i(j|k))    reffgz    y    $&-$0-$1    effgz-effgz-
is(("reffgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $<>), "effgz", 're_tests 320/0 (#416)');
is(("reffgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $0), "effgz", 're_tests 320/1 (#417)');
is(("reffgz" ~~ /(bc+d$|ef*g\N|h?i(j|k))/ && $1), "", 're_tests 320/2 (#418)');
# 229: ((((((((((a))))))))))    a    y    $00    a
is(("a" ~~ /((((((((((a))))))))))/ && $0), "a", 're_tests 322/10 (#420)');
# 230: ((((((((((a))))))))))    a    y    $-[0]    0
# 231: ((((((((((a))))))))))    a    y    $+[0]    1 # SKIP
is(("a" ~~ /((((((((((a))))))))))/ && $/.from), 0, 're_tests 324/0 (#422)');
# 232: ((((((((((a))))))))))    a    y    $-[10]    0
# 233: ((((((((((a))))))))))    a    y    $+[10]    1 # SKIP
is(("a" ~~ /((((((((((a))))))))))/ && $/[0].from), 0, 're_tests 326/10 (#424)');
# 234: ((((((((((a))))))))))\10    aa    y    $&    aa
is(("aa" ~~ /((((((((((a))))))))))$0/ && $<>), "aa", 're_tests 328/0 (#426)');
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
is(("ababbbcbc" ~~ /((<[a..c]>)b*?$1)*/ && $0[0]), "b", 're_tests 352/2 (#458)', :todo<feature>);
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
is(("b" ~~ /(a)|(b)/ && $/[1].from), 0, 're_tests 360/2 (#470)', :todo<bug>);
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

