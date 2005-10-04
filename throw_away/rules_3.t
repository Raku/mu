#!/usr/bin/pugs

use v6;
use Test;

plan 162;

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

