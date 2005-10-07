#!/usr/bin/pugs

use v6;
use Test;



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
ok((not ("dbcb" ~~ /<!after <[cd]>>b/)), 're_tests 741  (#937)', :todo<feature>);
# 498: (?<![cd])[ab]    dbaacb    y    $&    a
is(("dbaacb" ~~ /<!after <[cd]>><[ab]>/ && $<>), "a", 're_tests 743/0 (#939)', :todo<feature>);
# 499: (?<!(c|d))b    dbcb    n    -    -
ok((not ("dbcb" ~~ /<!after (c|d)>b/)), 're_tests 745  (#941)', :todo<feature>);
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
ok((not ("a" ~~ /[ <(defined $0)> :: a|b ]/)), 're_tests 765  (#961)', :todo<feature>);
# 517: (?(1)b|a)    a    y    $&    a
is(("a" ~~ /[ <(defined $0)> :: b|a ]/ && $<>), "a", 're_tests 767/0 (#963)', :todo<feature>);
# 518: (x)?(?(1)a|b)    a    n    -    -
ok((not ("a" ~~ /(x)?[ <(defined $0)> :: a|b ]/)), 're_tests 769  (#965)', :todo<feature>);
# 519: (x)?(?(1)b|a)    a    y    $&    a
is(("a" ~~ /(x)?[ <(defined $0)> :: b|a ]/ && $<>), "a", 're_tests 771/0 (#967)', :todo<feature>);
# 520: ()?(?(1)b|a)    a    y    $&    a
is(("a" ~~ /(<?null>)?[ <(defined $0)> :: b|a ]/ && $<>), "a", 're_tests 773/0 (#969)', :todo<feature>);
# 521: ()(?(1)b|a)    a    n    -    -
ok((not ("a" ~~ /(<?null>)[ <(defined $0)> :: b|a ]/)), 're_tests 775  (#971)', :todo<feature>);
# 522: ()?(?(1)a|b)    a    y    $&    a
is(("a" ~~ /(<?null>)?[ <(defined $0)> :: a|b ]/ && $<>), "a", 're_tests 777/0 (#973)', :todo<feature>);
# 523: ^(\()?blah(?(1)(\)))$    (blah)    y    $1    )
is(("(blah)" ~~ /^(\()?blah[ <(defined $0)> :: (\)) ]$/ && $1), ")", 're_tests 779/2 (#975)', :todo<feature>);
# 524: ^(\()?blah(?(1)(\)))$    blah    y    ($1)    ()
# SKIPPED: script doesn't understand `($1)' yet
# SKIPPED: script doesn't understand `($1)' yet
# 525: ^(\()?blah(?(1)(\)))$    blah)    n    -    -
ok((not ("blah)" ~~ /^(\()?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 781  (#977)', :todo<feature>);
# 526: ^(\()?blah(?(1)(\)))$    (blah    n    -    -
ok((not ("(blah" ~~ /^(\()?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 783  (#979)', :todo<feature>);
# 527: ^(\(+)?blah(?(1)(\)))$    (blah)    y    $1    )
is(("(blah)" ~~ /^(\(+)?blah[ <(defined $0)> :: (\)) ]$/ && $1), ")", 're_tests 785/2 (#981)', :todo<feature>);
# 528: ^(\(+)?blah(?(1)(\)))$    blah    y    ($1)    ()
# SKIPPED: script doesn't understand `($1)' yet
# SKIPPED: script doesn't understand `($1)' yet
# 529: ^(\(+)?blah(?(1)(\)))$    blah)    n    -    -
ok((not ("blah)" ~~ /^(\(+)?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 787  (#983)', :todo<feature>);
# 530: ^(\(+)?blah(?(1)(\)))$    (blah    n    -    -
ok((not ("(blah" ~~ /^(\(+)?blah[ <(defined $0)> :: (\)) ]$/)), 're_tests 789  (#985)', :todo<feature>);
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
ok((not ("aaab" ~~ /^<before (a+?)>$0ab/)), 're_tests 809  (#1005)', :todo<feature>);
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
