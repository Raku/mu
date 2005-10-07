#!/usr/bin/pugs

use v6;
use Test;

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
ok((not ("a" ~~ /^(a)?[ <(defined $0)> :: a|b ]+$/)), 're_tests 1217  (#1421)', :todo<feature>);
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
ok((not ("Oo" ~~ rx:i/^(o)<!before \N*$0>/)), 're_tests 1255  (#1473)', :todo<feature>);
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

