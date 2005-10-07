#!/usr/bin/pugs

use v6;
use Test;


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
ok((not ("......abef" ~~ /\N*a<!before (b|cd)*e>\N*f/)), 're_tests 1363  (#1581)', :todo<feature>);
# 944: x(?#    x    c    -    Sequence (?#... not terminated
# -- SKIPPED - TESTS ERROR MESSAGE
# 945: :x(?#:    x    c    -    Sequence (?#... not terminated
# -- SKIPPED - TESTS ERROR MESSAGE

