#!/usr/bin/pugs

use v6;
use Test;

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

