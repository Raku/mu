use v6-alpha;
use Test;

=pod

This file was originally derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/propcharset_slow_to_compile.t.

=cut

plan 2349;


if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

  force_todo(1..2,7..11,16..20,25..26,28..32,37..41,46..47,49..53,58..59,61..65,70..71,73..77,82..86,91..92,94..98,103..107,112..113,115..119,124..128,133..134,136..140,145..149,154..158,163..164,166..170,175..179,184..185,187..191,196..200,205..206,208..212,217..221,226..230,235..239,244..245,247..251,256..260,265..266,268..272,277..278,280..284,289..290,292..296,301..305,310..314,319..323,328..329,331..335,340..344,349..350,352..356,361..365,370..371,373..377,382..386,391..392,394..398,403..407,412..413,415..419,424..428,433..434,436..440,445..449,454..455,457..461,466..470,475..479,484..488,493..494,496..500,505..506,508..512,517..518,520..524,529..533,538..539,541..545,550..551,553..557,562..563,565..569,574..578,583..587,592..593,595..599,604..605,607..611,616..617,619..623,628..629,631..635,640..641,643..647,652..653,655..659,664..665,667..671,676..680,685..689,694..695,697..701,706..707,709..713,718..719,721..725,730..734,739..743,748..752,757..761,766..770,775..779,784..788,793..797,802..806,811..815,820..824,829..830,832..836,841..842,844..848,853..854,856..858,860..863,868..872,877..881,886..888,890..893,898..902,907..911,916..920,925..929,934..938,943..947,952..956,961..965,970..974,979..983,988..992,997..1001,1006..1007,1009..1013,1018..1019,1021..1025,1030..1031,1033..1037,1042..1043,1045..1049,1054..1055,1057..1061,1066..1068,1070..1073,1078..1082,1087..1091,1096..1097,1099..1103,1108..1109,1111..1115,1120..1124,1129..1133,1138..1142,1147..1151,1156..1160,1165..1169,1174..1178,1183..1187,1192..1196,1201..1205,1210..1214,1219..1223,1228..1232,1237..1241,1246..1250,1255..1259,1264..1268,1273..1277,1282..1286,1291..1292,1294..1296,1298..1299,1301..1304,1309..1313,1318..1322,1327..1331,1336..1340,1345..1349,1354..1358,1363..1367,1372..1376,1381..1382,1384..1388,1393..1397,1402..1406,1411..1415,1420..1424,1429..1430,1432..1436,1441..1445,1450..1451,1453..1457,1462..1466,1471..1475,1479..1481,1486..1490,1495..1499,1504..1505,1507..1509,1511..1514,1519..1521,1523..1524,1526..1529,1534..1538,1543..1547,1552..1556,1561..1565,1570..1574,1579..1583,1588..1592,1597..1601,1606..1610,1615..1617,1619..1622,1627..1629,1631..1632,1634..1635,1637..1640,1645..1649,1654..1658,1663..1667,1672..1674,1676..1679,1684..1688,1693..1697,1702..1704,1706..1709,1714..1718,1723..1727,1732..1736,1741..1743,1745..1748,1753..1757,1762..1766,1771..1775,1780..1784,1789..1793,1798..1802,1807..1811,1816..1818,1820..1823,1828..1832,1837..1841,1846..1850,1855..1857,1859..1862,1867..1871,1876..1880,1885..1889,1894..1898,1903..1905,1907..1908,1910..1913,1918..1922,1927..1931,1936..1940,1945..1949,1954..1958,1963..1967,1972..1976,1981..1985,1990..1994,1999..2003,2008..2012,2017..2021,2026..2030,2035..2039,2044..2046,2048..2051,2056..2058,2060..2063,2068..2072,2077..2081,2086..2090,2095..2099,2104..2108,2113..2115,2117..2120,2125..2129,2134..2138,2143..2145,2147..2150,2155..2159,2164..2166,2168..2171,2176..2180,2185..2187,2189..2192,2197..2199,2201..2204,2209..2213,2218..2222,2227..2231,2236..2238,2240..2241,2243..2246,2251..2255,2260..2264,2269..2271,2273..2276,2281..2285,2290..2294,2299..2303,2308..2312,2317..2321,2326..2328,2330..2333,2338..2342,2347..2349);  # Unicode property character sets are :todo<feature>.


# L           Letter


ok("\x[45CD]" ~~ m/^<+<isL>>$/, q{Match <isL> (Letter)} );
ok("\x[45CD]" ~~ m/^<[A]+<isL>>$/, q{Match compound <isL> (Letter)} );
ok(!( "\x[45CD]" ~~ m/^<-<isL>>$/ ), q{Don't match externally inverted <isL> (Letter)} );
ok(!( "\x[45CD]" ~~ m/^<[A]-<isL>>$/ ), q{Don't match compound inverted <isL> (Letter)} );
ok(!( "\x[45CD]" ~~ m/^<+<-isL>>$/ ), q{Don't match internally inverted <isL> (Letter)} );
ok(!( "\x[4DB6]"  ~~ m/^<+<isL>>$/ ), q{Don't match unrelated <isL> (Letter)} );
ok("\x[4DB6]"  ~~ m/^<-<isL>>$/, q{Match unrelated externally inverted <isL> (Letter)} );
ok("\x[4DB6]"  ~~ m/^<+<-isL>>$/, q{Match unrelated internally inverted <isL> (Letter)} );
ok("\x[4DB6]\x[45CD]" ~~ m/<+<isL>>/, q{Match unanchored <isL> (Letter)} );

ok("\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<+<?isLetter>>$/, q{Match <?isLetter>} );
ok("\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<[A]+<?isLetter>>$/, q{Match compound <?isLetter>} );
ok(!( "\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<-<?isLetter>>$/ ), q{Don't match externally inverted <?isLetter>} );
ok(!( "\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<[A]-<?isLetter>>$/ ), q{Don't match compound inverted <?isLetter>} );
ok(!( "\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<+<-isLetter>>$/ ), q{Don't match internally inverted <?isLetter>} );
ok(!( "\x[318F]"  ~~ m/^<+<?isLetter>>$/ ), q{Don't match unrelated <?isLetter>} );
ok("\x[318F]"  ~~ m/^<-<?isLetter>>$/, q{Match unrelated externally inverted <?isLetter>} );
ok("\x[318F]"  ~~ m/^<+<-isLetter>>$/, q{Match unrelated internally inverted <?isLetter>} );
ok("\x[318F]\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/<+<?isLetter>>/, q{Match unanchored <?isLetter>} );

# Lu          UppercaseLetter


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?isLu>>$/, q{Match <?isLu> (UppercaseLetter)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?isLu>>$/, q{Match compound <?isLu> (UppercaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?isLu>>$/ ), q{Don't match externally inverted <?isLu> (UppercaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?isLu>>$/ ), q{Don't match compound inverted <?isLu> (UppercaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-isLu>>$/ ), q{Don't match internally inverted <?isLu> (UppercaseLetter)} );
ok(!( "\x[5E52]"  ~~ m/^<+<?isLu>>$/ ), q{Don't match unrelated <?isLu> (UppercaseLetter)} );
ok("\x[5E52]"  ~~ m/^<-<?isLu>>$/, q{Match unrelated externally inverted <?isLu> (UppercaseLetter)} );
ok("\x[5E52]"  ~~ m/^<+<-isLu>>$/, q{Match unrelated internally inverted <?isLu> (UppercaseLetter)} );
ok(!( "\x[5E52]" ~~ m/^<+<?isLu>>$/ ), q{Don't match related <?isLu> (UppercaseLetter)} );
ok("\x[5E52]" ~~ m/^<+<-isLu>>$/, q{Match related internally inverted <?isLu> (UppercaseLetter)} );
ok("\x[5E52]" ~~ m/^<-<?isLu>>$/, q{Match related externally inverted <?isLu> (UppercaseLetter)} );
ok("\x[5E52]\x[5E52]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?isLu>>/, q{Match unanchored <?isLu> (UppercaseLetter)} );

ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<?isUppercaseLetter>>$/, q{Match <?isUppercaseLetter>} );
ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]+<?isUppercaseLetter>>$/, q{Match compound <?isUppercaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<-<?isUppercaseLetter>>$/ ), q{Don't match externally inverted <?isUppercaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]-<?isUppercaseLetter>>$/ ), q{Don't match compound inverted <?isUppercaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<-isUppercaseLetter>>$/ ), q{Don't match internally inverted <?isUppercaseLetter>} );
ok(!( "\x[1DB9]"  ~~ m/^<+<?isUppercaseLetter>>$/ ), q{Don't match unrelated <?isUppercaseLetter>} );
ok("\x[1DB9]"  ~~ m/^<-<?isUppercaseLetter>>$/, q{Match unrelated externally inverted <?isUppercaseLetter>} );
ok("\x[1DB9]"  ~~ m/^<+<-isUppercaseLetter>>$/, q{Match unrelated internally inverted <?isUppercaseLetter>} );
ok("\x[1DB9]\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/<+<?isUppercaseLetter>>/, q{Match unanchored <?isUppercaseLetter>} );

# Ll          LowercaseLetter


ok("\c[LATIN SMALL LETTER A]" ~~ m/^<+<?isLl>>$/, q{Match <?isLl> (LowercaseLetter)} );
ok("\c[LATIN SMALL LETTER A]" ~~ m/^<[A]+<?isLl>>$/, q{Match compound <?isLl> (LowercaseLetter)} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<-<?isLl>>$/ ), q{Don't match externally inverted <?isLl> (LowercaseLetter)} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<[A]-<?isLl>>$/ ), q{Don't match compound inverted <?isLl> (LowercaseLetter)} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<+<-isLl>>$/ ), q{Don't match internally inverted <?isLl> (LowercaseLetter)} );
ok(!( "\x[83AD]"  ~~ m/^<+<?isLl>>$/ ), q{Don't match unrelated <?isLl> (LowercaseLetter)} );
ok("\x[83AD]"  ~~ m/^<-<?isLl>>$/, q{Match unrelated externally inverted <?isLl> (LowercaseLetter)} );
ok("\x[83AD]"  ~~ m/^<+<-isLl>>$/, q{Match unrelated internally inverted <?isLl> (LowercaseLetter)} );
ok(!( "\x[83AD]" ~~ m/^<+<?isLl>>$/ ), q{Don't match related <?isLl> (LowercaseLetter)} );
ok("\x[83AD]" ~~ m/^<+<-isLl>>$/, q{Match related internally inverted <?isLl> (LowercaseLetter)} );
ok("\x[83AD]" ~~ m/^<-<?isLl>>$/, q{Match related externally inverted <?isLl> (LowercaseLetter)} );
ok("\x[83AD]\x[83AD]\c[LATIN SMALL LETTER A]" ~~ m/<+<?isLl>>/, q{Match unanchored <?isLl> (LowercaseLetter)} );

ok("\c[LATIN SMALL LETTER A]" ~~ m/^<+<?isLowercaseLetter>>$/, q{Match <?isLowercaseLetter>} );
ok("\c[LATIN SMALL LETTER A]" ~~ m/^<[A]+<?isLowercaseLetter>>$/, q{Match compound <?isLowercaseLetter>} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<-<?isLowercaseLetter>>$/ ), q{Don't match externally inverted <?isLowercaseLetter>} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<[A]-<?isLowercaseLetter>>$/ ), q{Don't match compound inverted <?isLowercaseLetter>} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<+<-isLowercaseLetter>>$/ ), q{Don't match internally inverted <?isLowercaseLetter>} );
ok(!( "\x[A9A8]"  ~~ m/^<+<?isLowercaseLetter>>$/ ), q{Don't match unrelated <?isLowercaseLetter>} );
ok("\x[A9A8]"  ~~ m/^<-<?isLowercaseLetter>>$/, q{Match unrelated externally inverted <?isLowercaseLetter>} );
ok("\x[A9A8]"  ~~ m/^<+<-isLowercaseLetter>>$/, q{Match unrelated internally inverted <?isLowercaseLetter>} );
ok(!( "\x[AC00]" ~~ m/^<+<?isLowercaseLetter>>$/ ), q{Don't match related <?isLowercaseLetter>} );
ok("\x[AC00]" ~~ m/^<+<-isLowercaseLetter>>$/, q{Match related internally inverted <?isLowercaseLetter>} );
ok("\x[AC00]" ~~ m/^<-<?isLowercaseLetter>>$/, q{Match related externally inverted <?isLowercaseLetter>} );
ok("\x[A9A8]\x[AC00]\c[LATIN SMALL LETTER A]" ~~ m/<+<?isLowercaseLetter>>/, q{Match unanchored <?isLowercaseLetter>} );

# Lt          TitlecaseLetter


ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<?isLt>>$/, q{Match <?isLt> (TitlecaseLetter)} );
ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]+<?isLt>>$/, q{Match compound <?isLt> (TitlecaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<-<?isLt>>$/ ), q{Don't match externally inverted <?isLt> (TitlecaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]-<?isLt>>$/ ), q{Don't match compound inverted <?isLt> (TitlecaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<-isLt>>$/ ), q{Don't match internally inverted <?isLt> (TitlecaseLetter)} );
ok(!( "\x[D187]"  ~~ m/^<+<?isLt>>$/ ), q{Don't match unrelated <?isLt> (TitlecaseLetter)} );
ok("\x[D187]"  ~~ m/^<-<?isLt>>$/, q{Match unrelated externally inverted <?isLt> (TitlecaseLetter)} );
ok("\x[D187]"  ~~ m/^<+<-isLt>>$/, q{Match unrelated internally inverted <?isLt> (TitlecaseLetter)} );
ok(!( "\x[D187]" ~~ m/^<+<?isLt>>$/ ), q{Don't match related <?isLt> (TitlecaseLetter)} );
ok("\x[D187]" ~~ m/^<+<-isLt>>$/, q{Match related internally inverted <?isLt> (TitlecaseLetter)} );
ok("\x[D187]" ~~ m/^<-<?isLt>>$/, q{Match related externally inverted <?isLt> (TitlecaseLetter)} );
ok("\x[D187]\x[D187]\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/<+<?isLt>>/, q{Match unanchored <?isLt> (TitlecaseLetter)} );

ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<?isTitlecaseLetter>>$/, q{Match <?isTitlecaseLetter>} );
ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]+<?isTitlecaseLetter>>$/, q{Match compound <?isTitlecaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<-<?isTitlecaseLetter>>$/ ), q{Don't match externally inverted <?isTitlecaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]-<?isTitlecaseLetter>>$/ ), q{Don't match compound inverted <?isTitlecaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<-isTitlecaseLetter>>$/ ), q{Don't match internally inverted <?isTitlecaseLetter>} );
ok(!( "\x[C2A9]"  ~~ m/^<+<?isTitlecaseLetter>>$/ ), q{Don't match unrelated <?isTitlecaseLetter>} );
ok("\x[C2A9]"  ~~ m/^<-<?isTitlecaseLetter>>$/, q{Match unrelated externally inverted <?isTitlecaseLetter>} );
ok("\x[C2A9]"  ~~ m/^<+<-isTitlecaseLetter>>$/, q{Match unrelated internally inverted <?isTitlecaseLetter>} );
ok("\x[C2A9]\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/<+<?isTitlecaseLetter>>/, q{Match unanchored <?isTitlecaseLetter>} );

# Lm          ModifierLetter


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?isLm>>$/, q{Match <?isLm> (ModifierLetter)} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?isLm>>$/, q{Match compound <?isLm> (ModifierLetter)} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?isLm>>$/ ), q{Don't match externally inverted <?isLm> (ModifierLetter)} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?isLm>>$/ ), q{Don't match compound inverted <?isLm> (ModifierLetter)} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-isLm>>$/ ), q{Don't match internally inverted <?isLm> (ModifierLetter)} );
ok(!( "\x[8C34]"  ~~ m/^<+<?isLm>>$/ ), q{Don't match unrelated <?isLm> (ModifierLetter)} );
ok("\x[8C34]"  ~~ m/^<-<?isLm>>$/, q{Match unrelated externally inverted <?isLm> (ModifierLetter)} );
ok("\x[8C34]"  ~~ m/^<+<-isLm>>$/, q{Match unrelated internally inverted <?isLm> (ModifierLetter)} );
ok(!( "\x[8C34]" ~~ m/^<+<?isLm>>$/ ), q{Don't match related <?isLm> (ModifierLetter)} );
ok("\x[8C34]" ~~ m/^<+<-isLm>>$/, q{Match related internally inverted <?isLm> (ModifierLetter)} );
ok("\x[8C34]" ~~ m/^<-<?isLm>>$/, q{Match related externally inverted <?isLm> (ModifierLetter)} );
ok("\x[8C34]\x[8C34]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?isLm>>/, q{Match unanchored <?isLm> (ModifierLetter)} );

ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?isModifierLetter>>$/, q{Match <?isModifierLetter>} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?isModifierLetter>>$/, q{Match compound <?isModifierLetter>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?isModifierLetter>>$/ ), q{Don't match externally inverted <?isModifierLetter>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?isModifierLetter>>$/ ), q{Don't match compound inverted <?isModifierLetter>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-isModifierLetter>>$/ ), q{Don't match internally inverted <?isModifierLetter>} );
ok(!( "\c[YI SYLLABLE NZAX]"  ~~ m/^<+<?isModifierLetter>>$/ ), q{Don't match unrelated <?isModifierLetter>} );
ok("\c[YI SYLLABLE NZAX]"  ~~ m/^<-<?isModifierLetter>>$/, q{Match unrelated externally inverted <?isModifierLetter>} );
ok("\c[YI SYLLABLE NZAX]"  ~~ m/^<+<-isModifierLetter>>$/, q{Match unrelated internally inverted <?isModifierLetter>} );
ok("\c[YI SYLLABLE NZAX]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?isModifierLetter>>/, q{Match unanchored <?isModifierLetter>} );

# Lo          OtherLetter


ok("\x[8CC9]" ~~ m/^<+<?isLo>>$/, q{Match <?isLo> (OtherLetter)} );
ok("\x[8CC9]" ~~ m/^<[A]+<?isLo>>$/, q{Match compound <?isLo> (OtherLetter)} );
ok(!( "\x[8CC9]" ~~ m/^<-<?isLo>>$/ ), q{Don't match externally inverted <?isLo> (OtherLetter)} );
ok(!( "\x[8CC9]" ~~ m/^<[A]-<?isLo>>$/ ), q{Don't match compound inverted <?isLo> (OtherLetter)} );
ok(!( "\x[8CC9]" ~~ m/^<+<-isLo>>$/ ), q{Don't match internally inverted <?isLo> (OtherLetter)} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?isLo>>$/ ), q{Don't match unrelated <?isLo> (OtherLetter)} );
ok("\x[9FA6]"  ~~ m/^<-<?isLo>>$/, q{Match unrelated externally inverted <?isLo> (OtherLetter)} );
ok("\x[9FA6]"  ~~ m/^<+<-isLo>>$/, q{Match unrelated internally inverted <?isLo> (OtherLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?isLo>>$/ ), q{Don't match related <?isLo> (OtherLetter)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-isLo>>$/, q{Match related internally inverted <?isLo> (OtherLetter)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?isLo>>$/, q{Match related externally inverted <?isLo> (OtherLetter)} );
ok("\x[9FA6]\c[LATIN CAPITAL LETTER A]\x[8CC9]" ~~ m/<+<?isLo>>/, q{Match unanchored <?isLo> (OtherLetter)} );

ok("\x[BC7D]" ~~ m/^<+<?isOtherLetter>>$/, q{Match <?isOtherLetter>} );
ok("\x[BC7D]" ~~ m/^<[A]+<?isOtherLetter>>$/, q{Match compound <?isOtherLetter>} );
ok(!( "\x[BC7D]" ~~ m/^<-<?isOtherLetter>>$/ ), q{Don't match externally inverted <?isOtherLetter>} );
ok(!( "\x[BC7D]" ~~ m/^<[A]-<?isOtherLetter>>$/ ), q{Don't match compound inverted <?isOtherLetter>} );
ok(!( "\x[BC7D]" ~~ m/^<+<-isOtherLetter>>$/ ), q{Don't match internally inverted <?isOtherLetter>} );
ok(!( "\x[D7A4]"  ~~ m/^<+<?isOtherLetter>>$/ ), q{Don't match unrelated <?isOtherLetter>} );
ok("\x[D7A4]"  ~~ m/^<-<?isOtherLetter>>$/, q{Match unrelated externally inverted <?isOtherLetter>} );
ok("\x[D7A4]"  ~~ m/^<+<-isOtherLetter>>$/, q{Match unrelated internally inverted <?isOtherLetter>} );
ok("\x[D7A4]\x[BC7D]" ~~ m/<+<?isOtherLetter>>/, q{Match unanchored <?isOtherLetter>} );

# Lr             # Alias for "Ll", "Lu", and "Lt".


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?isLr>>$/, q{Match (Alias for "Ll", "Lu", and "Lt".)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?isLr>>$/, q{Match compound (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?isLr>>$/ ), q{Don't match externally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?isLr>>$/ ), q{Don't match compound inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-isLr>>$/ ), q{Don't match internally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\x[CD29]"  ~~ m/^<+<?isLr>>$/ ), q{Don't match unrelated (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]"  ~~ m/^<-<?isLr>>$/, q{Match unrelated externally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]"  ~~ m/^<+<-isLr>>$/, q{Match unrelated internally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\x[CD29]" ~~ m/^<+<?isLr>>$/ ), q{Don't match related (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]" ~~ m/^<+<-isLr>>$/, q{Match related internally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]" ~~ m/^<-<?isLr>>$/, q{Match related externally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]\x[CD29]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?isLr>>/, q{Match unanchored (Alias for "Ll", "Lu", and "Lt".)} );


# M           Mark


ok("\c[TAGALOG VOWEL SIGN I]" ~~ m/^<+<isM>>$/, q{Match <isM> (Mark)} );
ok("\c[TAGALOG VOWEL SIGN I]" ~~ m/^<[A]+<isM>>$/, q{Match compound <isM> (Mark)} );
ok(!( "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<-<isM>>$/ ), q{Don't match externally inverted <isM> (Mark)} );
ok(!( "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<[A]-<isM>>$/ ), q{Don't match compound inverted <isM> (Mark)} );
ok(!( "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<+<-isM>>$/ ), q{Don't match internally inverted <isM> (Mark)} );
ok(!( "\c[CANADIAN SYLLABICS KAAI]"  ~~ m/^<+<isM>>$/ ), q{Don't match unrelated <isM> (Mark)} );
ok("\c[CANADIAN SYLLABICS KAAI]"  ~~ m/^<-<isM>>$/, q{Match unrelated externally inverted <isM> (Mark)} );
ok("\c[CANADIAN SYLLABICS KAAI]"  ~~ m/^<+<-isM>>$/, q{Match unrelated internally inverted <isM> (Mark)} );
ok("\c[CANADIAN SYLLABICS KAAI]\c[TAGALOG VOWEL SIGN I]" ~~ m/<+<isM>>/, q{Match unanchored <isM> (Mark)} );

ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isMark>>$/, q{Match <?isMark>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?isMark>>$/, q{Match compound <?isMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isMark>>$/ ), q{Don't match externally inverted <?isMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?isMark>>$/ ), q{Don't match compound inverted <?isMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isMark>>$/ ), q{Don't match internally inverted <?isMark>} );
ok(!( "\x[4BF0]"  ~~ m/^<+<?isMark>>$/ ), q{Don't match unrelated <?isMark>} );
ok("\x[4BF0]"  ~~ m/^<-<?isMark>>$/, q{Match unrelated externally inverted <?isMark>} );
ok("\x[4BF0]"  ~~ m/^<+<-isMark>>$/, q{Match unrelated internally inverted <?isMark>} );
ok("\x[4BF0]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?isMark>>/, q{Match unanchored <?isMark>} );

# Mn          NonspacingMark


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isMn>>$/, q{Match <?isMn> (NonspacingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?isMn>>$/, q{Match compound <?isMn> (NonspacingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isMn>>$/ ), q{Don't match externally inverted <?isMn> (NonspacingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?isMn>>$/ ), q{Don't match compound inverted <?isMn> (NonspacingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isMn>>$/ ), q{Don't match internally inverted <?isMn> (NonspacingMark)} );
ok(!( "\x[CF2C]"  ~~ m/^<+<?isMn>>$/ ), q{Don't match unrelated <?isMn> (NonspacingMark)} );
ok("\x[CF2C]"  ~~ m/^<-<?isMn>>$/, q{Match unrelated externally inverted <?isMn> (NonspacingMark)} );
ok("\x[CF2C]"  ~~ m/^<+<-isMn>>$/, q{Match unrelated internally inverted <?isMn> (NonspacingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<?isMn>>$/ ), q{Don't match related <?isMn> (NonspacingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<-isMn>>$/, q{Match related internally inverted <?isMn> (NonspacingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-<?isMn>>$/, q{Match related externally inverted <?isMn> (NonspacingMark)} );
ok("\x[CF2C]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?isMn>>/, q{Match unanchored <?isMn> (NonspacingMark)} );

ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isNonspacingMark>>$/, q{Match <?isNonspacingMark>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?isNonspacingMark>>$/, q{Match compound <?isNonspacingMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isNonspacingMark>>$/ ), q{Don't match externally inverted <?isNonspacingMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?isNonspacingMark>>$/ ), q{Don't match compound inverted <?isNonspacingMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isNonspacingMark>>$/ ), q{Don't match internally inverted <?isNonspacingMark>} );
ok(!( "\x[B617]"  ~~ m/^<+<?isNonspacingMark>>$/ ), q{Don't match unrelated <?isNonspacingMark>} );
ok("\x[B617]"  ~~ m/^<-<?isNonspacingMark>>$/, q{Match unrelated externally inverted <?isNonspacingMark>} );
ok("\x[B617]"  ~~ m/^<+<-isNonspacingMark>>$/, q{Match unrelated internally inverted <?isNonspacingMark>} );
ok("\x[B617]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?isNonspacingMark>>/, q{Match unanchored <?isNonspacingMark>} );

# Mc          SpacingMark


ok("\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<+<?isMc>>$/, q{Match <?isMc> (SpacingMark)} );
ok("\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<[A]+<?isMc>>$/, q{Match compound <?isMc> (SpacingMark)} );
ok(!( "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<-<?isMc>>$/ ), q{Don't match externally inverted <?isMc> (SpacingMark)} );
ok(!( "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<[A]-<?isMc>>$/ ), q{Don't match compound inverted <?isMc> (SpacingMark)} );
ok(!( "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<+<-isMc>>$/ ), q{Don't match internally inverted <?isMc> (SpacingMark)} );
ok(!( "\c[BALLOT BOX WITH CHECK]"  ~~ m/^<+<?isMc>>$/ ), q{Don't match unrelated <?isMc> (SpacingMark)} );
ok("\c[BALLOT BOX WITH CHECK]"  ~~ m/^<-<?isMc>>$/, q{Match unrelated externally inverted <?isMc> (SpacingMark)} );
ok("\c[BALLOT BOX WITH CHECK]"  ~~ m/^<+<-isMc>>$/, q{Match unrelated internally inverted <?isMc> (SpacingMark)} );
ok(!( "\c[IDEOGRAPHIC LEVEL TONE MARK]" ~~ m/^<+<?isMc>>$/ ), q{Don't match related <?isMc> (SpacingMark)} );
ok("\c[IDEOGRAPHIC LEVEL TONE MARK]" ~~ m/^<+<-isMc>>$/, q{Match related internally inverted <?isMc> (SpacingMark)} );
ok("\c[IDEOGRAPHIC LEVEL TONE MARK]" ~~ m/^<-<?isMc>>$/, q{Match related externally inverted <?isMc> (SpacingMark)} );
ok("\c[BALLOT BOX WITH CHECK]\c[IDEOGRAPHIC LEVEL TONE MARK]\c[DEVANAGARI SIGN VISARGA]" ~~ m/<+<?isMc>>/, q{Match unanchored <?isMc> (SpacingMark)} );

ok("\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<+<?isSpacingMark>>$/, q{Match <?isSpacingMark>} );
ok("\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<[A]+<?isSpacingMark>>$/, q{Match compound <?isSpacingMark>} );
ok(!( "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<-<?isSpacingMark>>$/ ), q{Don't match externally inverted <?isSpacingMark>} );
ok(!( "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<[A]-<?isSpacingMark>>$/ ), q{Don't match compound inverted <?isSpacingMark>} );
ok(!( "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<+<-isSpacingMark>>$/ ), q{Don't match internally inverted <?isSpacingMark>} );
ok(!( "\c[KANNADA LETTER VOCALIC LL]"  ~~ m/^<+<?isSpacingMark>>$/ ), q{Don't match unrelated <?isSpacingMark>} );
ok("\c[KANNADA LETTER VOCALIC LL]"  ~~ m/^<-<?isSpacingMark>>$/, q{Match unrelated externally inverted <?isSpacingMark>} );
ok("\c[KANNADA LETTER VOCALIC LL]"  ~~ m/^<+<-isSpacingMark>>$/, q{Match unrelated internally inverted <?isSpacingMark>} );
ok("\c[KANNADA LETTER VOCALIC LL]\c[MALAYALAM SIGN ANUSVARA]" ~~ m/<+<?isSpacingMark>>/, q{Match unanchored <?isSpacingMark>} );

# Me          EnclosingMark


ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<?isMe>>$/, q{Match <?isMe> (EnclosingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]+<?isMe>>$/, q{Match compound <?isMe> (EnclosingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-<?isMe>>$/ ), q{Don't match externally inverted <?isMe> (EnclosingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]-<?isMe>>$/ ), q{Don't match compound inverted <?isMe> (EnclosingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<-isMe>>$/ ), q{Don't match internally inverted <?isMe> (EnclosingMark)} );
ok(!( "\x[C680]"  ~~ m/^<+<?isMe>>$/ ), q{Don't match unrelated <?isMe> (EnclosingMark)} );
ok("\x[C680]"  ~~ m/^<-<?isMe>>$/, q{Match unrelated externally inverted <?isMe> (EnclosingMark)} );
ok("\x[C680]"  ~~ m/^<+<-isMe>>$/, q{Match unrelated internally inverted <?isMe> (EnclosingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isMe>>$/ ), q{Don't match related <?isMe> (EnclosingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isMe>>$/, q{Match related internally inverted <?isMe> (EnclosingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isMe>>$/, q{Match related externally inverted <?isMe> (EnclosingMark)} );
ok("\x[C680]\c[COMBINING GRAVE ACCENT]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/<+<?isMe>>/, q{Match unanchored <?isMe> (EnclosingMark)} );

ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<?isEnclosingMark>>$/, q{Match <?isEnclosingMark>} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]+<?isEnclosingMark>>$/, q{Match compound <?isEnclosingMark>} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-<?isEnclosingMark>>$/ ), q{Don't match externally inverted <?isEnclosingMark>} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]-<?isEnclosingMark>>$/ ), q{Don't match compound inverted <?isEnclosingMark>} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<-isEnclosingMark>>$/ ), q{Don't match internally inverted <?isEnclosingMark>} );
ok(!( "\x[911E]"  ~~ m/^<+<?isEnclosingMark>>$/ ), q{Don't match unrelated <?isEnclosingMark>} );
ok("\x[911E]"  ~~ m/^<-<?isEnclosingMark>>$/, q{Match unrelated externally inverted <?isEnclosingMark>} );
ok("\x[911E]"  ~~ m/^<+<-isEnclosingMark>>$/, q{Match unrelated internally inverted <?isEnclosingMark>} );
ok("\x[911E]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/<+<?isEnclosingMark>>/, q{Match unanchored <?isEnclosingMark>} );

# N           Number


ok("\c[DIGIT ZERO]" ~~ m/^<+<isN>>$/, q{Match <isN> (Number)} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<isN>>$/, q{Match compound <isN> (Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<isN>>$/ ), q{Don't match externally inverted <isN> (Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<isN>>$/ ), q{Don't match compound inverted <isN> (Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-isN>>$/ ), q{Don't match internally inverted <isN> (Number)} );
ok(!( "\x[3BA3]"  ~~ m/^<+<isN>>$/ ), q{Don't match unrelated <isN> (Number)} );
ok("\x[3BA3]"  ~~ m/^<-<isN>>$/, q{Match unrelated externally inverted <isN> (Number)} );
ok("\x[3BA3]"  ~~ m/^<+<-isN>>$/, q{Match unrelated internally inverted <isN> (Number)} );
ok("\x[3BA3]\c[DIGIT ZERO]" ~~ m/<+<isN>>/, q{Match unanchored <isN> (Number)} );

ok("\c[DIGIT ZERO]" ~~ m/^<+<?isNumber>>$/, q{Match <?isNumber>} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?isNumber>>$/, q{Match compound <?isNumber>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?isNumber>>$/ ), q{Don't match externally inverted <?isNumber>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?isNumber>>$/ ), q{Don't match compound inverted <?isNumber>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-isNumber>>$/ ), q{Don't match internally inverted <?isNumber>} );
ok(!( "\x[37D0]"  ~~ m/^<+<?isNumber>>$/ ), q{Don't match unrelated <?isNumber>} );
ok("\x[37D0]"  ~~ m/^<-<?isNumber>>$/, q{Match unrelated externally inverted <?isNumber>} );
ok("\x[37D0]"  ~~ m/^<+<-isNumber>>$/, q{Match unrelated internally inverted <?isNumber>} );
ok("\x[37D0]\c[DIGIT ZERO]" ~~ m/<+<?isNumber>>/, q{Match unanchored <?isNumber>} );

# Nd          DecimalNumber


ok("\c[DIGIT ZERO]" ~~ m/^<+<?isNd>>$/, q{Match <?isNd> (DecimalNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?isNd>>$/, q{Match compound <?isNd> (DecimalNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?isNd>>$/ ), q{Don't match externally inverted <?isNd> (DecimalNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?isNd>>$/ ), q{Don't match compound inverted <?isNd> (DecimalNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-isNd>>$/ ), q{Don't match internally inverted <?isNd> (DecimalNumber)} );
ok(!( "\x[8536]"  ~~ m/^<+<?isNd>>$/ ), q{Don't match unrelated <?isNd> (DecimalNumber)} );
ok("\x[8536]"  ~~ m/^<-<?isNd>>$/, q{Match unrelated externally inverted <?isNd> (DecimalNumber)} );
ok("\x[8536]"  ~~ m/^<+<-isNd>>$/, q{Match unrelated internally inverted <?isNd> (DecimalNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<+<?isNd>>$/ ), q{Don't match related <?isNd> (DecimalNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<+<-isNd>>$/, q{Match related internally inverted <?isNd> (DecimalNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<-<?isNd>>$/, q{Match related externally inverted <?isNd> (DecimalNumber)} );
ok("\x[8536]\c[SUPERSCRIPT TWO]\c[DIGIT ZERO]" ~~ m/<+<?isNd>>/, q{Match unanchored <?isNd> (DecimalNumber)} );

ok("\c[KHMER DIGIT ZERO]" ~~ m/^<+<?isDecimalNumber>>$/, q{Match <?isDecimalNumber>} );
ok("\c[KHMER DIGIT ZERO]" ~~ m/^<[A]+<?isDecimalNumber>>$/, q{Match compound <?isDecimalNumber>} );
ok(!( "\c[KHMER DIGIT ZERO]" ~~ m/^<-<?isDecimalNumber>>$/ ), q{Don't match externally inverted <?isDecimalNumber>} );
ok(!( "\c[KHMER DIGIT ZERO]" ~~ m/^<[A]-<?isDecimalNumber>>$/ ), q{Don't match compound inverted <?isDecimalNumber>} );
ok(!( "\c[KHMER DIGIT ZERO]" ~~ m/^<+<-isDecimalNumber>>$/ ), q{Don't match internally inverted <?isDecimalNumber>} );
ok(!( "\c[CANADIAN SYLLABICS NWE]"  ~~ m/^<+<?isDecimalNumber>>$/ ), q{Don't match unrelated <?isDecimalNumber>} );
ok("\c[CANADIAN SYLLABICS NWE]"  ~~ m/^<-<?isDecimalNumber>>$/, q{Match unrelated externally inverted <?isDecimalNumber>} );
ok("\c[CANADIAN SYLLABICS NWE]"  ~~ m/^<+<-isDecimalNumber>>$/, q{Match unrelated internally inverted <?isDecimalNumber>} );
ok("\c[CANADIAN SYLLABICS NWE]\c[KHMER DIGIT ZERO]" ~~ m/<+<?isDecimalNumber>>/, q{Match unanchored <?isDecimalNumber>} );

# Nl          LetterNumber


ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<+<?isNl>>$/, q{Match <?isNl> (LetterNumber)} );
ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]+<?isNl>>$/, q{Match compound <?isNl> (LetterNumber)} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<-<?isNl>>$/ ), q{Don't match externally inverted <?isNl> (LetterNumber)} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]-<?isNl>>$/ ), q{Don't match compound inverted <?isNl> (LetterNumber)} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<+<-isNl>>$/ ), q{Don't match internally inverted <?isNl> (LetterNumber)} );
ok(!( "\x[1B95]"  ~~ m/^<+<?isNl>>$/ ), q{Don't match unrelated <?isNl> (LetterNumber)} );
ok("\x[1B95]"  ~~ m/^<-<?isNl>>$/, q{Match unrelated externally inverted <?isNl> (LetterNumber)} );
ok("\x[1B95]"  ~~ m/^<+<-isNl>>$/, q{Match unrelated internally inverted <?isNl> (LetterNumber)} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<+<?isNl>>$/ ), q{Don't match related <?isNl> (LetterNumber)} );
ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<+<-isNl>>$/, q{Match related internally inverted <?isNl> (LetterNumber)} );
ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<-<?isNl>>$/, q{Match related externally inverted <?isNl> (LetterNumber)} );
ok("\x[1B95]\c[SUPERSCRIPT ZERO]\c[ROMAN NUMERAL ONE]" ~~ m/<+<?isNl>>/, q{Match unanchored <?isNl> (LetterNumber)} );

ok("\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<+<?isLetterNumber>>$/, q{Match <?isLetterNumber>} );
ok("\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<[A]+<?isLetterNumber>>$/, q{Match compound <?isLetterNumber>} );
ok(!( "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<-<?isLetterNumber>>$/ ), q{Don't match externally inverted <?isLetterNumber>} );
ok(!( "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<[A]-<?isLetterNumber>>$/ ), q{Don't match compound inverted <?isLetterNumber>} );
ok(!( "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<+<-isLetterNumber>>$/ ), q{Don't match internally inverted <?isLetterNumber>} );
ok(!( "\x[9B4F]"  ~~ m/^<+<?isLetterNumber>>$/ ), q{Don't match unrelated <?isLetterNumber>} );
ok("\x[9B4F]"  ~~ m/^<-<?isLetterNumber>>$/, q{Match unrelated externally inverted <?isLetterNumber>} );
ok("\x[9B4F]"  ~~ m/^<+<-isLetterNumber>>$/, q{Match unrelated internally inverted <?isLetterNumber>} );
ok(!( "\x[9B4F]" ~~ m/^<+<?isLetterNumber>>$/ ), q{Don't match related <?isLetterNumber>} );
ok("\x[9B4F]" ~~ m/^<+<-isLetterNumber>>$/, q{Match related internally inverted <?isLetterNumber>} );
ok("\x[9B4F]" ~~ m/^<-<?isLetterNumber>>$/, q{Match related externally inverted <?isLetterNumber>} );
ok("\x[9B4F]\x[9B4F]\c[RUNIC ARLAUG SYMBOL]" ~~ m/<+<?isLetterNumber>>/, q{Match unanchored <?isLetterNumber>} );

# No          OtherNumber


ok("\c[SUPERSCRIPT TWO]" ~~ m/^<+<?isNo>>$/, q{Match <?isNo> (OtherNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<[A]+<?isNo>>$/, q{Match compound <?isNo> (OtherNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<-<?isNo>>$/ ), q{Don't match externally inverted <?isNo> (OtherNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<[A]-<?isNo>>$/ ), q{Don't match compound inverted <?isNo> (OtherNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<+<-isNo>>$/ ), q{Don't match internally inverted <?isNo> (OtherNumber)} );
ok(!( "\x[8F9A]"  ~~ m/^<+<?isNo>>$/ ), q{Don't match unrelated <?isNo> (OtherNumber)} );
ok("\x[8F9A]"  ~~ m/^<-<?isNo>>$/, q{Match unrelated externally inverted <?isNo> (OtherNumber)} );
ok("\x[8F9A]"  ~~ m/^<+<-isNo>>$/, q{Match unrelated internally inverted <?isNo> (OtherNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<?isNo>>$/ ), q{Don't match related <?isNo> (OtherNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<+<-isNo>>$/, q{Match related internally inverted <?isNo> (OtherNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<-<?isNo>>$/, q{Match related externally inverted <?isNo> (OtherNumber)} );
ok("\x[8F9A]\c[DIGIT ZERO]\c[SUPERSCRIPT TWO]" ~~ m/<+<?isNo>>/, q{Match unanchored <?isNo> (OtherNumber)} );

ok("\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<+<?isOtherNumber>>$/, q{Match <?isOtherNumber>} );
ok("\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<[A]+<?isOtherNumber>>$/, q{Match compound <?isOtherNumber>} );
ok(!( "\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<-<?isOtherNumber>>$/ ), q{Don't match externally inverted <?isOtherNumber>} );
ok(!( "\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<[A]-<?isOtherNumber>>$/ ), q{Don't match compound inverted <?isOtherNumber>} );
ok(!( "\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<+<-isOtherNumber>>$/ ), q{Don't match internally inverted <?isOtherNumber>} );
ok(!( "\x[0522]"  ~~ m/^<+<?isOtherNumber>>$/ ), q{Don't match unrelated <?isOtherNumber>} );
ok("\x[0522]"  ~~ m/^<-<?isOtherNumber>>$/, q{Match unrelated externally inverted <?isOtherNumber>} );
ok("\x[0522]"  ~~ m/^<+<-isOtherNumber>>$/, q{Match unrelated internally inverted <?isOtherNumber>} );
ok("\x[0522]\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/<+<?isOtherNumber>>/, q{Match unanchored <?isOtherNumber>} );

# P           Punctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<+<isP>>$/, q{Match <isP> (Punctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<isP>>$/, q{Match compound <isP> (Punctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<isP>>$/ ), q{Don't match externally inverted <isP> (Punctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<isP>>$/ ), q{Don't match compound inverted <isP> (Punctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-isP>>$/ ), q{Don't match internally inverted <isP> (Punctuation)} );
ok(!( "\x[3753]"  ~~ m/^<+<isP>>$/ ), q{Don't match unrelated <isP> (Punctuation)} );
ok("\x[3753]"  ~~ m/^<-<isP>>$/, q{Match unrelated externally inverted <isP> (Punctuation)} );
ok("\x[3753]"  ~~ m/^<+<-isP>>$/, q{Match unrelated internally inverted <isP> (Punctuation)} );
ok("\x[3753]\c[EXCLAMATION MARK]" ~~ m/<+<isP>>/, q{Match unanchored <isP> (Punctuation)} );

ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?isPunctuation>>$/, q{Match <?isPunctuation>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?isPunctuation>>$/, q{Match compound <?isPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?isPunctuation>>$/ ), q{Don't match externally inverted <?isPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?isPunctuation>>$/ ), q{Don't match compound inverted <?isPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-isPunctuation>>$/ ), q{Don't match internally inverted <?isPunctuation>} );
ok(!( "\x[9C5E]"  ~~ m/^<+<?isPunctuation>>$/ ), q{Don't match unrelated <?isPunctuation>} );
ok("\x[9C5E]"  ~~ m/^<-<?isPunctuation>>$/, q{Match unrelated externally inverted <?isPunctuation>} );
ok("\x[9C5E]"  ~~ m/^<+<-isPunctuation>>$/, q{Match unrelated internally inverted <?isPunctuation>} );
ok("\x[9C5E]\c[EXCLAMATION MARK]" ~~ m/<+<?isPunctuation>>/, q{Match unanchored <?isPunctuation>} );

# Pc          ConnectorPunctuation


ok("\c[LOW LINE]" ~~ m/^<+<?isPc>>$/, q{Match <?isPc> (ConnectorPunctuation)} );
ok("\c[LOW LINE]" ~~ m/^<[A]+<?isPc>>$/, q{Match compound <?isPc> (ConnectorPunctuation)} );
ok(!( "\c[LOW LINE]" ~~ m/^<-<?isPc>>$/ ), q{Don't match externally inverted <?isPc> (ConnectorPunctuation)} );
ok(!( "\c[LOW LINE]" ~~ m/^<[A]-<?isPc>>$/ ), q{Don't match compound inverted <?isPc> (ConnectorPunctuation)} );
ok(!( "\c[LOW LINE]" ~~ m/^<+<-isPc>>$/ ), q{Don't match internally inverted <?isPc> (ConnectorPunctuation)} );
ok(!( "\x[B2C9]"  ~~ m/^<+<?isPc>>$/ ), q{Don't match unrelated <?isPc> (ConnectorPunctuation)} );
ok("\x[B2C9]"  ~~ m/^<-<?isPc>>$/, q{Match unrelated externally inverted <?isPc> (ConnectorPunctuation)} );
ok("\x[B2C9]"  ~~ m/^<+<-isPc>>$/, q{Match unrelated internally inverted <?isPc> (ConnectorPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?isPc>>$/ ), q{Don't match related <?isPc> (ConnectorPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-isPc>>$/, q{Match related internally inverted <?isPc> (ConnectorPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?isPc>>$/, q{Match related externally inverted <?isPc> (ConnectorPunctuation)} );
ok("\x[B2C9]\c[EXCLAMATION MARK]\c[LOW LINE]" ~~ m/<+<?isPc>>/, q{Match unanchored <?isPc> (ConnectorPunctuation)} );

ok("\c[LOW LINE]" ~~ m/^<+<?isConnectorPunctuation>>$/, q{Match <?isConnectorPunctuation>} );
ok("\c[LOW LINE]" ~~ m/^<[A]+<?isConnectorPunctuation>>$/, q{Match compound <?isConnectorPunctuation>} );
ok(!( "\c[LOW LINE]" ~~ m/^<-<?isConnectorPunctuation>>$/ ), q{Don't match externally inverted <?isConnectorPunctuation>} );
ok(!( "\c[LOW LINE]" ~~ m/^<[A]-<?isConnectorPunctuation>>$/ ), q{Don't match compound inverted <?isConnectorPunctuation>} );
ok(!( "\c[LOW LINE]" ~~ m/^<+<-isConnectorPunctuation>>$/ ), q{Don't match internally inverted <?isConnectorPunctuation>} );
ok(!( "\x[AEFC]"  ~~ m/^<+<?isConnectorPunctuation>>$/ ), q{Don't match unrelated <?isConnectorPunctuation>} );
ok("\x[AEFC]"  ~~ m/^<-<?isConnectorPunctuation>>$/, q{Match unrelated externally inverted <?isConnectorPunctuation>} );
ok("\x[AEFC]"  ~~ m/^<+<-isConnectorPunctuation>>$/, q{Match unrelated internally inverted <?isConnectorPunctuation>} );
ok("\x[AEFC]\c[LOW LINE]" ~~ m/<+<?isConnectorPunctuation>>/, q{Match unanchored <?isConnectorPunctuation>} );

# Pd          DashPunctuation


ok("\c[HYPHEN-MINUS]" ~~ m/^<+<?isPd>>$/, q{Match <?isPd> (DashPunctuation)} );
ok("\c[HYPHEN-MINUS]" ~~ m/^<[A]+<?isPd>>$/, q{Match compound <?isPd> (DashPunctuation)} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<-<?isPd>>$/ ), q{Don't match externally inverted <?isPd> (DashPunctuation)} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<[A]-<?isPd>>$/ ), q{Don't match compound inverted <?isPd> (DashPunctuation)} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<+<-isPd>>$/ ), q{Don't match internally inverted <?isPd> (DashPunctuation)} );
ok(!( "\x[86C8]"  ~~ m/^<+<?isPd>>$/ ), q{Don't match unrelated <?isPd> (DashPunctuation)} );
ok("\x[86C8]"  ~~ m/^<-<?isPd>>$/, q{Match unrelated externally inverted <?isPd> (DashPunctuation)} );
ok("\x[86C8]"  ~~ m/^<+<-isPd>>$/, q{Match unrelated internally inverted <?isPd> (DashPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?isPd>>$/ ), q{Don't match related <?isPd> (DashPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-isPd>>$/, q{Match related internally inverted <?isPd> (DashPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?isPd>>$/, q{Match related externally inverted <?isPd> (DashPunctuation)} );
ok("\x[86C8]\c[EXCLAMATION MARK]\c[HYPHEN-MINUS]" ~~ m/<+<?isPd>>/, q{Match unanchored <?isPd> (DashPunctuation)} );

ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<?isDashPunctuation>>$/, q{Match <?isDashPunctuation>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]+<?isDashPunctuation>>$/, q{Match compound <?isDashPunctuation>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<-<?isDashPunctuation>>$/ ), q{Don't match externally inverted <?isDashPunctuation>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]-<?isDashPunctuation>>$/ ), q{Don't match compound inverted <?isDashPunctuation>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<-isDashPunctuation>>$/ ), q{Don't match internally inverted <?isDashPunctuation>} );
ok(!( "\c[HIRAGANA LETTER NI]"  ~~ m/^<+<?isDashPunctuation>>$/ ), q{Don't match unrelated <?isDashPunctuation>} );
ok("\c[HIRAGANA LETTER NI]"  ~~ m/^<-<?isDashPunctuation>>$/, q{Match unrelated externally inverted <?isDashPunctuation>} );
ok("\c[HIRAGANA LETTER NI]"  ~~ m/^<+<-isDashPunctuation>>$/, q{Match unrelated internally inverted <?isDashPunctuation>} );
ok("\c[HIRAGANA LETTER NI]\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/<+<?isDashPunctuation>>/, q{Match unanchored <?isDashPunctuation>} );

# Ps          OpenPunctuation


ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?isPs>>$/, q{Match <?isPs> (OpenPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?isPs>>$/, q{Match compound <?isPs> (OpenPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?isPs>>$/ ), q{Don't match externally inverted <?isPs> (OpenPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?isPs>>$/ ), q{Don't match compound inverted <?isPs> (OpenPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-isPs>>$/ ), q{Don't match internally inverted <?isPs> (OpenPunctuation)} );
ok(!( "\x[B601]"  ~~ m/^<+<?isPs>>$/ ), q{Don't match unrelated <?isPs> (OpenPunctuation)} );
ok("\x[B601]"  ~~ m/^<-<?isPs>>$/, q{Match unrelated externally inverted <?isPs> (OpenPunctuation)} );
ok("\x[B601]"  ~~ m/^<+<-isPs>>$/, q{Match unrelated internally inverted <?isPs> (OpenPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?isPs>>$/ ), q{Don't match related <?isPs> (OpenPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-isPs>>$/, q{Match related internally inverted <?isPs> (OpenPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?isPs>>$/, q{Match related externally inverted <?isPs> (OpenPunctuation)} );
ok("\x[B601]\c[EXCLAMATION MARK]\c[LEFT PARENTHESIS]" ~~ m/<+<?isPs>>/, q{Match unanchored <?isPs> (OpenPunctuation)} );

ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?isOpenPunctuation>>$/, q{Match <?isOpenPunctuation>} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?isOpenPunctuation>>$/, q{Match compound <?isOpenPunctuation>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?isOpenPunctuation>>$/ ), q{Don't match externally inverted <?isOpenPunctuation>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?isOpenPunctuation>>$/ ), q{Don't match compound inverted <?isOpenPunctuation>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-isOpenPunctuation>>$/ ), q{Don't match internally inverted <?isOpenPunctuation>} );
ok(!( "\x[89C1]"  ~~ m/^<+<?isOpenPunctuation>>$/ ), q{Don't match unrelated <?isOpenPunctuation>} );
ok("\x[89C1]"  ~~ m/^<-<?isOpenPunctuation>>$/, q{Match unrelated externally inverted <?isOpenPunctuation>} );
ok("\x[89C1]"  ~~ m/^<+<-isOpenPunctuation>>$/, q{Match unrelated internally inverted <?isOpenPunctuation>} );
ok("\x[89C1]\c[LEFT PARENTHESIS]" ~~ m/<+<?isOpenPunctuation>>/, q{Match unanchored <?isOpenPunctuation>} );

# Pe          ClosePunctuation


ok("\c[RIGHT PARENTHESIS]" ~~ m/^<+<?isPe>>$/, q{Match <?isPe> (ClosePunctuation)} );
ok("\c[RIGHT PARENTHESIS]" ~~ m/^<[A]+<?isPe>>$/, q{Match compound <?isPe> (ClosePunctuation)} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<-<?isPe>>$/ ), q{Don't match externally inverted <?isPe> (ClosePunctuation)} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<[A]-<?isPe>>$/ ), q{Don't match compound inverted <?isPe> (ClosePunctuation)} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<+<-isPe>>$/ ), q{Don't match internally inverted <?isPe> (ClosePunctuation)} );
ok(!( "\x[5561]"  ~~ m/^<+<?isPe>>$/ ), q{Don't match unrelated <?isPe> (ClosePunctuation)} );
ok("\x[5561]"  ~~ m/^<-<?isPe>>$/, q{Match unrelated externally inverted <?isPe> (ClosePunctuation)} );
ok("\x[5561]"  ~~ m/^<+<-isPe>>$/, q{Match unrelated internally inverted <?isPe> (ClosePunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?isPe>>$/ ), q{Don't match related <?isPe> (ClosePunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-isPe>>$/, q{Match related internally inverted <?isPe> (ClosePunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?isPe>>$/, q{Match related externally inverted <?isPe> (ClosePunctuation)} );
ok("\x[5561]\c[EXCLAMATION MARK]\c[RIGHT PARENTHESIS]" ~~ m/<+<?isPe>>/, q{Match unanchored <?isPe> (ClosePunctuation)} );

ok("\c[RIGHT PARENTHESIS]" ~~ m/^<+<?isClosePunctuation>>$/, q{Match <?isClosePunctuation>} );
ok("\c[RIGHT PARENTHESIS]" ~~ m/^<[A]+<?isClosePunctuation>>$/, q{Match compound <?isClosePunctuation>} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<-<?isClosePunctuation>>$/ ), q{Don't match externally inverted <?isClosePunctuation>} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<[A]-<?isClosePunctuation>>$/ ), q{Don't match compound inverted <?isClosePunctuation>} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<+<-isClosePunctuation>>$/ ), q{Don't match internally inverted <?isClosePunctuation>} );
ok(!( "\x[98D0]"  ~~ m/^<+<?isClosePunctuation>>$/ ), q{Don't match unrelated <?isClosePunctuation>} );
ok("\x[98D0]"  ~~ m/^<-<?isClosePunctuation>>$/, q{Match unrelated externally inverted <?isClosePunctuation>} );
ok("\x[98D0]"  ~~ m/^<+<-isClosePunctuation>>$/, q{Match unrelated internally inverted <?isClosePunctuation>} );
ok("\x[98D0]\c[RIGHT PARENTHESIS]" ~~ m/<+<?isClosePunctuation>>/, q{Match unanchored <?isClosePunctuation>} );

# Pi          InitialPunctuation


ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<?isPi>>$/, q{Match <?isPi> (InitialPunctuation)} );
ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]+<?isPi>>$/, q{Match compound <?isPi> (InitialPunctuation)} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-<?isPi>>$/ ), q{Don't match externally inverted <?isPi> (InitialPunctuation)} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]-<?isPi>>$/ ), q{Don't match compound inverted <?isPi> (InitialPunctuation)} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<-isPi>>$/ ), q{Don't match internally inverted <?isPi> (InitialPunctuation)} );
ok(!( "\x[D76F]"  ~~ m/^<+<?isPi>>$/ ), q{Don't match unrelated <?isPi> (InitialPunctuation)} );
ok("\x[D76F]"  ~~ m/^<-<?isPi>>$/, q{Match unrelated externally inverted <?isPi> (InitialPunctuation)} );
ok("\x[D76F]"  ~~ m/^<+<-isPi>>$/, q{Match unrelated internally inverted <?isPi> (InitialPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?isPi>>$/ ), q{Don't match related <?isPi> (InitialPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-isPi>>$/, q{Match related internally inverted <?isPi> (InitialPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?isPi>>$/, q{Match related externally inverted <?isPi> (InitialPunctuation)} );
ok("\x[D76F]\c[EXCLAMATION MARK]\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<+<?isPi>>/, q{Match unanchored <?isPi> (InitialPunctuation)} );

ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<?isInitialPunctuation>>$/, q{Match <?isInitialPunctuation>} );
ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]+<?isInitialPunctuation>>$/, q{Match compound <?isInitialPunctuation>} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-<?isInitialPunctuation>>$/ ), q{Don't match externally inverted <?isInitialPunctuation>} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]-<?isInitialPunctuation>>$/ ), q{Don't match compound inverted <?isInitialPunctuation>} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<-isInitialPunctuation>>$/ ), q{Don't match internally inverted <?isInitialPunctuation>} );
ok(!( "\x[C96E]"  ~~ m/^<+<?isInitialPunctuation>>$/ ), q{Don't match unrelated <?isInitialPunctuation>} );
ok("\x[C96E]"  ~~ m/^<-<?isInitialPunctuation>>$/, q{Match unrelated externally inverted <?isInitialPunctuation>} );
ok("\x[C96E]"  ~~ m/^<+<-isInitialPunctuation>>$/, q{Match unrelated internally inverted <?isInitialPunctuation>} );
ok("\x[C96E]\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<+<?isInitialPunctuation>>/, q{Match unanchored <?isInitialPunctuation>} );

# Pf          FinalPunctuation


ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<?isPf>>$/, q{Match <?isPf> (FinalPunctuation)} );
ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]+<?isPf>>$/, q{Match compound <?isPf> (FinalPunctuation)} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<-<?isPf>>$/ ), q{Don't match externally inverted <?isPf> (FinalPunctuation)} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]-<?isPf>>$/ ), q{Don't match compound inverted <?isPf> (FinalPunctuation)} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<-isPf>>$/ ), q{Don't match internally inverted <?isPf> (FinalPunctuation)} );
ok(!( "\x[0515]"  ~~ m/^<+<?isPf>>$/ ), q{Don't match unrelated <?isPf> (FinalPunctuation)} );
ok("\x[0515]"  ~~ m/^<-<?isPf>>$/, q{Match unrelated externally inverted <?isPf> (FinalPunctuation)} );
ok("\x[0515]"  ~~ m/^<+<-isPf>>$/, q{Match unrelated internally inverted <?isPf> (FinalPunctuation)} );
ok(!( "\c[ARMENIAN APOSTROPHE]" ~~ m/^<+<?isPf>>$/ ), q{Don't match related <?isPf> (FinalPunctuation)} );
ok("\c[ARMENIAN APOSTROPHE]" ~~ m/^<+<-isPf>>$/, q{Match related internally inverted <?isPf> (FinalPunctuation)} );
ok("\c[ARMENIAN APOSTROPHE]" ~~ m/^<-<?isPf>>$/, q{Match related externally inverted <?isPf> (FinalPunctuation)} );
ok("\x[0515]\c[ARMENIAN APOSTROPHE]\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/<+<?isPf>>/, q{Match unanchored <?isPf> (FinalPunctuation)} );

ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<?isFinalPunctuation>>$/, q{Match <?isFinalPunctuation>} );
ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]+<?isFinalPunctuation>>$/, q{Match compound <?isFinalPunctuation>} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<-<?isFinalPunctuation>>$/ ), q{Don't match externally inverted <?isFinalPunctuation>} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]-<?isFinalPunctuation>>$/ ), q{Don't match compound inverted <?isFinalPunctuation>} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<-isFinalPunctuation>>$/ ), q{Don't match internally inverted <?isFinalPunctuation>} );
ok(!( "\c[MODIFIER LETTER PRIME]"  ~~ m/^<+<?isFinalPunctuation>>$/ ), q{Don't match unrelated <?isFinalPunctuation>} );
ok("\c[MODIFIER LETTER PRIME]"  ~~ m/^<-<?isFinalPunctuation>>$/, q{Match unrelated externally inverted <?isFinalPunctuation>} );
ok("\c[MODIFIER LETTER PRIME]"  ~~ m/^<+<-isFinalPunctuation>>$/, q{Match unrelated internally inverted <?isFinalPunctuation>} );
ok("\c[MODIFIER LETTER PRIME]\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/<+<?isFinalPunctuation>>/, q{Match unanchored <?isFinalPunctuation>} );

# Po          OtherPunctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?isPo>>$/, q{Match <?isPo> (OtherPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?isPo>>$/, q{Match compound <?isPo> (OtherPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?isPo>>$/ ), q{Don't match externally inverted <?isPo> (OtherPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?isPo>>$/ ), q{Don't match compound inverted <?isPo> (OtherPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-isPo>>$/ ), q{Don't match internally inverted <?isPo> (OtherPunctuation)} );
ok(!( "\x[A586]"  ~~ m/^<+<?isPo>>$/ ), q{Don't match unrelated <?isPo> (OtherPunctuation)} );
ok("\x[A586]"  ~~ m/^<-<?isPo>>$/, q{Match unrelated externally inverted <?isPo> (OtherPunctuation)} );
ok("\x[A586]"  ~~ m/^<+<-isPo>>$/, q{Match unrelated internally inverted <?isPo> (OtherPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<?isPo>>$/ ), q{Don't match related <?isPo> (OtherPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<-isPo>>$/, q{Match related internally inverted <?isPo> (OtherPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<-<?isPo>>$/, q{Match related externally inverted <?isPo> (OtherPunctuation)} );
ok("\x[A586]\c[LEFT PARENTHESIS]\c[EXCLAMATION MARK]" ~~ m/<+<?isPo>>/, q{Match unanchored <?isPo> (OtherPunctuation)} );

ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?isOtherPunctuation>>$/, q{Match <?isOtherPunctuation>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?isOtherPunctuation>>$/, q{Match compound <?isOtherPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?isOtherPunctuation>>$/ ), q{Don't match externally inverted <?isOtherPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?isOtherPunctuation>>$/ ), q{Don't match compound inverted <?isOtherPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-isOtherPunctuation>>$/ ), q{Don't match internally inverted <?isOtherPunctuation>} );
ok(!( "\x[5FBD]"  ~~ m/^<+<?isOtherPunctuation>>$/ ), q{Don't match unrelated <?isOtherPunctuation>} );
ok("\x[5FBD]"  ~~ m/^<-<?isOtherPunctuation>>$/, q{Match unrelated externally inverted <?isOtherPunctuation>} );
ok("\x[5FBD]"  ~~ m/^<+<-isOtherPunctuation>>$/, q{Match unrelated internally inverted <?isOtherPunctuation>} );
ok("\x[5FBD]\c[EXCLAMATION MARK]" ~~ m/<+<?isOtherPunctuation>>/, q{Match unanchored <?isOtherPunctuation>} );

# S           Symbol


ok("\c[GUJARATI RUPEE SIGN]" ~~ m/^<+<isS>>$/, q{Match <isS> (Symbol)} );
ok("\c[GUJARATI RUPEE SIGN]" ~~ m/^<[A]+<isS>>$/, q{Match compound <isS> (Symbol)} );
ok(!( "\c[GUJARATI RUPEE SIGN]" ~~ m/^<-<isS>>$/ ), q{Don't match externally inverted <isS> (Symbol)} );
ok(!( "\c[GUJARATI RUPEE SIGN]" ~~ m/^<[A]-<isS>>$/ ), q{Don't match compound inverted <isS> (Symbol)} );
ok(!( "\c[GUJARATI RUPEE SIGN]" ~~ m/^<+<-isS>>$/ ), q{Don't match internally inverted <isS> (Symbol)} );
ok(!( "\c[GURMUKHI SIGN ADAK BINDI]"  ~~ m/^<+<isS>>$/ ), q{Don't match unrelated <isS> (Symbol)} );
ok("\c[GURMUKHI SIGN ADAK BINDI]"  ~~ m/^<-<isS>>$/, q{Match unrelated externally inverted <isS> (Symbol)} );
ok("\c[GURMUKHI SIGN ADAK BINDI]"  ~~ m/^<+<-isS>>$/, q{Match unrelated internally inverted <isS> (Symbol)} );
ok("\c[GURMUKHI SIGN ADAK BINDI]\c[GUJARATI RUPEE SIGN]" ~~ m/<+<isS>>/, q{Match unanchored <isS> (Symbol)} );

ok("\c[LIMBU SIGN LOO]" ~~ m/^<+<?isSymbol>>$/, q{Match <?isSymbol>} );
ok("\c[LIMBU SIGN LOO]" ~~ m/^<[A]+<?isSymbol>>$/, q{Match compound <?isSymbol>} );
ok(!( "\c[LIMBU SIGN LOO]" ~~ m/^<-<?isSymbol>>$/ ), q{Don't match externally inverted <?isSymbol>} );
ok(!( "\c[LIMBU SIGN LOO]" ~~ m/^<[A]-<?isSymbol>>$/ ), q{Don't match compound inverted <?isSymbol>} );
ok(!( "\c[LIMBU SIGN LOO]" ~~ m/^<+<-isSymbol>>$/ ), q{Don't match internally inverted <?isSymbol>} );
ok(!( "\x[192C]"  ~~ m/^<+<?isSymbol>>$/ ), q{Don't match unrelated <?isSymbol>} );
ok("\x[192C]"  ~~ m/^<-<?isSymbol>>$/, q{Match unrelated externally inverted <?isSymbol>} );
ok("\x[192C]"  ~~ m/^<+<-isSymbol>>$/, q{Match unrelated internally inverted <?isSymbol>} );
ok("\x[192C]\c[LIMBU SIGN LOO]" ~~ m/<+<?isSymbol>>/, q{Match unanchored <?isSymbol>} );

# Sm          MathSymbol


ok("\c[PLUS SIGN]" ~~ m/^<+<?isSm>>$/, q{Match <?isSm> (MathSymbol)} );
ok("\c[PLUS SIGN]" ~~ m/^<[A]+<?isSm>>$/, q{Match compound <?isSm> (MathSymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<-<?isSm>>$/ ), q{Don't match externally inverted <?isSm> (MathSymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<[A]-<?isSm>>$/ ), q{Don't match compound inverted <?isSm> (MathSymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<+<-isSm>>$/ ), q{Don't match internally inverted <?isSm> (MathSymbol)} );
ok(!( "\x[769B]"  ~~ m/^<+<?isSm>>$/ ), q{Don't match unrelated <?isSm> (MathSymbol)} );
ok("\x[769B]"  ~~ m/^<-<?isSm>>$/, q{Match unrelated externally inverted <?isSm> (MathSymbol)} );
ok("\x[769B]"  ~~ m/^<+<-isSm>>$/, q{Match unrelated internally inverted <?isSm> (MathSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<?isSm>>$/ ), q{Don't match related <?isSm> (MathSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<+<-isSm>>$/, q{Match related internally inverted <?isSm> (MathSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-<?isSm>>$/, q{Match related externally inverted <?isSm> (MathSymbol)} );
ok("\x[769B]\c[YI RADICAL QOT]\c[PLUS SIGN]" ~~ m/<+<?isSm>>/, q{Match unanchored <?isSm> (MathSymbol)} );

ok("\c[FRACTION SLASH]" ~~ m/^<+<?isMathSymbol>>$/, q{Match <?isMathSymbol>} );
ok("\c[FRACTION SLASH]" ~~ m/^<[A]+<?isMathSymbol>>$/, q{Match compound <?isMathSymbol>} );
ok(!( "\c[FRACTION SLASH]" ~~ m/^<-<?isMathSymbol>>$/ ), q{Don't match externally inverted <?isMathSymbol>} );
ok(!( "\c[FRACTION SLASH]" ~~ m/^<[A]-<?isMathSymbol>>$/ ), q{Don't match compound inverted <?isMathSymbol>} );
ok(!( "\c[FRACTION SLASH]" ~~ m/^<+<-isMathSymbol>>$/ ), q{Don't match internally inverted <?isMathSymbol>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]"  ~~ m/^<+<?isMathSymbol>>$/ ), q{Don't match unrelated <?isMathSymbol>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]"  ~~ m/^<-<?isMathSymbol>>$/, q{Match unrelated externally inverted <?isMathSymbol>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]"  ~~ m/^<+<-isMathSymbol>>$/, q{Match unrelated internally inverted <?isMathSymbol>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<?isMathSymbol>>$/ ), q{Don't match related <?isMathSymbol>} );
ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<-isMathSymbol>>$/, q{Match related internally inverted <?isMathSymbol>} );
ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<-<?isMathSymbol>>$/, q{Match related externally inverted <?isMathSymbol>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]\c[COMBINING LEFT HARPOON ABOVE]\c[FRACTION SLASH]" ~~ m/<+<?isMathSymbol>>/, q{Match unanchored <?isMathSymbol>} );

# Sc          CurrencySymbol


ok("\c[DOLLAR SIGN]" ~~ m/^<+<?isSc>>$/, q{Match <?isSc> (CurrencySymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<[A]+<?isSc>>$/, q{Match compound <?isSc> (CurrencySymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<-<?isSc>>$/ ), q{Don't match externally inverted <?isSc> (CurrencySymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<[A]-<?isSc>>$/ ), q{Don't match compound inverted <?isSc> (CurrencySymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<+<-isSc>>$/ ), q{Don't match internally inverted <?isSc> (CurrencySymbol)} );
ok(!( "\x[B6B4]"  ~~ m/^<+<?isSc>>$/ ), q{Don't match unrelated <?isSc> (CurrencySymbol)} );
ok("\x[B6B4]"  ~~ m/^<-<?isSc>>$/, q{Match unrelated externally inverted <?isSc> (CurrencySymbol)} );
ok("\x[B6B4]"  ~~ m/^<+<-isSc>>$/, q{Match unrelated internally inverted <?isSc> (CurrencySymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<+<?isSc>>$/ ), q{Don't match related <?isSc> (CurrencySymbol)} );
ok("\c[PLUS SIGN]" ~~ m/^<+<-isSc>>$/, q{Match related internally inverted <?isSc> (CurrencySymbol)} );
ok("\c[PLUS SIGN]" ~~ m/^<-<?isSc>>$/, q{Match related externally inverted <?isSc> (CurrencySymbol)} );
ok("\x[B6B4]\c[PLUS SIGN]\c[DOLLAR SIGN]" ~~ m/<+<?isSc>>/, q{Match unanchored <?isSc> (CurrencySymbol)} );

ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<+<?isCurrencySymbol>>$/, q{Match <?isCurrencySymbol>} );
ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]+<?isCurrencySymbol>>$/, q{Match compound <?isCurrencySymbol>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<-<?isCurrencySymbol>>$/ ), q{Don't match externally inverted <?isCurrencySymbol>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]-<?isCurrencySymbol>>$/ ), q{Don't match compound inverted <?isCurrencySymbol>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<+<-isCurrencySymbol>>$/ ), q{Don't match internally inverted <?isCurrencySymbol>} );
ok(!( "\x[1CD3]"  ~~ m/^<+<?isCurrencySymbol>>$/ ), q{Don't match unrelated <?isCurrencySymbol>} );
ok("\x[1CD3]"  ~~ m/^<-<?isCurrencySymbol>>$/, q{Match unrelated externally inverted <?isCurrencySymbol>} );
ok("\x[1CD3]"  ~~ m/^<+<-isCurrencySymbol>>$/, q{Match unrelated internally inverted <?isCurrencySymbol>} );
ok("\x[1CD3]\c[EURO-CURRENCY SIGN]" ~~ m/<+<?isCurrencySymbol>>/, q{Match unanchored <?isCurrencySymbol>} );

# Sk          ModifierSymbol


ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<?isSk>>$/, q{Match <?isSk> (ModifierSymbol)} );
ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]+<?isSk>>$/, q{Match compound <?isSk> (ModifierSymbol)} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-<?isSk>>$/ ), q{Don't match externally inverted <?isSk> (ModifierSymbol)} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]-<?isSk>>$/ ), q{Don't match compound inverted <?isSk> (ModifierSymbol)} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<-isSk>>$/ ), q{Don't match internally inverted <?isSk> (ModifierSymbol)} );
ok(!( "\x[68FA]"  ~~ m/^<+<?isSk>>$/ ), q{Don't match unrelated <?isSk> (ModifierSymbol)} );
ok("\x[68FA]"  ~~ m/^<-<?isSk>>$/, q{Match unrelated externally inverted <?isSk> (ModifierSymbol)} );
ok("\x[68FA]"  ~~ m/^<+<-isSk>>$/, q{Match unrelated internally inverted <?isSk> (ModifierSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<?isSk>>$/ ), q{Don't match related <?isSk> (ModifierSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<+<-isSk>>$/, q{Match related internally inverted <?isSk> (ModifierSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-<?isSk>>$/, q{Match related externally inverted <?isSk> (ModifierSymbol)} );
ok("\x[68FA]\c[YI RADICAL QOT]\c[CIRCUMFLEX ACCENT]" ~~ m/<+<?isSk>>/, q{Match unanchored <?isSk> (ModifierSymbol)} );

ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<?isModifierSymbol>>$/, q{Match <?isModifierSymbol>} );
ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]+<?isModifierSymbol>>$/, q{Match compound <?isModifierSymbol>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-<?isModifierSymbol>>$/ ), q{Don't match externally inverted <?isModifierSymbol>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]-<?isModifierSymbol>>$/ ), q{Don't match compound inverted <?isModifierSymbol>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<-isModifierSymbol>>$/ ), q{Don't match internally inverted <?isModifierSymbol>} );
ok(!( "\x[69E7]"  ~~ m/^<+<?isModifierSymbol>>$/ ), q{Don't match unrelated <?isModifierSymbol>} );
ok("\x[69E7]"  ~~ m/^<-<?isModifierSymbol>>$/, q{Match unrelated externally inverted <?isModifierSymbol>} );
ok("\x[69E7]"  ~~ m/^<+<-isModifierSymbol>>$/, q{Match unrelated internally inverted <?isModifierSymbol>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isModifierSymbol>>$/ ), q{Don't match related <?isModifierSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isModifierSymbol>>$/, q{Match related internally inverted <?isModifierSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isModifierSymbol>>$/, q{Match related externally inverted <?isModifierSymbol>} );
ok("\x[69E7]\c[COMBINING GRAVE ACCENT]\c[CIRCUMFLEX ACCENT]" ~~ m/<+<?isModifierSymbol>>/, q{Match unanchored <?isModifierSymbol>} );

# So          OtherSymbol


ok("\c[YI RADICAL QOT]" ~~ m/^<+<?isSo>>$/, q{Match <?isSo> (OtherSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<[A]+<?isSo>>$/, q{Match compound <?isSo> (OtherSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<-<?isSo>>$/ ), q{Don't match externally inverted <?isSo> (OtherSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<[A]-<?isSo>>$/ ), q{Don't match compound inverted <?isSo> (OtherSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<-isSo>>$/ ), q{Don't match internally inverted <?isSo> (OtherSymbol)} );
ok(!( "\x[8C90]"  ~~ m/^<+<?isSo>>$/ ), q{Don't match unrelated <?isSo> (OtherSymbol)} );
ok("\x[8C90]"  ~~ m/^<-<?isSo>>$/, q{Match unrelated externally inverted <?isSo> (OtherSymbol)} );
ok("\x[8C90]"  ~~ m/^<+<-isSo>>$/, q{Match unrelated internally inverted <?isSo> (OtherSymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<+<?isSo>>$/ ), q{Don't match related <?isSo> (OtherSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<+<-isSo>>$/, q{Match related internally inverted <?isSo> (OtherSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-<?isSo>>$/, q{Match related externally inverted <?isSo> (OtherSymbol)} );
ok("\x[8C90]\c[DOLLAR SIGN]\c[YI RADICAL QOT]" ~~ m/<+<?isSo>>/, q{Match unanchored <?isSo> (OtherSymbol)} );

ok("\c[YI RADICAL QOT]" ~~ m/^<+<?isOtherSymbol>>$/, q{Match <?isOtherSymbol>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<[A]+<?isOtherSymbol>>$/, q{Match compound <?isOtherSymbol>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<-<?isOtherSymbol>>$/ ), q{Don't match externally inverted <?isOtherSymbol>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<[A]-<?isOtherSymbol>>$/ ), q{Don't match compound inverted <?isOtherSymbol>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<-isOtherSymbol>>$/ ), q{Don't match internally inverted <?isOtherSymbol>} );
ok(!( "\x[95A7]"  ~~ m/^<+<?isOtherSymbol>>$/ ), q{Don't match unrelated <?isOtherSymbol>} );
ok("\x[95A7]"  ~~ m/^<-<?isOtherSymbol>>$/, q{Match unrelated externally inverted <?isOtherSymbol>} );
ok("\x[95A7]"  ~~ m/^<+<-isOtherSymbol>>$/, q{Match unrelated internally inverted <?isOtherSymbol>} );
ok("\x[95A7]\c[YI RADICAL QOT]" ~~ m/<+<?isOtherSymbol>>/, q{Match unanchored <?isOtherSymbol>} );

# Z           Separator


ok("\c[SPACE]" ~~ m/^<+<isZ>>$/, q{Match <isZ> (Separator)} );
ok("\c[SPACE]" ~~ m/^<[A]+<isZ>>$/, q{Match compound <isZ> (Separator)} );
ok(!( "\c[SPACE]" ~~ m/^<-<isZ>>$/ ), q{Don't match externally inverted <isZ> (Separator)} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<isZ>>$/ ), q{Don't match compound inverted <isZ> (Separator)} );
ok(!( "\c[SPACE]" ~~ m/^<+<-isZ>>$/ ), q{Don't match internally inverted <isZ> (Separator)} );
ok(!( "\x[D222]"  ~~ m/^<+<isZ>>$/ ), q{Don't match unrelated <isZ> (Separator)} );
ok("\x[D222]"  ~~ m/^<-<isZ>>$/, q{Match unrelated externally inverted <isZ> (Separator)} );
ok("\x[D222]"  ~~ m/^<+<-isZ>>$/, q{Match unrelated internally inverted <isZ> (Separator)} );
ok("\x[D222]\c[SPACE]" ~~ m/<+<isZ>>/, q{Match unanchored <isZ> (Separator)} );

ok("\c[SPACE]" ~~ m/^<+<?isSeparator>>$/, q{Match <?isSeparator>} );
ok("\c[SPACE]" ~~ m/^<[A]+<?isSeparator>>$/, q{Match compound <?isSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<-<?isSeparator>>$/ ), q{Don't match externally inverted <?isSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<?isSeparator>>$/ ), q{Don't match compound inverted <?isSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<+<-isSeparator>>$/ ), q{Don't match internally inverted <?isSeparator>} );
ok(!( "\x[D7B7]"  ~~ m/^<+<?isSeparator>>$/ ), q{Don't match unrelated <?isSeparator>} );
ok("\x[D7B7]"  ~~ m/^<-<?isSeparator>>$/, q{Match unrelated externally inverted <?isSeparator>} );
ok("\x[D7B7]"  ~~ m/^<+<-isSeparator>>$/, q{Match unrelated internally inverted <?isSeparator>} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<+<?isSeparator>>$/ ), q{Don't match related <?isSeparator>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<+<-isSeparator>>$/, q{Match related internally inverted <?isSeparator>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-<?isSeparator>>$/, q{Match related externally inverted <?isSeparator>} );
ok("\x[D7B7]\c[DOLLAR SIGN]\c[SPACE]" ~~ m/<+<?isSeparator>>/, q{Match unanchored <?isSeparator>} );

# Zs          SpaceSeparator


ok("\c[SPACE]" ~~ m/^<+<?isZs>>$/, q{Match <?isZs> (SpaceSeparator)} );
ok("\c[SPACE]" ~~ m/^<[A]+<?isZs>>$/, q{Match compound <?isZs> (SpaceSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<-<?isZs>>$/ ), q{Don't match externally inverted <?isZs> (SpaceSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<?isZs>>$/ ), q{Don't match compound inverted <?isZs> (SpaceSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<+<-isZs>>$/ ), q{Don't match internally inverted <?isZs> (SpaceSeparator)} );
ok(!( "\x[5918]"  ~~ m/^<+<?isZs>>$/ ), q{Don't match unrelated <?isZs> (SpaceSeparator)} );
ok("\x[5918]"  ~~ m/^<-<?isZs>>$/, q{Match unrelated externally inverted <?isZs> (SpaceSeparator)} );
ok("\x[5918]"  ~~ m/^<+<-isZs>>$/, q{Match unrelated internally inverted <?isZs> (SpaceSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<+<?isZs>>$/ ), q{Don't match related <?isZs> (SpaceSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<+<-isZs>>$/, q{Match related internally inverted <?isZs> (SpaceSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<-<?isZs>>$/, q{Match related externally inverted <?isZs> (SpaceSeparator)} );
ok("\x[5918]\c[LINE SEPARATOR]\c[SPACE]" ~~ m/<+<?isZs>>/, q{Match unanchored <?isZs> (SpaceSeparator)} );

ok("\c[SPACE]" ~~ m/^<+<?isSpaceSeparator>>$/, q{Match <?isSpaceSeparator>} );
ok("\c[SPACE]" ~~ m/^<[A]+<?isSpaceSeparator>>$/, q{Match compound <?isSpaceSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<-<?isSpaceSeparator>>$/ ), q{Don't match externally inverted <?isSpaceSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<?isSpaceSeparator>>$/ ), q{Don't match compound inverted <?isSpaceSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<+<-isSpaceSeparator>>$/ ), q{Don't match internally inverted <?isSpaceSeparator>} );
ok(!( "\x[3704]"  ~~ m/^<+<?isSpaceSeparator>>$/ ), q{Don't match unrelated <?isSpaceSeparator>} );
ok("\x[3704]"  ~~ m/^<-<?isSpaceSeparator>>$/, q{Match unrelated externally inverted <?isSpaceSeparator>} );
ok("\x[3704]"  ~~ m/^<+<-isSpaceSeparator>>$/, q{Match unrelated internally inverted <?isSpaceSeparator>} );
ok(!( "\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<+<?isSpaceSeparator>>$/ ), q{Don't match related <?isSpaceSeparator>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<+<-isSpaceSeparator>>$/, q{Match related internally inverted <?isSpaceSeparator>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<-<?isSpaceSeparator>>$/, q{Match related externally inverted <?isSpaceSeparator>} );
ok("\x[3704]\c[HEXAGRAM FOR THE CREATIVE HEAVEN]\c[SPACE]" ~~ m/<+<?isSpaceSeparator>>/, q{Match unanchored <?isSpaceSeparator>} );

# Zl          LineSeparator


ok("\c[LINE SEPARATOR]" ~~ m/^<+<?isZl>>$/, q{Match <?isZl> (LineSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<[A]+<?isZl>>$/, q{Match compound <?isZl> (LineSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<-<?isZl>>$/ ), q{Don't match externally inverted <?isZl> (LineSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<[A]-<?isZl>>$/ ), q{Don't match compound inverted <?isZl> (LineSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<+<-isZl>>$/ ), q{Don't match internally inverted <?isZl> (LineSeparator)} );
ok(!( "\x[ADAD]"  ~~ m/^<+<?isZl>>$/ ), q{Don't match unrelated <?isZl> (LineSeparator)} );
ok("\x[ADAD]"  ~~ m/^<-<?isZl>>$/, q{Match unrelated externally inverted <?isZl> (LineSeparator)} );
ok("\x[ADAD]"  ~~ m/^<+<-isZl>>$/, q{Match unrelated internally inverted <?isZl> (LineSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<+<?isZl>>$/ ), q{Don't match related <?isZl> (LineSeparator)} );
ok("\c[SPACE]" ~~ m/^<+<-isZl>>$/, q{Match related internally inverted <?isZl> (LineSeparator)} );
ok("\c[SPACE]" ~~ m/^<-<?isZl>>$/, q{Match related externally inverted <?isZl> (LineSeparator)} );
ok("\x[ADAD]\c[SPACE]\c[LINE SEPARATOR]" ~~ m/<+<?isZl>>/, q{Match unanchored <?isZl> (LineSeparator)} );

ok("\c[LINE SEPARATOR]" ~~ m/^<+<?isLineSeparator>>$/, q{Match <?isLineSeparator>} );
ok("\c[LINE SEPARATOR]" ~~ m/^<[A]+<?isLineSeparator>>$/, q{Match compound <?isLineSeparator>} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<-<?isLineSeparator>>$/ ), q{Don't match externally inverted <?isLineSeparator>} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<[A]-<?isLineSeparator>>$/ ), q{Don't match compound inverted <?isLineSeparator>} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<+<-isLineSeparator>>$/ ), q{Don't match internally inverted <?isLineSeparator>} );
ok(!( "\x[C5E7]"  ~~ m/^<+<?isLineSeparator>>$/ ), q{Don't match unrelated <?isLineSeparator>} );
ok("\x[C5E7]"  ~~ m/^<-<?isLineSeparator>>$/, q{Match unrelated externally inverted <?isLineSeparator>} );
ok("\x[C5E7]"  ~~ m/^<+<-isLineSeparator>>$/, q{Match unrelated internally inverted <?isLineSeparator>} );
ok(!( "\x[C5E7]" ~~ m/^<+<?isLineSeparator>>$/ ), q{Don't match related <?isLineSeparator>} );
ok("\x[C5E7]" ~~ m/^<+<-isLineSeparator>>$/, q{Match related internally inverted <?isLineSeparator>} );
ok("\x[C5E7]" ~~ m/^<-<?isLineSeparator>>$/, q{Match related externally inverted <?isLineSeparator>} );
ok("\x[C5E7]\x[C5E7]\c[LINE SEPARATOR]" ~~ m/<+<?isLineSeparator>>/, q{Match unanchored <?isLineSeparator>} );

# Zp          ParagraphSeparator


ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<?isZp>>$/, q{Match <?isZp> (ParagraphSeparator)} );
ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]+<?isZp>>$/, q{Match compound <?isZp> (ParagraphSeparator)} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<-<?isZp>>$/ ), q{Don't match externally inverted <?isZp> (ParagraphSeparator)} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]-<?isZp>>$/ ), q{Don't match compound inverted <?isZp> (ParagraphSeparator)} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<-isZp>>$/ ), q{Don't match internally inverted <?isZp> (ParagraphSeparator)} );
ok(!( "\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]"  ~~ m/^<+<?isZp>>$/ ), q{Don't match unrelated <?isZp> (ParagraphSeparator)} );
ok("\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]"  ~~ m/^<-<?isZp>>$/, q{Match unrelated externally inverted <?isZp> (ParagraphSeparator)} );
ok("\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]"  ~~ m/^<+<-isZp>>$/, q{Match unrelated internally inverted <?isZp> (ParagraphSeparator)} );
ok(!( "\c[MONGOLIAN VOWEL SEPARATOR]" ~~ m/^<+<?isZp>>$/ ), q{Don't match related <?isZp> (ParagraphSeparator)} );
ok("\c[MONGOLIAN VOWEL SEPARATOR]" ~~ m/^<+<-isZp>>$/, q{Match related internally inverted <?isZp> (ParagraphSeparator)} );
ok("\c[MONGOLIAN VOWEL SEPARATOR]" ~~ m/^<-<?isZp>>$/, q{Match related externally inverted <?isZp> (ParagraphSeparator)} );
ok("\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]\c[MONGOLIAN VOWEL SEPARATOR]\c[PARAGRAPH SEPARATOR]" ~~ m/<+<?isZp>>/, q{Match unanchored <?isZp> (ParagraphSeparator)} );

ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<?isParagraphSeparator>>$/, q{Match <?isParagraphSeparator>} );
ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]+<?isParagraphSeparator>>$/, q{Match compound <?isParagraphSeparator>} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<-<?isParagraphSeparator>>$/ ), q{Don't match externally inverted <?isParagraphSeparator>} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]-<?isParagraphSeparator>>$/ ), q{Don't match compound inverted <?isParagraphSeparator>} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<-isParagraphSeparator>>$/ ), q{Don't match internally inverted <?isParagraphSeparator>} );
ok(!( "\c[HIRAGANA LETTER KA]"  ~~ m/^<+<?isParagraphSeparator>>$/ ), q{Don't match unrelated <?isParagraphSeparator>} );
ok("\c[HIRAGANA LETTER KA]"  ~~ m/^<-<?isParagraphSeparator>>$/, q{Match unrelated externally inverted <?isParagraphSeparator>} );
ok("\c[HIRAGANA LETTER KA]"  ~~ m/^<+<-isParagraphSeparator>>$/, q{Match unrelated internally inverted <?isParagraphSeparator>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<?isParagraphSeparator>>$/ ), q{Don't match related <?isParagraphSeparator>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<-isParagraphSeparator>>$/, q{Match related internally inverted <?isParagraphSeparator>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<-<?isParagraphSeparator>>$/, q{Match related externally inverted <?isParagraphSeparator>} );
ok("\c[HIRAGANA LETTER KA]\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]\c[PARAGRAPH SEPARATOR]" ~~ m/<+<?isParagraphSeparator>>/, q{Match unanchored <?isParagraphSeparator>} );

# C           Other


ok("\x[9FA6]" ~~ m/^<+<isC>>$/, q{Match <isC> (Other)} );
ok("\x[9FA6]" ~~ m/^<[A]+<isC>>$/, q{Match compound <isC> (Other)} );
ok(!( "\x[9FA6]" ~~ m/^<-<isC>>$/ ), q{Don't match externally inverted <isC> (Other)} );
ok(!( "\x[9FA6]" ~~ m/^<[A]-<isC>>$/ ), q{Don't match compound inverted <isC> (Other)} );
ok(!( "\x[9FA6]" ~~ m/^<+<-isC>>$/ ), q{Don't match internally inverted <isC> (Other)} );
ok(!( "\x[56E6]"  ~~ m/^<+<isC>>$/ ), q{Don't match unrelated <isC> (Other)} );
ok("\x[56E6]"  ~~ m/^<-<isC>>$/, q{Match unrelated externally inverted <isC> (Other)} );
ok("\x[56E6]"  ~~ m/^<+<-isC>>$/, q{Match unrelated internally inverted <isC> (Other)} );
ok("\x[56E6]\x[9FA6]" ~~ m/<+<isC>>/, q{Match unanchored <isC> (Other)} );

ok("\x[0EAC]" ~~ m/^<+<?isOther>>$/, q{Match <?isOther>} );
ok("\x[0EAC]" ~~ m/^<[A]+<?isOther>>$/, q{Match compound <?isOther>} );
ok(!( "\x[0EAC]" ~~ m/^<-<?isOther>>$/ ), q{Don't match externally inverted <?isOther>} );
ok(!( "\x[0EAC]" ~~ m/^<[A]-<?isOther>>$/ ), q{Don't match compound inverted <?isOther>} );
ok(!( "\x[0EAC]" ~~ m/^<+<-isOther>>$/ ), q{Don't match internally inverted <?isOther>} );
ok(!( "\c[LAO LETTER HO SUNG]"  ~~ m/^<+<?isOther>>$/ ), q{Don't match unrelated <?isOther>} );
ok("\c[LAO LETTER HO SUNG]"  ~~ m/^<-<?isOther>>$/, q{Match unrelated externally inverted <?isOther>} );
ok("\c[LAO LETTER HO SUNG]"  ~~ m/^<+<-isOther>>$/, q{Match unrelated internally inverted <?isOther>} );
ok("\c[LAO LETTER HO SUNG]\x[0EAC]" ~~ m/<+<?isOther>>/, q{Match unanchored <?isOther>} );

# Cc          Control


ok("\c[NULL]" ~~ m/^<+<?isCc>>$/, q{Match <?isCc> (Control)} );
ok("\c[NULL]" ~~ m/^<[A]+<?isCc>>$/, q{Match compound <?isCc> (Control)} );
ok(!( "\c[NULL]" ~~ m/^<-<?isCc>>$/ ), q{Don't match externally inverted <?isCc> (Control)} );
ok(!( "\c[NULL]" ~~ m/^<[A]-<?isCc>>$/ ), q{Don't match compound inverted <?isCc> (Control)} );
ok(!( "\c[NULL]" ~~ m/^<+<-isCc>>$/ ), q{Don't match internally inverted <?isCc> (Control)} );
ok(!( "\c[OGONEK]"  ~~ m/^<+<?isCc>>$/ ), q{Don't match unrelated <?isCc> (Control)} );
ok("\c[OGONEK]"  ~~ m/^<-<?isCc>>$/, q{Match unrelated externally inverted <?isCc> (Control)} );
ok("\c[OGONEK]"  ~~ m/^<+<-isCc>>$/, q{Match unrelated internally inverted <?isCc> (Control)} );
ok(!( "\x[0358]" ~~ m/^<+<?isCc>>$/ ), q{Don't match related <?isCc> (Control)} );
ok("\x[0358]" ~~ m/^<+<-isCc>>$/, q{Match related internally inverted <?isCc> (Control)} );
ok("\x[0358]" ~~ m/^<-<?isCc>>$/, q{Match related externally inverted <?isCc> (Control)} );
ok("\c[OGONEK]\x[0358]\c[NULL]" ~~ m/<+<?isCc>>/, q{Match unanchored <?isCc> (Control)} );

ok("\c[NULL]" ~~ m/^<+<?isControl>>$/, q{Match <?isControl>} );
ok("\c[NULL]" ~~ m/^<[A]+<?isControl>>$/, q{Match compound <?isControl>} );
ok(!( "\c[NULL]" ~~ m/^<-<?isControl>>$/ ), q{Don't match externally inverted <?isControl>} );
ok(!( "\c[NULL]" ~~ m/^<[A]-<?isControl>>$/ ), q{Don't match compound inverted <?isControl>} );
ok(!( "\c[NULL]" ~~ m/^<+<-isControl>>$/ ), q{Don't match internally inverted <?isControl>} );
ok(!( "\x[4A20]"  ~~ m/^<+<?isControl>>$/ ), q{Don't match unrelated <?isControl>} );
ok("\x[4A20]"  ~~ m/^<-<?isControl>>$/, q{Match unrelated externally inverted <?isControl>} );
ok("\x[4A20]"  ~~ m/^<+<-isControl>>$/, q{Match unrelated internally inverted <?isControl>} );
ok(!( "\x[4DB6]" ~~ m/^<+<?isControl>>$/ ), q{Don't match related <?isControl>} );
ok("\x[4DB6]" ~~ m/^<+<-isControl>>$/, q{Match related internally inverted <?isControl>} );
ok("\x[4DB6]" ~~ m/^<-<?isControl>>$/, q{Match related externally inverted <?isControl>} );
ok("\x[4A20]\x[4DB6]\c[NULL]" ~~ m/<+<?isControl>>/, q{Match unanchored <?isControl>} );

# Cf          Format


ok("\c[SOFT HYPHEN]" ~~ m/^<+<?isCf>>$/, q{Match <?isCf> (Format)} );
ok("\c[SOFT HYPHEN]" ~~ m/^<[A]+<?isCf>>$/, q{Match compound <?isCf> (Format)} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<-<?isCf>>$/ ), q{Don't match externally inverted <?isCf> (Format)} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<[A]-<?isCf>>$/ ), q{Don't match compound inverted <?isCf> (Format)} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<+<-isCf>>$/ ), q{Don't match internally inverted <?isCf> (Format)} );
ok(!( "\x[AECE]"  ~~ m/^<+<?isCf>>$/ ), q{Don't match unrelated <?isCf> (Format)} );
ok("\x[AECE]"  ~~ m/^<-<?isCf>>$/, q{Match unrelated externally inverted <?isCf> (Format)} );
ok("\x[AECE]"  ~~ m/^<+<-isCf>>$/, q{Match unrelated internally inverted <?isCf> (Format)} );
ok(!( "\x[D7A4]" ~~ m/^<+<?isCf>>$/ ), q{Don't match related <?isCf> (Format)} );
ok("\x[D7A4]" ~~ m/^<+<-isCf>>$/, q{Match related internally inverted <?isCf> (Format)} );
ok("\x[D7A4]" ~~ m/^<-<?isCf>>$/, q{Match related externally inverted <?isCf> (Format)} );
ok("\x[AECE]\x[D7A4]\c[SOFT HYPHEN]" ~~ m/<+<?isCf>>/, q{Match unanchored <?isCf> (Format)} );

ok("\c[SOFT HYPHEN]" ~~ m/^<+<?isFormat>>$/, q{Match <?isFormat>} );
ok("\c[SOFT HYPHEN]" ~~ m/^<[A]+<?isFormat>>$/, q{Match compound <?isFormat>} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<-<?isFormat>>$/ ), q{Don't match externally inverted <?isFormat>} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<[A]-<?isFormat>>$/ ), q{Don't match compound inverted <?isFormat>} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<+<-isFormat>>$/ ), q{Don't match internally inverted <?isFormat>} );
ok(!( "\x[5382]"  ~~ m/^<+<?isFormat>>$/ ), q{Don't match unrelated <?isFormat>} );
ok("\x[5382]"  ~~ m/^<-<?isFormat>>$/, q{Match unrelated externally inverted <?isFormat>} );
ok("\x[5382]"  ~~ m/^<+<-isFormat>>$/, q{Match unrelated internally inverted <?isFormat>} );
ok("\x[5382]\c[SOFT HYPHEN]" ~~ m/<+<?isFormat>>/, q{Match unanchored <?isFormat>} );

# BidiL       # Left-to-Right


ok("\c[YI SYLLABLE IT]" ~~ m/^<+<?isBidiL>>$/, q{Match (Left-to-Right)} );
ok("\c[YI SYLLABLE IT]" ~~ m/^<[A]+<?isBidiL>>$/, q{Match compound (Left-to-Right)} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<-<?isBidiL>>$/ ), q{Don't match externally inverted (Left-to-Right)} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<[A]-<?isBidiL>>$/ ), q{Don't match compound inverted (Left-to-Right)} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<+<-isBidiL>>$/ ), q{Don't match internally inverted (Left-to-Right)} );
ok(!( "\x[5BF5]"  ~~ m/^<+<?isBidiL>>$/ ), q{Don't match unrelated (Left-to-Right)} );
ok("\x[5BF5]"  ~~ m/^<-<?isBidiL>>$/, q{Match unrelated externally inverted (Left-to-Right)} );
ok("\x[5BF5]"  ~~ m/^<+<-isBidiL>>$/, q{Match unrelated internally inverted (Left-to-Right)} );
ok("\x[5BF5]\c[YI SYLLABLE IT]" ~~ m/<+<?isBidiL>>/, q{Match unanchored (Left-to-Right)} );

# BidiEN      # European Number


ok("\c[DIGIT ZERO]" ~~ m/^<+<?isBidiEN>>$/, q{Match (European Number)} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?isBidiEN>>$/, q{Match compound (European Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?isBidiEN>>$/ ), q{Don't match externally inverted (European Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?isBidiEN>>$/ ), q{Don't match compound inverted (European Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-isBidiEN>>$/ ), q{Don't match internally inverted (European Number)} );
ok(!( "\x[5596]"  ~~ m/^<+<?isBidiEN>>$/ ), q{Don't match unrelated (European Number)} );
ok("\x[5596]"  ~~ m/^<-<?isBidiEN>>$/, q{Match unrelated externally inverted (European Number)} );
ok("\x[5596]"  ~~ m/^<+<-isBidiEN>>$/, q{Match unrelated internally inverted (European Number)} );
ok("\x[5596]\c[DIGIT ZERO]" ~~ m/<+<?isBidiEN>>/, q{Match unanchored (European Number)} );

# BidiES      # European Number Separator


ok("\c[SOLIDUS]" ~~ m/^<+<?isBidiES>>$/, q{Match (European Number Separator)} );
ok("\c[SOLIDUS]" ~~ m/^<[A]+<?isBidiES>>$/, q{Match compound (European Number Separator)} );
ok(!( "\c[SOLIDUS]" ~~ m/^<-<?isBidiES>>$/ ), q{Don't match externally inverted (European Number Separator)} );
ok(!( "\c[SOLIDUS]" ~~ m/^<[A]-<?isBidiES>>$/ ), q{Don't match compound inverted (European Number Separator)} );
ok(!( "\c[SOLIDUS]" ~~ m/^<+<-isBidiES>>$/ ), q{Don't match internally inverted (European Number Separator)} );
ok(!( "\x[85D3]"  ~~ m/^<+<?isBidiES>>$/ ), q{Don't match unrelated (European Number Separator)} );
ok("\x[85D3]"  ~~ m/^<-<?isBidiES>>$/, q{Match unrelated externally inverted (European Number Separator)} );
ok("\x[85D3]"  ~~ m/^<+<-isBidiES>>$/, q{Match unrelated internally inverted (European Number Separator)} );
ok("\x[85D3]\c[SOLIDUS]" ~~ m/<+<?isBidiES>>/, q{Match unanchored (European Number Separator)} );

# BidiET      # European Number Terminator


ok("\c[NUMBER SIGN]" ~~ m/^<+<?isBidiET>>$/, q{Match (European Number Terminator)} );
ok("\c[NUMBER SIGN]" ~~ m/^<[A]+<?isBidiET>>$/, q{Match compound (European Number Terminator)} );
ok(!( "\c[NUMBER SIGN]" ~~ m/^<-<?isBidiET>>$/ ), q{Don't match externally inverted (European Number Terminator)} );
ok(!( "\c[NUMBER SIGN]" ~~ m/^<[A]-<?isBidiET>>$/ ), q{Don't match compound inverted (European Number Terminator)} );
ok(!( "\c[NUMBER SIGN]" ~~ m/^<+<-isBidiET>>$/ ), q{Don't match internally inverted (European Number Terminator)} );
ok(!( "\x[9AFC]"  ~~ m/^<+<?isBidiET>>$/ ), q{Don't match unrelated (European Number Terminator)} );
ok("\x[9AFC]"  ~~ m/^<-<?isBidiET>>$/, q{Match unrelated externally inverted (European Number Terminator)} );
ok("\x[9AFC]"  ~~ m/^<+<-isBidiET>>$/, q{Match unrelated internally inverted (European Number Terminator)} );
ok("\x[9AFC]\c[NUMBER SIGN]" ~~ m/<+<?isBidiET>>/, q{Match unanchored (European Number Terminator)} );

# BidiWS      # Whitespace


ok("\c[FORM FEED (FF)]" ~~ m/^<+<?isBidiWS>>$/, q{Match (Whitespace)} );
ok("\c[FORM FEED (FF)]" ~~ m/^<[A]+<?isBidiWS>>$/, q{Match compound (Whitespace)} );
ok(!( "\c[FORM FEED (FF)]" ~~ m/^<-<?isBidiWS>>$/ ), q{Don't match externally inverted (Whitespace)} );
ok(!( "\c[FORM FEED (FF)]" ~~ m/^<[A]-<?isBidiWS>>$/ ), q{Don't match compound inverted (Whitespace)} );
ok(!( "\c[FORM FEED (FF)]" ~~ m/^<+<-isBidiWS>>$/ ), q{Don't match internally inverted (Whitespace)} );
ok(!( "\x[4441]"  ~~ m/^<+<?isBidiWS>>$/ ), q{Don't match unrelated (Whitespace)} );
ok("\x[4441]"  ~~ m/^<-<?isBidiWS>>$/, q{Match unrelated externally inverted (Whitespace)} );
ok("\x[4441]"  ~~ m/^<+<-isBidiWS>>$/, q{Match unrelated internally inverted (Whitespace)} );
ok("\x[4441]\c[FORM FEED (FF)]" ~~ m/<+<?isBidiWS>>/, q{Match unanchored (Whitespace)} );

# Arabic


ok("\c[ARABIC LETTER HAMZA]" ~~ m/^<+<?isArabic>>$/, q{Match <?isArabic>} );
ok("\c[ARABIC LETTER HAMZA]" ~~ m/^<[A]+<?isArabic>>$/, q{Match compound <?isArabic>} );
ok(!( "\c[ARABIC LETTER HAMZA]" ~~ m/^<-<?isArabic>>$/ ), q{Don't match externally inverted <?isArabic>} );
ok(!( "\c[ARABIC LETTER HAMZA]" ~~ m/^<[A]-<?isArabic>>$/ ), q{Don't match compound inverted <?isArabic>} );
ok(!( "\c[ARABIC LETTER HAMZA]" ~~ m/^<+<-isArabic>>$/ ), q{Don't match internally inverted <?isArabic>} );
ok(!( "\c[YI SYLLABLE RYRX]"  ~~ m/^<+<?isArabic>>$/ ), q{Don't match unrelated <?isArabic>} );
ok("\c[YI SYLLABLE RYRX]"  ~~ m/^<-<?isArabic>>$/, q{Match unrelated externally inverted <?isArabic>} );
ok("\c[YI SYLLABLE RYRX]"  ~~ m/^<+<-isArabic>>$/, q{Match unrelated internally inverted <?isArabic>} );
ok("\c[YI SYLLABLE RYRX]\c[ARABIC LETTER HAMZA]" ~~ m/<+<?isArabic>>/, q{Match unanchored <?isArabic>} );

# Armenian


ok("\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<+<?isArmenian>>$/, q{Match <?isArmenian>} );
ok("\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<[A]+<?isArmenian>>$/, q{Match compound <?isArmenian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<-<?isArmenian>>$/ ), q{Don't match externally inverted <?isArmenian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<[A]-<?isArmenian>>$/ ), q{Don't match compound inverted <?isArmenian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<+<-isArmenian>>$/ ), q{Don't match internally inverted <?isArmenian>} );
ok(!( "\x[B2ED]"  ~~ m/^<+<?isArmenian>>$/ ), q{Don't match unrelated <?isArmenian>} );
ok("\x[B2ED]"  ~~ m/^<-<?isArmenian>>$/, q{Match unrelated externally inverted <?isArmenian>} );
ok("\x[B2ED]"  ~~ m/^<+<-isArmenian>>$/, q{Match unrelated internally inverted <?isArmenian>} );
ok("\x[B2ED]\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/<+<?isArmenian>>/, q{Match unanchored <?isArmenian>} );

# Bengali


ok("\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<+<?isBengali>>$/, q{Match <?isBengali>} );
ok("\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<[A]+<?isBengali>>$/, q{Match compound <?isBengali>} );
ok(!( "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<-<?isBengali>>$/ ), q{Don't match externally inverted <?isBengali>} );
ok(!( "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<[A]-<?isBengali>>$/ ), q{Don't match compound inverted <?isBengali>} );
ok(!( "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<+<-isBengali>>$/ ), q{Don't match internally inverted <?isBengali>} );
ok(!( "\x[4AFD]"  ~~ m/^<+<?isBengali>>$/ ), q{Don't match unrelated <?isBengali>} );
ok("\x[4AFD]"  ~~ m/^<-<?isBengali>>$/, q{Match unrelated externally inverted <?isBengali>} );
ok("\x[4AFD]"  ~~ m/^<+<-isBengali>>$/, q{Match unrelated internally inverted <?isBengali>} );
ok("\x[4AFD]\c[BENGALI SIGN CANDRABINDU]" ~~ m/<+<?isBengali>>/, q{Match unanchored <?isBengali>} );

# Bopomofo


ok("\c[BOPOMOFO LETTER B]" ~~ m/^<+<?isBopomofo>>$/, q{Match <?isBopomofo>} );
ok("\c[BOPOMOFO LETTER B]" ~~ m/^<[A]+<?isBopomofo>>$/, q{Match compound <?isBopomofo>} );
ok(!( "\c[BOPOMOFO LETTER B]" ~~ m/^<-<?isBopomofo>>$/ ), q{Don't match externally inverted <?isBopomofo>} );
ok(!( "\c[BOPOMOFO LETTER B]" ~~ m/^<[A]-<?isBopomofo>>$/ ), q{Don't match compound inverted <?isBopomofo>} );
ok(!( "\c[BOPOMOFO LETTER B]" ~~ m/^<+<-isBopomofo>>$/ ), q{Don't match internally inverted <?isBopomofo>} );
ok(!( "\x[8369]"  ~~ m/^<+<?isBopomofo>>$/ ), q{Don't match unrelated <?isBopomofo>} );
ok("\x[8369]"  ~~ m/^<-<?isBopomofo>>$/, q{Match unrelated externally inverted <?isBopomofo>} );
ok("\x[8369]"  ~~ m/^<+<-isBopomofo>>$/, q{Match unrelated internally inverted <?isBopomofo>} );
ok("\x[8369]\c[BOPOMOFO LETTER B]" ~~ m/<+<?isBopomofo>>/, q{Match unanchored <?isBopomofo>} );

# Buhid


ok("\c[BUHID LETTER A]" ~~ m/^<+<?isBuhid>>$/, q{Match <?isBuhid>} );
ok("\c[BUHID LETTER A]" ~~ m/^<[A]+<?isBuhid>>$/, q{Match compound <?isBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<-<?isBuhid>>$/ ), q{Don't match externally inverted <?isBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<[A]-<?isBuhid>>$/ ), q{Don't match compound inverted <?isBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<+<-isBuhid>>$/ ), q{Don't match internally inverted <?isBuhid>} );
ok(!( "\x[877F]"  ~~ m/^<+<?isBuhid>>$/ ), q{Don't match unrelated <?isBuhid>} );
ok("\x[877F]"  ~~ m/^<-<?isBuhid>>$/, q{Match unrelated externally inverted <?isBuhid>} );
ok("\x[877F]"  ~~ m/^<+<-isBuhid>>$/, q{Match unrelated internally inverted <?isBuhid>} );
ok("\x[877F]\c[BUHID LETTER A]" ~~ m/<+<?isBuhid>>/, q{Match unanchored <?isBuhid>} );

# CanadianAboriginal


ok("\c[CANADIAN SYLLABICS E]" ~~ m/^<+<?isCanadianAboriginal>>$/, q{Match <?isCanadianAboriginal>} );
ok("\c[CANADIAN SYLLABICS E]" ~~ m/^<[A]+<?isCanadianAboriginal>>$/, q{Match compound <?isCanadianAboriginal>} );
ok(!( "\c[CANADIAN SYLLABICS E]" ~~ m/^<-<?isCanadianAboriginal>>$/ ), q{Don't match externally inverted <?isCanadianAboriginal>} );
ok(!( "\c[CANADIAN SYLLABICS E]" ~~ m/^<[A]-<?isCanadianAboriginal>>$/ ), q{Don't match compound inverted <?isCanadianAboriginal>} );
ok(!( "\c[CANADIAN SYLLABICS E]" ~~ m/^<+<-isCanadianAboriginal>>$/ ), q{Don't match internally inverted <?isCanadianAboriginal>} );
ok(!( "\x[3A42]"  ~~ m/^<+<?isCanadianAboriginal>>$/ ), q{Don't match unrelated <?isCanadianAboriginal>} );
ok("\x[3A42]"  ~~ m/^<-<?isCanadianAboriginal>>$/, q{Match unrelated externally inverted <?isCanadianAboriginal>} );
ok("\x[3A42]"  ~~ m/^<+<-isCanadianAboriginal>>$/, q{Match unrelated internally inverted <?isCanadianAboriginal>} );
ok(!( "\x[4DB6]" ~~ m/^<+<?isCanadianAboriginal>>$/ ), q{Don't match related <?isCanadianAboriginal>} );
ok("\x[4DB6]" ~~ m/^<+<-isCanadianAboriginal>>$/, q{Match related internally inverted <?isCanadianAboriginal>} );
ok("\x[4DB6]" ~~ m/^<-<?isCanadianAboriginal>>$/, q{Match related externally inverted <?isCanadianAboriginal>} );
ok("\x[3A42]\x[4DB6]\c[CANADIAN SYLLABICS E]" ~~ m/<+<?isCanadianAboriginal>>/, q{Match unanchored <?isCanadianAboriginal>} );

# Cherokee


ok("\c[CHEROKEE LETTER A]" ~~ m/^<+<?isCherokee>>$/, q{Match <?isCherokee>} );
ok("\c[CHEROKEE LETTER A]" ~~ m/^<[A]+<?isCherokee>>$/, q{Match compound <?isCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<-<?isCherokee>>$/ ), q{Don't match externally inverted <?isCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<[A]-<?isCherokee>>$/ ), q{Don't match compound inverted <?isCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<+<-isCherokee>>$/ ), q{Don't match internally inverted <?isCherokee>} );
ok(!( "\x[A9EF]"  ~~ m/^<+<?isCherokee>>$/ ), q{Don't match unrelated <?isCherokee>} );
ok("\x[A9EF]"  ~~ m/^<-<?isCherokee>>$/, q{Match unrelated externally inverted <?isCherokee>} );
ok("\x[A9EF]"  ~~ m/^<+<-isCherokee>>$/, q{Match unrelated internally inverted <?isCherokee>} );
ok(!( "\x[A9EF]" ~~ m/^<+<?isCherokee>>$/ ), q{Don't match related <?isCherokee>} );
ok("\x[A9EF]" ~~ m/^<+<-isCherokee>>$/, q{Match related internally inverted <?isCherokee>} );
ok("\x[A9EF]" ~~ m/^<-<?isCherokee>>$/, q{Match related externally inverted <?isCherokee>} );
ok("\x[A9EF]\x[A9EF]\c[CHEROKEE LETTER A]" ~~ m/<+<?isCherokee>>/, q{Match unanchored <?isCherokee>} );

# Cyrillic


ok("\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<+<?isCyrillic>>$/, q{Match <?isCyrillic>} );
ok("\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<[A]+<?isCyrillic>>$/, q{Match compound <?isCyrillic>} );
ok(!( "\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<-<?isCyrillic>>$/ ), q{Don't match externally inverted <?isCyrillic>} );
ok(!( "\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<[A]-<?isCyrillic>>$/ ), q{Don't match compound inverted <?isCyrillic>} );
ok(!( "\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<+<-isCyrillic>>$/ ), q{Don't match internally inverted <?isCyrillic>} );
ok(!( "\x[07EF]"  ~~ m/^<+<?isCyrillic>>$/ ), q{Don't match unrelated <?isCyrillic>} );
ok("\x[07EF]"  ~~ m/^<-<?isCyrillic>>$/, q{Match unrelated externally inverted <?isCyrillic>} );
ok("\x[07EF]"  ~~ m/^<+<-isCyrillic>>$/, q{Match unrelated internally inverted <?isCyrillic>} );
ok(!( "\x[07EF]" ~~ m/^<+<?isCyrillic>>$/ ), q{Don't match related <?isCyrillic>} );
ok("\x[07EF]" ~~ m/^<+<-isCyrillic>>$/, q{Match related internally inverted <?isCyrillic>} );
ok("\x[07EF]" ~~ m/^<-<?isCyrillic>>$/, q{Match related externally inverted <?isCyrillic>} );
ok("\x[07EF]\x[07EF]\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/<+<?isCyrillic>>/, q{Match unanchored <?isCyrillic>} );

# Deseret


ok(!( "\x[65BD]"  ~~ m/^<+<?isDeseret>>$/ ), q{Don't match unrelated <?isDeseret>} );
ok("\x[65BD]"  ~~ m/^<-<?isDeseret>>$/, q{Match unrelated externally inverted <?isDeseret>} );
ok("\x[65BD]"  ~~ m/^<+<-isDeseret>>$/, q{Match unrelated internally inverted <?isDeseret>} );

# Devanagari


ok("\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<+<?isDevanagari>>$/, q{Match <?isDevanagari>} );
ok("\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<[A]+<?isDevanagari>>$/, q{Match compound <?isDevanagari>} );
ok(!( "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<-<?isDevanagari>>$/ ), q{Don't match externally inverted <?isDevanagari>} );
ok(!( "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<[A]-<?isDevanagari>>$/ ), q{Don't match compound inverted <?isDevanagari>} );
ok(!( "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<+<-isDevanagari>>$/ ), q{Don't match internally inverted <?isDevanagari>} );
ok(!( "\x[653B]"  ~~ m/^<+<?isDevanagari>>$/ ), q{Don't match unrelated <?isDevanagari>} );
ok("\x[653B]"  ~~ m/^<-<?isDevanagari>>$/, q{Match unrelated externally inverted <?isDevanagari>} );
ok("\x[653B]"  ~~ m/^<+<-isDevanagari>>$/, q{Match unrelated internally inverted <?isDevanagari>} );
ok("\x[653B]\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/<+<?isDevanagari>>/, q{Match unanchored <?isDevanagari>} );

# Ethiopic


ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<?isEthiopic>>$/, q{Match <?isEthiopic>} );
ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]+<?isEthiopic>>$/, q{Match compound <?isEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<-<?isEthiopic>>$/ ), q{Don't match externally inverted <?isEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]-<?isEthiopic>>$/ ), q{Don't match compound inverted <?isEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<-isEthiopic>>$/ ), q{Don't match internally inverted <?isEthiopic>} );
ok(!( "\x[482C]"  ~~ m/^<+<?isEthiopic>>$/ ), q{Don't match unrelated <?isEthiopic>} );
ok("\x[482C]"  ~~ m/^<-<?isEthiopic>>$/, q{Match unrelated externally inverted <?isEthiopic>} );
ok("\x[482C]"  ~~ m/^<+<-isEthiopic>>$/, q{Match unrelated internally inverted <?isEthiopic>} );
ok("\x[482C]\c[ETHIOPIC SYLLABLE HA]" ~~ m/<+<?isEthiopic>>/, q{Match unanchored <?isEthiopic>} );

# Georgian


ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<?isGeorgian>>$/, q{Match <?isGeorgian>} );
ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]+<?isGeorgian>>$/, q{Match compound <?isGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<-<?isGeorgian>>$/ ), q{Don't match externally inverted <?isGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]-<?isGeorgian>>$/ ), q{Don't match compound inverted <?isGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<-isGeorgian>>$/ ), q{Don't match internally inverted <?isGeorgian>} );
ok(!( "\x[9BE5]"  ~~ m/^<+<?isGeorgian>>$/ ), q{Don't match unrelated <?isGeorgian>} );
ok("\x[9BE5]"  ~~ m/^<-<?isGeorgian>>$/, q{Match unrelated externally inverted <?isGeorgian>} );
ok("\x[9BE5]"  ~~ m/^<+<-isGeorgian>>$/, q{Match unrelated internally inverted <?isGeorgian>} );
ok("\x[9BE5]\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/<+<?isGeorgian>>/, q{Match unanchored <?isGeorgian>} );

# Gothic


ok(!( "\x[4ED2]"  ~~ m/^<+<?isGothic>>$/ ), q{Don't match unrelated <?isGothic>} );
ok("\x[4ED2]"  ~~ m/^<-<?isGothic>>$/, q{Match unrelated externally inverted <?isGothic>} );
ok("\x[4ED2]"  ~~ m/^<+<-isGothic>>$/, q{Match unrelated internally inverted <?isGothic>} );

# Greek


ok("\c[MICRO SIGN]" ~~ m/^<+<?isGreek>>$/, q{Match <?isGreek>} );
ok("\c[MICRO SIGN]" ~~ m/^<[A]+<?isGreek>>$/, q{Match compound <?isGreek>} );
ok(!( "\c[MICRO SIGN]" ~~ m/^<-<?isGreek>>$/ ), q{Don't match externally inverted <?isGreek>} );
ok(!( "\c[MICRO SIGN]" ~~ m/^<[A]-<?isGreek>>$/ ), q{Don't match compound inverted <?isGreek>} );
ok(!( "\c[MICRO SIGN]" ~~ m/^<+<-isGreek>>$/ ), q{Don't match internally inverted <?isGreek>} );
ok(!( "\x[D486]"  ~~ m/^<+<?isGreek>>$/ ), q{Don't match unrelated <?isGreek>} );
ok("\x[D486]"  ~~ m/^<-<?isGreek>>$/, q{Match unrelated externally inverted <?isGreek>} );
ok("\x[D486]"  ~~ m/^<+<-isGreek>>$/, q{Match unrelated internally inverted <?isGreek>} );
ok("\x[D486]\c[MICRO SIGN]" ~~ m/<+<?isGreek>>/, q{Match unanchored <?isGreek>} );

# Gujarati


ok("\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<+<?isGujarati>>$/, q{Match <?isGujarati>} );
ok("\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<[A]+<?isGujarati>>$/, q{Match compound <?isGujarati>} );
ok(!( "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<-<?isGujarati>>$/ ), q{Don't match externally inverted <?isGujarati>} );
ok(!( "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<[A]-<?isGujarati>>$/ ), q{Don't match compound inverted <?isGujarati>} );
ok(!( "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<+<-isGujarati>>$/ ), q{Don't match internally inverted <?isGujarati>} );
ok(!( "\x[B3F3]"  ~~ m/^<+<?isGujarati>>$/ ), q{Don't match unrelated <?isGujarati>} );
ok("\x[B3F3]"  ~~ m/^<-<?isGujarati>>$/, q{Match unrelated externally inverted <?isGujarati>} );
ok("\x[B3F3]"  ~~ m/^<+<-isGujarati>>$/, q{Match unrelated internally inverted <?isGujarati>} );
ok("\x[B3F3]\c[GUJARATI SIGN CANDRABINDU]" ~~ m/<+<?isGujarati>>/, q{Match unanchored <?isGujarati>} );

# Gurmukhi


ok("\c[GURMUKHI SIGN BINDI]" ~~ m/^<+<?isGurmukhi>>$/, q{Match <?isGurmukhi>} );
ok("\c[GURMUKHI SIGN BINDI]" ~~ m/^<[A]+<?isGurmukhi>>$/, q{Match compound <?isGurmukhi>} );
ok(!( "\c[GURMUKHI SIGN BINDI]" ~~ m/^<-<?isGurmukhi>>$/ ), q{Don't match externally inverted <?isGurmukhi>} );
ok(!( "\c[GURMUKHI SIGN BINDI]" ~~ m/^<[A]-<?isGurmukhi>>$/ ), q{Don't match compound inverted <?isGurmukhi>} );
ok(!( "\c[GURMUKHI SIGN BINDI]" ~~ m/^<+<-isGurmukhi>>$/ ), q{Don't match internally inverted <?isGurmukhi>} );
ok(!( "\x[6469]"  ~~ m/^<+<?isGurmukhi>>$/ ), q{Don't match unrelated <?isGurmukhi>} );
ok("\x[6469]"  ~~ m/^<-<?isGurmukhi>>$/, q{Match unrelated externally inverted <?isGurmukhi>} );
ok("\x[6469]"  ~~ m/^<+<-isGurmukhi>>$/, q{Match unrelated internally inverted <?isGurmukhi>} );
ok("\x[6469]\c[GURMUKHI SIGN BINDI]" ~~ m/<+<?isGurmukhi>>/, q{Match unanchored <?isGurmukhi>} );

# Han


ok("\x[9DB5]" ~~ m/^<+<?isHan>>$/, q{Match <?isHan>} );
ok("\x[9DB5]" ~~ m/^<[A]+<?isHan>>$/, q{Match compound <?isHan>} );
ok(!( "\x[9DB5]" ~~ m/^<-<?isHan>>$/ ), q{Don't match externally inverted <?isHan>} );
ok(!( "\x[9DB5]" ~~ m/^<[A]-<?isHan>>$/ ), q{Don't match compound inverted <?isHan>} );
ok(!( "\x[9DB5]" ~~ m/^<+<-isHan>>$/ ), q{Don't match internally inverted <?isHan>} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?isHan>>$/ ), q{Don't match unrelated <?isHan>} );
ok("\x[9FA6]"  ~~ m/^<-<?isHan>>$/, q{Match unrelated externally inverted <?isHan>} );
ok("\x[9FA6]"  ~~ m/^<+<-isHan>>$/, q{Match unrelated internally inverted <?isHan>} );
ok("\x[9FA6]\x[9DB5]" ~~ m/<+<?isHan>>/, q{Match unanchored <?isHan>} );

# Hangul


ok("\x[AC00]" ~~ m/^<+<?isHangul>>$/, q{Match <?isHangul>} );
ok("\x[AC00]" ~~ m/^<[A]+<?isHangul>>$/, q{Match compound <?isHangul>} );
ok(!( "\x[AC00]" ~~ m/^<-<?isHangul>>$/ ), q{Don't match externally inverted <?isHangul>} );
ok(!( "\x[AC00]" ~~ m/^<[A]-<?isHangul>>$/ ), q{Don't match compound inverted <?isHangul>} );
ok(!( "\x[AC00]" ~~ m/^<+<-isHangul>>$/ ), q{Don't match internally inverted <?isHangul>} );
ok(!( "\x[9E09]"  ~~ m/^<+<?isHangul>>$/ ), q{Don't match unrelated <?isHangul>} );
ok("\x[9E09]"  ~~ m/^<-<?isHangul>>$/, q{Match unrelated externally inverted <?isHangul>} );
ok("\x[9E09]"  ~~ m/^<+<-isHangul>>$/, q{Match unrelated internally inverted <?isHangul>} );
ok("\x[9E09]\x[AC00]" ~~ m/<+<?isHangul>>/, q{Match unanchored <?isHangul>} );

# Hanunoo


ok("\c[HANUNOO LETTER A]" ~~ m/^<+<?isHanunoo>>$/, q{Match <?isHanunoo>} );
ok("\c[HANUNOO LETTER A]" ~~ m/^<[A]+<?isHanunoo>>$/, q{Match compound <?isHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<-<?isHanunoo>>$/ ), q{Don't match externally inverted <?isHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<[A]-<?isHanunoo>>$/ ), q{Don't match compound inverted <?isHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<+<-isHanunoo>>$/ ), q{Don't match internally inverted <?isHanunoo>} );
ok(!( "\x[580B]"  ~~ m/^<+<?isHanunoo>>$/ ), q{Don't match unrelated <?isHanunoo>} );
ok("\x[580B]"  ~~ m/^<-<?isHanunoo>>$/, q{Match unrelated externally inverted <?isHanunoo>} );
ok("\x[580B]"  ~~ m/^<+<-isHanunoo>>$/, q{Match unrelated internally inverted <?isHanunoo>} );
ok("\x[580B]\c[HANUNOO LETTER A]" ~~ m/<+<?isHanunoo>>/, q{Match unanchored <?isHanunoo>} );

# Hebrew


ok("\c[HEBREW LETTER ALEF]" ~~ m/^<+<?isHebrew>>$/, q{Match <?isHebrew>} );
ok("\c[HEBREW LETTER ALEF]" ~~ m/^<[A]+<?isHebrew>>$/, q{Match compound <?isHebrew>} );
ok(!( "\c[HEBREW LETTER ALEF]" ~~ m/^<-<?isHebrew>>$/ ), q{Don't match externally inverted <?isHebrew>} );
ok(!( "\c[HEBREW LETTER ALEF]" ~~ m/^<[A]-<?isHebrew>>$/ ), q{Don't match compound inverted <?isHebrew>} );
ok(!( "\c[HEBREW LETTER ALEF]" ~~ m/^<+<-isHebrew>>$/ ), q{Don't match internally inverted <?isHebrew>} );
ok(!( "\x[62B4]"  ~~ m/^<+<?isHebrew>>$/ ), q{Don't match unrelated <?isHebrew>} );
ok("\x[62B4]"  ~~ m/^<-<?isHebrew>>$/, q{Match unrelated externally inverted <?isHebrew>} );
ok("\x[62B4]"  ~~ m/^<+<-isHebrew>>$/, q{Match unrelated internally inverted <?isHebrew>} );
ok("\x[62B4]\c[HEBREW LETTER ALEF]" ~~ m/<+<?isHebrew>>/, q{Match unanchored <?isHebrew>} );

# Hiragana


ok("\c[HIRAGANA LETTER SMALL A]" ~~ m/^<+<?isHiragana>>$/, q{Match <?isHiragana>} );
ok("\c[HIRAGANA LETTER SMALL A]" ~~ m/^<[A]+<?isHiragana>>$/, q{Match compound <?isHiragana>} );
ok(!( "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<-<?isHiragana>>$/ ), q{Don't match externally inverted <?isHiragana>} );
ok(!( "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<[A]-<?isHiragana>>$/ ), q{Don't match compound inverted <?isHiragana>} );
ok(!( "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<+<-isHiragana>>$/ ), q{Don't match internally inverted <?isHiragana>} );
ok(!( "\x[9504]"  ~~ m/^<+<?isHiragana>>$/ ), q{Don't match unrelated <?isHiragana>} );
ok("\x[9504]"  ~~ m/^<-<?isHiragana>>$/, q{Match unrelated externally inverted <?isHiragana>} );
ok("\x[9504]"  ~~ m/^<+<-isHiragana>>$/, q{Match unrelated internally inverted <?isHiragana>} );
ok("\x[9504]\c[HIRAGANA LETTER SMALL A]" ~~ m/<+<?isHiragana>>/, q{Match unanchored <?isHiragana>} );

# Inherited


ok("\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<+<?isInherited>>$/, q{Match <?isInherited>} );
ok("\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<[A]+<?isInherited>>$/, q{Match compound <?isInherited>} );
ok(!( "\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<-<?isInherited>>$/ ), q{Don't match externally inverted <?isInherited>} );
ok(!( "\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<[A]-<?isInherited>>$/ ), q{Don't match compound inverted <?isInherited>} );
ok(!( "\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<+<-isInherited>>$/ ), q{Don't match internally inverted <?isInherited>} );
ok(!( "\c[TAMIL LETTER RRA]"  ~~ m/^<+<?isInherited>>$/ ), q{Don't match unrelated <?isInherited>} );
ok("\c[TAMIL LETTER RRA]"  ~~ m/^<-<?isInherited>>$/, q{Match unrelated externally inverted <?isInherited>} );
ok("\c[TAMIL LETTER RRA]"  ~~ m/^<+<-isInherited>>$/, q{Match unrelated internally inverted <?isInherited>} );
ok("\c[TAMIL LETTER RRA]\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/<+<?isInherited>>/, q{Match unanchored <?isInherited>} );

# Kannada


ok("\c[KANNADA SIGN ANUSVARA]" ~~ m/^<+<?isKannada>>$/, q{Match <?isKannada>} );
ok("\c[KANNADA SIGN ANUSVARA]" ~~ m/^<[A]+<?isKannada>>$/, q{Match compound <?isKannada>} );
ok(!( "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<-<?isKannada>>$/ ), q{Don't match externally inverted <?isKannada>} );
ok(!( "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<[A]-<?isKannada>>$/ ), q{Don't match compound inverted <?isKannada>} );
ok(!( "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<+<-isKannada>>$/ ), q{Don't match internally inverted <?isKannada>} );
ok(!( "\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<+<?isKannada>>$/ ), q{Don't match unrelated <?isKannada>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<-<?isKannada>>$/, q{Match unrelated externally inverted <?isKannada>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<+<-isKannada>>$/, q{Match unrelated internally inverted <?isKannada>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]\c[KANNADA SIGN ANUSVARA]" ~~ m/<+<?isKannada>>/, q{Match unanchored <?isKannada>} );

# Katakana


ok("\c[KATAKANA LETTER SMALL A]" ~~ m/^<+<?isKatakana>>$/, q{Match <?isKatakana>} );
ok("\c[KATAKANA LETTER SMALL A]" ~~ m/^<[A]+<?isKatakana>>$/, q{Match compound <?isKatakana>} );
ok(!( "\c[KATAKANA LETTER SMALL A]" ~~ m/^<-<?isKatakana>>$/ ), q{Don't match externally inverted <?isKatakana>} );
ok(!( "\c[KATAKANA LETTER SMALL A]" ~~ m/^<[A]-<?isKatakana>>$/ ), q{Don't match compound inverted <?isKatakana>} );
ok(!( "\c[KATAKANA LETTER SMALL A]" ~~ m/^<+<-isKatakana>>$/ ), q{Don't match internally inverted <?isKatakana>} );
ok(!( "\x[40DB]"  ~~ m/^<+<?isKatakana>>$/ ), q{Don't match unrelated <?isKatakana>} );
ok("\x[40DB]"  ~~ m/^<-<?isKatakana>>$/, q{Match unrelated externally inverted <?isKatakana>} );
ok("\x[40DB]"  ~~ m/^<+<-isKatakana>>$/, q{Match unrelated internally inverted <?isKatakana>} );
ok("\x[40DB]\c[KATAKANA LETTER SMALL A]" ~~ m/<+<?isKatakana>>/, q{Match unanchored <?isKatakana>} );

# Khmer


ok("\c[KHMER LETTER KA]" ~~ m/^<+<?isKhmer>>$/, q{Match <?isKhmer>} );
ok("\c[KHMER LETTER KA]" ~~ m/^<[A]+<?isKhmer>>$/, q{Match compound <?isKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<-<?isKhmer>>$/ ), q{Don't match externally inverted <?isKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<[A]-<?isKhmer>>$/ ), q{Don't match compound inverted <?isKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<+<-isKhmer>>$/ ), q{Don't match internally inverted <?isKhmer>} );
ok(!( "\x[AC3E]"  ~~ m/^<+<?isKhmer>>$/ ), q{Don't match unrelated <?isKhmer>} );
ok("\x[AC3E]"  ~~ m/^<-<?isKhmer>>$/, q{Match unrelated externally inverted <?isKhmer>} );
ok("\x[AC3E]"  ~~ m/^<+<-isKhmer>>$/, q{Match unrelated internally inverted <?isKhmer>} );
ok("\x[AC3E]\c[KHMER LETTER KA]" ~~ m/<+<?isKhmer>>/, q{Match unanchored <?isKhmer>} );

# Lao


ok("\c[LAO LETTER KO]" ~~ m/^<+<?isLao>>$/, q{Match <?isLao>} );
ok("\c[LAO LETTER KO]" ~~ m/^<[A]+<?isLao>>$/, q{Match compound <?isLao>} );
ok(!( "\c[LAO LETTER KO]" ~~ m/^<-<?isLao>>$/ ), q{Don't match externally inverted <?isLao>} );
ok(!( "\c[LAO LETTER KO]" ~~ m/^<[A]-<?isLao>>$/ ), q{Don't match compound inverted <?isLao>} );
ok(!( "\c[LAO LETTER KO]" ~~ m/^<+<-isLao>>$/ ), q{Don't match internally inverted <?isLao>} );
ok(!( "\c[MODIFIER LETTER UNASPIRATED]"  ~~ m/^<+<?isLao>>$/ ), q{Don't match unrelated <?isLao>} );
ok("\c[MODIFIER LETTER UNASPIRATED]"  ~~ m/^<-<?isLao>>$/, q{Match unrelated externally inverted <?isLao>} );
ok("\c[MODIFIER LETTER UNASPIRATED]"  ~~ m/^<+<-isLao>>$/, q{Match unrelated internally inverted <?isLao>} );
ok(!( "\c[MODIFIER LETTER DOUBLE APOSTROPHE]" ~~ m/^<+<?isLao>>$/ ), q{Don't match related <?isLao>} );
ok("\c[MODIFIER LETTER DOUBLE APOSTROPHE]" ~~ m/^<+<-isLao>>$/, q{Match related internally inverted <?isLao>} );
ok("\c[MODIFIER LETTER DOUBLE APOSTROPHE]" ~~ m/^<-<?isLao>>$/, q{Match related externally inverted <?isLao>} );
ok("\c[MODIFIER LETTER UNASPIRATED]\c[MODIFIER LETTER DOUBLE APOSTROPHE]\c[LAO LETTER KO]" ~~ m/<+<?isLao>>/, q{Match unanchored <?isLao>} );

# Latin


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?isLatin>>$/, q{Match <?isLatin>} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?isLatin>>$/, q{Match compound <?isLatin>} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?isLatin>>$/ ), q{Don't match externally inverted <?isLatin>} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?isLatin>>$/ ), q{Don't match compound inverted <?isLatin>} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-isLatin>>$/ ), q{Don't match internally inverted <?isLatin>} );
ok(!( "\x[6B4C]"  ~~ m/^<+<?isLatin>>$/ ), q{Don't match unrelated <?isLatin>} );
ok("\x[6B4C]"  ~~ m/^<-<?isLatin>>$/, q{Match unrelated externally inverted <?isLatin>} );
ok("\x[6B4C]"  ~~ m/^<+<-isLatin>>$/, q{Match unrelated internally inverted <?isLatin>} );
ok(!( "\x[6B4C]" ~~ m/^<+<?isLatin>>$/ ), q{Don't match related <?isLatin>} );
ok("\x[6B4C]" ~~ m/^<+<-isLatin>>$/, q{Match related internally inverted <?isLatin>} );
ok("\x[6B4C]" ~~ m/^<-<?isLatin>>$/, q{Match related externally inverted <?isLatin>} );
ok("\x[6B4C]\x[6B4C]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?isLatin>>/, q{Match unanchored <?isLatin>} );

# Malayalam


ok("\c[MALAYALAM LETTER TA]" ~~ m/^<+<?isMalayalam>>$/, q{Match <?isMalayalam>} );
ok("\c[MALAYALAM LETTER TA]" ~~ m/^<[A]+<?isMalayalam>>$/, q{Match compound <?isMalayalam>} );
ok(!( "\c[MALAYALAM LETTER TA]" ~~ m/^<-<?isMalayalam>>$/ ), q{Don't match externally inverted <?isMalayalam>} );
ok(!( "\c[MALAYALAM LETTER TA]" ~~ m/^<[A]-<?isMalayalam>>$/ ), q{Don't match compound inverted <?isMalayalam>} );
ok(!( "\c[MALAYALAM LETTER TA]" ~~ m/^<+<-isMalayalam>>$/ ), q{Don't match internally inverted <?isMalayalam>} );
ok(!( "\x[0D29]"  ~~ m/^<+<?isMalayalam>>$/ ), q{Don't match unrelated <?isMalayalam>} );
ok("\x[0D29]"  ~~ m/^<-<?isMalayalam>>$/, q{Match unrelated externally inverted <?isMalayalam>} );
ok("\x[0D29]"  ~~ m/^<+<-isMalayalam>>$/, q{Match unrelated internally inverted <?isMalayalam>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<?isMalayalam>>$/ ), q{Don't match related <?isMalayalam>} );
ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<-isMalayalam>>$/, q{Match related internally inverted <?isMalayalam>} );
ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<-<?isMalayalam>>$/, q{Match related externally inverted <?isMalayalam>} );
ok("\x[0D29]\c[SINHALA SIGN ANUSVARAYA]\c[MALAYALAM LETTER TA]" ~~ m/<+<?isMalayalam>>/, q{Match unanchored <?isMalayalam>} );

# Mongolian


ok("\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<+<?isMongolian>>$/, q{Match <?isMongolian>} );
ok("\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<[A]+<?isMongolian>>$/, q{Match compound <?isMongolian>} );
ok(!( "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<-<?isMongolian>>$/ ), q{Don't match externally inverted <?isMongolian>} );
ok(!( "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<[A]-<?isMongolian>>$/ ), q{Don't match compound inverted <?isMongolian>} );
ok(!( "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<+<-isMongolian>>$/ ), q{Don't match internally inverted <?isMongolian>} );
ok(!( "\x[70C0]"  ~~ m/^<+<?isMongolian>>$/ ), q{Don't match unrelated <?isMongolian>} );
ok("\x[70C0]"  ~~ m/^<-<?isMongolian>>$/, q{Match unrelated externally inverted <?isMongolian>} );
ok("\x[70C0]"  ~~ m/^<+<-isMongolian>>$/, q{Match unrelated internally inverted <?isMongolian>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isMongolian>>$/ ), q{Don't match related <?isMongolian>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isMongolian>>$/, q{Match related internally inverted <?isMongolian>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isMongolian>>$/, q{Match related externally inverted <?isMongolian>} );
ok("\x[70C0]\c[COMBINING GRAVE ACCENT]\c[MONGOLIAN DIGIT ZERO]" ~~ m/<+<?isMongolian>>/, q{Match unanchored <?isMongolian>} );

# Myanmar


ok("\c[MYANMAR LETTER KA]" ~~ m/^<+<?isMyanmar>>$/, q{Match <?isMyanmar>} );
ok("\c[MYANMAR LETTER KA]" ~~ m/^<[A]+<?isMyanmar>>$/, q{Match compound <?isMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<-<?isMyanmar>>$/ ), q{Don't match externally inverted <?isMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<[A]-<?isMyanmar>>$/ ), q{Don't match compound inverted <?isMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<+<-isMyanmar>>$/ ), q{Don't match internally inverted <?isMyanmar>} );
ok(!( "\x[3CE3]"  ~~ m/^<+<?isMyanmar>>$/ ), q{Don't match unrelated <?isMyanmar>} );
ok("\x[3CE3]"  ~~ m/^<-<?isMyanmar>>$/, q{Match unrelated externally inverted <?isMyanmar>} );
ok("\x[3CE3]"  ~~ m/^<+<-isMyanmar>>$/, q{Match unrelated internally inverted <?isMyanmar>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isMyanmar>>$/ ), q{Don't match related <?isMyanmar>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isMyanmar>>$/, q{Match related internally inverted <?isMyanmar>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isMyanmar>>$/, q{Match related externally inverted <?isMyanmar>} );
ok("\x[3CE3]\c[COMBINING GRAVE ACCENT]\c[MYANMAR LETTER KA]" ~~ m/<+<?isMyanmar>>/, q{Match unanchored <?isMyanmar>} );

# Ogham


ok("\c[OGHAM LETTER BEITH]" ~~ m/^<+<?isOgham>>$/, q{Match <?isOgham>} );
ok("\c[OGHAM LETTER BEITH]" ~~ m/^<[A]+<?isOgham>>$/, q{Match compound <?isOgham>} );
ok(!( "\c[OGHAM LETTER BEITH]" ~~ m/^<-<?isOgham>>$/ ), q{Don't match externally inverted <?isOgham>} );
ok(!( "\c[OGHAM LETTER BEITH]" ~~ m/^<[A]-<?isOgham>>$/ ), q{Don't match compound inverted <?isOgham>} );
ok(!( "\c[OGHAM LETTER BEITH]" ~~ m/^<+<-isOgham>>$/ ), q{Don't match internally inverted <?isOgham>} );
ok(!( "\x[077B]"  ~~ m/^<+<?isOgham>>$/ ), q{Don't match unrelated <?isOgham>} );
ok("\x[077B]"  ~~ m/^<-<?isOgham>>$/, q{Match unrelated externally inverted <?isOgham>} );
ok("\x[077B]"  ~~ m/^<+<-isOgham>>$/, q{Match unrelated internally inverted <?isOgham>} );
ok("\x[077B]\c[OGHAM LETTER BEITH]" ~~ m/<+<?isOgham>>/, q{Match unanchored <?isOgham>} );

# OldItalic


ok(!( "\x[562B]"  ~~ m/^<+<?isOldItalic>>$/ ), q{Don't match unrelated <?isOldItalic>} );
ok("\x[562B]"  ~~ m/^<-<?isOldItalic>>$/, q{Match unrelated externally inverted <?isOldItalic>} );
ok("\x[562B]"  ~~ m/^<+<-isOldItalic>>$/, q{Match unrelated internally inverted <?isOldItalic>} );

# Oriya


ok("\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<+<?isOriya>>$/, q{Match <?isOriya>} );
ok("\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<[A]+<?isOriya>>$/, q{Match compound <?isOriya>} );
ok(!( "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<-<?isOriya>>$/ ), q{Don't match externally inverted <?isOriya>} );
ok(!( "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<[A]-<?isOriya>>$/ ), q{Don't match compound inverted <?isOriya>} );
ok(!( "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<+<-isOriya>>$/ ), q{Don't match internally inverted <?isOriya>} );
ok(!( "\x[3CE7]"  ~~ m/^<+<?isOriya>>$/ ), q{Don't match unrelated <?isOriya>} );
ok("\x[3CE7]"  ~~ m/^<-<?isOriya>>$/, q{Match unrelated externally inverted <?isOriya>} );
ok("\x[3CE7]"  ~~ m/^<+<-isOriya>>$/, q{Match unrelated internally inverted <?isOriya>} );
ok("\x[3CE7]\c[ORIYA SIGN CANDRABINDU]" ~~ m/<+<?isOriya>>/, q{Match unanchored <?isOriya>} );

# Runic


ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<?isRunic>>$/, q{Match <?isRunic>} );
ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]+<?isRunic>>$/, q{Match compound <?isRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<-<?isRunic>>$/ ), q{Don't match externally inverted <?isRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]-<?isRunic>>$/ ), q{Don't match compound inverted <?isRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<-isRunic>>$/ ), q{Don't match internally inverted <?isRunic>} );
ok(!( "\c[BLACK HEART SUIT]"  ~~ m/^<+<?isRunic>>$/ ), q{Don't match unrelated <?isRunic>} );
ok("\c[BLACK HEART SUIT]"  ~~ m/^<-<?isRunic>>$/, q{Match unrelated externally inverted <?isRunic>} );
ok("\c[BLACK HEART SUIT]"  ~~ m/^<+<-isRunic>>$/, q{Match unrelated internally inverted <?isRunic>} );
ok("\c[BLACK HEART SUIT]\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/<+<?isRunic>>/, q{Match unanchored <?isRunic>} );

# Sinhala


ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<?isSinhala>>$/, q{Match <?isSinhala>} );
ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<[A]+<?isSinhala>>$/, q{Match compound <?isSinhala>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<-<?isSinhala>>$/ ), q{Don't match externally inverted <?isSinhala>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<[A]-<?isSinhala>>$/ ), q{Don't match compound inverted <?isSinhala>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<-isSinhala>>$/ ), q{Don't match internally inverted <?isSinhala>} );
ok(!( "\c[MYANMAR VOWEL SIGN II]"  ~~ m/^<+<?isSinhala>>$/ ), q{Don't match unrelated <?isSinhala>} );
ok("\c[MYANMAR VOWEL SIGN II]"  ~~ m/^<-<?isSinhala>>$/, q{Match unrelated externally inverted <?isSinhala>} );
ok("\c[MYANMAR VOWEL SIGN II]"  ~~ m/^<+<-isSinhala>>$/, q{Match unrelated internally inverted <?isSinhala>} );
ok(!( "\c[KHMER CURRENCY SYMBOL RIEL]" ~~ m/^<+<?isSinhala>>$/ ), q{Don't match related <?isSinhala>} );
ok("\c[KHMER CURRENCY SYMBOL RIEL]" ~~ m/^<+<-isSinhala>>$/, q{Match related internally inverted <?isSinhala>} );
ok("\c[KHMER CURRENCY SYMBOL RIEL]" ~~ m/^<-<?isSinhala>>$/, q{Match related externally inverted <?isSinhala>} );
ok("\c[MYANMAR VOWEL SIGN II]\c[KHMER CURRENCY SYMBOL RIEL]\c[SINHALA SIGN ANUSVARAYA]" ~~ m/<+<?isSinhala>>/, q{Match unanchored <?isSinhala>} );

# Syriac


ok("\c[SYRIAC LETTER ALAPH]" ~~ m/^<+<?isSyriac>>$/, q{Match <?isSyriac>} );
ok("\c[SYRIAC LETTER ALAPH]" ~~ m/^<[A]+<?isSyriac>>$/, q{Match compound <?isSyriac>} );
ok(!( "\c[SYRIAC LETTER ALAPH]" ~~ m/^<-<?isSyriac>>$/ ), q{Don't match externally inverted <?isSyriac>} );
ok(!( "\c[SYRIAC LETTER ALAPH]" ~~ m/^<[A]-<?isSyriac>>$/ ), q{Don't match compound inverted <?isSyriac>} );
ok(!( "\c[SYRIAC LETTER ALAPH]" ~~ m/^<+<-isSyriac>>$/ ), q{Don't match internally inverted <?isSyriac>} );
ok(!( "\x[7BAA]"  ~~ m/^<+<?isSyriac>>$/ ), q{Don't match unrelated <?isSyriac>} );
ok("\x[7BAA]"  ~~ m/^<-<?isSyriac>>$/, q{Match unrelated externally inverted <?isSyriac>} );
ok("\x[7BAA]"  ~~ m/^<+<-isSyriac>>$/, q{Match unrelated internally inverted <?isSyriac>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<?isSyriac>>$/ ), q{Don't match related <?isSyriac>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<+<-isSyriac>>$/, q{Match related internally inverted <?isSyriac>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-<?isSyriac>>$/, q{Match related externally inverted <?isSyriac>} );
ok("\x[7BAA]\c[YI RADICAL QOT]\c[SYRIAC LETTER ALAPH]" ~~ m/<+<?isSyriac>>/, q{Match unanchored <?isSyriac>} );

# Tagalog


ok("\c[TAGALOG LETTER A]" ~~ m/^<+<?isTagalog>>$/, q{Match <?isTagalog>} );
ok("\c[TAGALOG LETTER A]" ~~ m/^<[A]+<?isTagalog>>$/, q{Match compound <?isTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<-<?isTagalog>>$/ ), q{Don't match externally inverted <?isTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<[A]-<?isTagalog>>$/ ), q{Don't match compound inverted <?isTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<+<-isTagalog>>$/ ), q{Don't match internally inverted <?isTagalog>} );
ok(!( "\x[50F2]"  ~~ m/^<+<?isTagalog>>$/ ), q{Don't match unrelated <?isTagalog>} );
ok("\x[50F2]"  ~~ m/^<-<?isTagalog>>$/, q{Match unrelated externally inverted <?isTagalog>} );
ok("\x[50F2]"  ~~ m/^<+<-isTagalog>>$/, q{Match unrelated internally inverted <?isTagalog>} );
ok("\x[50F2]\c[TAGALOG LETTER A]" ~~ m/<+<?isTagalog>>/, q{Match unanchored <?isTagalog>} );

# Tagbanwa


ok("\c[TAGBANWA LETTER A]" ~~ m/^<+<?isTagbanwa>>$/, q{Match <?isTagbanwa>} );
ok("\c[TAGBANWA LETTER A]" ~~ m/^<[A]+<?isTagbanwa>>$/, q{Match compound <?isTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<-<?isTagbanwa>>$/ ), q{Don't match externally inverted <?isTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<[A]-<?isTagbanwa>>$/ ), q{Don't match compound inverted <?isTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<+<-isTagbanwa>>$/ ), q{Don't match internally inverted <?isTagbanwa>} );
ok(!( "\x[8843]"  ~~ m/^<+<?isTagbanwa>>$/ ), q{Don't match unrelated <?isTagbanwa>} );
ok("\x[8843]"  ~~ m/^<-<?isTagbanwa>>$/, q{Match unrelated externally inverted <?isTagbanwa>} );
ok("\x[8843]"  ~~ m/^<+<-isTagbanwa>>$/, q{Match unrelated internally inverted <?isTagbanwa>} );
ok("\x[8843]\c[TAGBANWA LETTER A]" ~~ m/<+<?isTagbanwa>>/, q{Match unanchored <?isTagbanwa>} );

# Tamil


ok("\c[TAMIL SIGN ANUSVARA]" ~~ m/^<+<?isTamil>>$/, q{Match <?isTamil>} );
ok("\c[TAMIL SIGN ANUSVARA]" ~~ m/^<[A]+<?isTamil>>$/, q{Match compound <?isTamil>} );
ok(!( "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<-<?isTamil>>$/ ), q{Don't match externally inverted <?isTamil>} );
ok(!( "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<[A]-<?isTamil>>$/ ), q{Don't match compound inverted <?isTamil>} );
ok(!( "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<+<-isTamil>>$/ ), q{Don't match internally inverted <?isTamil>} );
ok(!( "\x[76C0]"  ~~ m/^<+<?isTamil>>$/ ), q{Don't match unrelated <?isTamil>} );
ok("\x[76C0]"  ~~ m/^<-<?isTamil>>$/, q{Match unrelated externally inverted <?isTamil>} );
ok("\x[76C0]"  ~~ m/^<+<-isTamil>>$/, q{Match unrelated internally inverted <?isTamil>} );
ok("\x[76C0]\c[TAMIL SIGN ANUSVARA]" ~~ m/<+<?isTamil>>/, q{Match unanchored <?isTamil>} );

# Telugu


ok("\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<+<?isTelugu>>$/, q{Match <?isTelugu>} );
ok("\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<[A]+<?isTelugu>>$/, q{Match compound <?isTelugu>} );
ok(!( "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<-<?isTelugu>>$/ ), q{Don't match externally inverted <?isTelugu>} );
ok(!( "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<[A]-<?isTelugu>>$/ ), q{Don't match compound inverted <?isTelugu>} );
ok(!( "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<+<-isTelugu>>$/ ), q{Don't match internally inverted <?isTelugu>} );
ok(!( "\x[60BD]"  ~~ m/^<+<?isTelugu>>$/ ), q{Don't match unrelated <?isTelugu>} );
ok("\x[60BD]"  ~~ m/^<-<?isTelugu>>$/, q{Match unrelated externally inverted <?isTelugu>} );
ok("\x[60BD]"  ~~ m/^<+<-isTelugu>>$/, q{Match unrelated internally inverted <?isTelugu>} );
ok("\x[60BD]\c[TELUGU SIGN CANDRABINDU]" ~~ m/<+<?isTelugu>>/, q{Match unanchored <?isTelugu>} );

# Thaana


ok("\c[THAANA LETTER HAA]" ~~ m/^<+<?isThaana>>$/, q{Match <?isThaana>} );
ok("\c[THAANA LETTER HAA]" ~~ m/^<[A]+<?isThaana>>$/, q{Match compound <?isThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<-<?isThaana>>$/ ), q{Don't match externally inverted <?isThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<[A]-<?isThaana>>$/ ), q{Don't match compound inverted <?isThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<+<-isThaana>>$/ ), q{Don't match internally inverted <?isThaana>} );
ok(!( "\x[2E74]"  ~~ m/^<+<?isThaana>>$/ ), q{Don't match unrelated <?isThaana>} );
ok("\x[2E74]"  ~~ m/^<-<?isThaana>>$/, q{Match unrelated externally inverted <?isThaana>} );
ok("\x[2E74]"  ~~ m/^<+<-isThaana>>$/, q{Match unrelated internally inverted <?isThaana>} );
ok("\x[2E74]\c[THAANA LETTER HAA]" ~~ m/<+<?isThaana>>/, q{Match unanchored <?isThaana>} );

# Thai


ok("\c[THAI CHARACTER KO KAI]" ~~ m/^<+<?isThai>>$/, q{Match <?isThai>} );
ok("\c[THAI CHARACTER KO KAI]" ~~ m/^<[A]+<?isThai>>$/, q{Match compound <?isThai>} );
ok(!( "\c[THAI CHARACTER KO KAI]" ~~ m/^<-<?isThai>>$/ ), q{Don't match externally inverted <?isThai>} );
ok(!( "\c[THAI CHARACTER KO KAI]" ~~ m/^<[A]-<?isThai>>$/ ), q{Don't match compound inverted <?isThai>} );
ok(!( "\c[THAI CHARACTER KO KAI]" ~~ m/^<+<-isThai>>$/ ), q{Don't match internally inverted <?isThai>} );
ok(!( "\x[A929]"  ~~ m/^<+<?isThai>>$/ ), q{Don't match unrelated <?isThai>} );
ok("\x[A929]"  ~~ m/^<-<?isThai>>$/, q{Match unrelated externally inverted <?isThai>} );
ok("\x[A929]"  ~~ m/^<+<-isThai>>$/, q{Match unrelated internally inverted <?isThai>} );
ok("\x[A929]\c[THAI CHARACTER KO KAI]" ~~ m/<+<?isThai>>/, q{Match unanchored <?isThai>} );

# Tibetan


ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<?isTibetan>>$/, q{Match <?isTibetan>} );
ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]+<?isTibetan>>$/, q{Match compound <?isTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<-<?isTibetan>>$/ ), q{Don't match externally inverted <?isTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]-<?isTibetan>>$/ ), q{Don't match compound inverted <?isTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<-isTibetan>>$/ ), q{Don't match internally inverted <?isTibetan>} );
ok(!( "\x[19C9]"  ~~ m/^<+<?isTibetan>>$/ ), q{Don't match unrelated <?isTibetan>} );
ok("\x[19C9]"  ~~ m/^<-<?isTibetan>>$/, q{Match unrelated externally inverted <?isTibetan>} );
ok("\x[19C9]"  ~~ m/^<+<-isTibetan>>$/, q{Match unrelated internally inverted <?isTibetan>} );
ok("\x[19C9]\c[TIBETAN SYLLABLE OM]" ~~ m/<+<?isTibetan>>/, q{Match unanchored <?isTibetan>} );

# Yi


ok("\c[YI SYLLABLE IT]" ~~ m/^<+<?isYi>>$/, q{Match <?isYi>} );
ok("\c[YI SYLLABLE IT]" ~~ m/^<[A]+<?isYi>>$/, q{Match compound <?isYi>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<-<?isYi>>$/ ), q{Don't match externally inverted <?isYi>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<[A]-<?isYi>>$/ ), q{Don't match compound inverted <?isYi>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<+<-isYi>>$/ ), q{Don't match internally inverted <?isYi>} );
ok(!( "\x[3A38]"  ~~ m/^<+<?isYi>>$/ ), q{Don't match unrelated <?isYi>} );
ok("\x[3A38]"  ~~ m/^<-<?isYi>>$/, q{Match unrelated externally inverted <?isYi>} );
ok("\x[3A38]"  ~~ m/^<+<-isYi>>$/, q{Match unrelated internally inverted <?isYi>} );
ok("\x[3A38]\c[YI SYLLABLE IT]" ~~ m/<+<?isYi>>/, q{Match unanchored <?isYi>} );

# ASCIIHexDigit


ok("\c[DIGIT ZERO]" ~~ m/^<+<?isASCIIHexDigit>>$/, q{Match <?isASCIIHexDigit>} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?isASCIIHexDigit>>$/, q{Match compound <?isASCIIHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?isASCIIHexDigit>>$/ ), q{Don't match externally inverted <?isASCIIHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?isASCIIHexDigit>>$/ ), q{Don't match compound inverted <?isASCIIHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-isASCIIHexDigit>>$/ ), q{Don't match internally inverted <?isASCIIHexDigit>} );
ok(!( "\x[55D7]"  ~~ m/^<+<?isASCIIHexDigit>>$/ ), q{Don't match unrelated <?isASCIIHexDigit>} );
ok("\x[55D7]"  ~~ m/^<-<?isASCIIHexDigit>>$/, q{Match unrelated externally inverted <?isASCIIHexDigit>} );
ok("\x[55D7]"  ~~ m/^<+<-isASCIIHexDigit>>$/, q{Match unrelated internally inverted <?isASCIIHexDigit>} );
ok("\x[55D7]\c[DIGIT ZERO]" ~~ m/<+<?isASCIIHexDigit>>/, q{Match unanchored <?isASCIIHexDigit>} );

# Dash


ok("\c[HYPHEN-MINUS]" ~~ m/^<+<?isDash>>$/, q{Match <?isDash>} );
ok("\c[HYPHEN-MINUS]" ~~ m/^<[A]+<?isDash>>$/, q{Match compound <?isDash>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<-<?isDash>>$/ ), q{Don't match externally inverted <?isDash>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<[A]-<?isDash>>$/ ), q{Don't match compound inverted <?isDash>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<+<-isDash>>$/ ), q{Don't match internally inverted <?isDash>} );
ok(!( "\x[4C7F]"  ~~ m/^<+<?isDash>>$/ ), q{Don't match unrelated <?isDash>} );
ok("\x[4C7F]"  ~~ m/^<-<?isDash>>$/, q{Match unrelated externally inverted <?isDash>} );
ok("\x[4C7F]"  ~~ m/^<+<-isDash>>$/, q{Match unrelated internally inverted <?isDash>} );
ok("\x[4C7F]\c[HYPHEN-MINUS]" ~~ m/<+<?isDash>>/, q{Match unanchored <?isDash>} );

# Diacritic


ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<?isDiacritic>>$/, q{Match <?isDiacritic>} );
ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]+<?isDiacritic>>$/, q{Match compound <?isDiacritic>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-<?isDiacritic>>$/ ), q{Don't match externally inverted <?isDiacritic>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]-<?isDiacritic>>$/ ), q{Don't match compound inverted <?isDiacritic>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<-isDiacritic>>$/ ), q{Don't match internally inverted <?isDiacritic>} );
ok(!( "\x[398E]"  ~~ m/^<+<?isDiacritic>>$/ ), q{Don't match unrelated <?isDiacritic>} );
ok("\x[398E]"  ~~ m/^<-<?isDiacritic>>$/, q{Match unrelated externally inverted <?isDiacritic>} );
ok("\x[398E]"  ~~ m/^<+<-isDiacritic>>$/, q{Match unrelated internally inverted <?isDiacritic>} );
ok("\x[398E]\c[CIRCUMFLEX ACCENT]" ~~ m/<+<?isDiacritic>>/, q{Match unanchored <?isDiacritic>} );

# Extender


ok("\c[MIDDLE DOT]" ~~ m/^<+<?isExtender>>$/, q{Match <?isExtender>} );
ok("\c[MIDDLE DOT]" ~~ m/^<[A]+<?isExtender>>$/, q{Match compound <?isExtender>} );
ok(!( "\c[MIDDLE DOT]" ~~ m/^<-<?isExtender>>$/ ), q{Don't match externally inverted <?isExtender>} );
ok(!( "\c[MIDDLE DOT]" ~~ m/^<[A]-<?isExtender>>$/ ), q{Don't match compound inverted <?isExtender>} );
ok(!( "\c[MIDDLE DOT]" ~~ m/^<+<-isExtender>>$/ ), q{Don't match internally inverted <?isExtender>} );
ok(!( "\x[3F66]"  ~~ m/^<+<?isExtender>>$/ ), q{Don't match unrelated <?isExtender>} );
ok("\x[3F66]"  ~~ m/^<-<?isExtender>>$/, q{Match unrelated externally inverted <?isExtender>} );
ok("\x[3F66]"  ~~ m/^<+<-isExtender>>$/, q{Match unrelated internally inverted <?isExtender>} );
ok("\x[3F66]\c[MIDDLE DOT]" ~~ m/<+<?isExtender>>/, q{Match unanchored <?isExtender>} );

# GraphemeLink


ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<?isGraphemeLink>>$/, q{Match <?isGraphemeLink>} );
ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]+<?isGraphemeLink>>$/, q{Match compound <?isGraphemeLink>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<-<?isGraphemeLink>>$/ ), q{Don't match externally inverted <?isGraphemeLink>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]-<?isGraphemeLink>>$/ ), q{Don't match compound inverted <?isGraphemeLink>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<-isGraphemeLink>>$/ ), q{Don't match internally inverted <?isGraphemeLink>} );
ok(!( "\x[34DA]"  ~~ m/^<+<?isGraphemeLink>>$/ ), q{Don't match unrelated <?isGraphemeLink>} );
ok("\x[34DA]"  ~~ m/^<-<?isGraphemeLink>>$/, q{Match unrelated externally inverted <?isGraphemeLink>} );
ok("\x[34DA]"  ~~ m/^<+<-isGraphemeLink>>$/, q{Match unrelated internally inverted <?isGraphemeLink>} );
ok("\x[34DA]\c[COMBINING GRAPHEME JOINER]" ~~ m/<+<?isGraphemeLink>>/, q{Match unanchored <?isGraphemeLink>} );

# HexDigit


ok("\c[DIGIT ZERO]" ~~ m/^<+<?isHexDigit>>$/, q{Match <?isHexDigit>} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?isHexDigit>>$/, q{Match compound <?isHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?isHexDigit>>$/ ), q{Don't match externally inverted <?isHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?isHexDigit>>$/ ), q{Don't match compound inverted <?isHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-isHexDigit>>$/ ), q{Don't match internally inverted <?isHexDigit>} );
ok(!( "\x[D446]"  ~~ m/^<+<?isHexDigit>>$/ ), q{Don't match unrelated <?isHexDigit>} );
ok("\x[D446]"  ~~ m/^<-<?isHexDigit>>$/, q{Match unrelated externally inverted <?isHexDigit>} );
ok("\x[D446]"  ~~ m/^<+<-isHexDigit>>$/, q{Match unrelated internally inverted <?isHexDigit>} );
ok("\x[D446]\c[DIGIT ZERO]" ~~ m/<+<?isHexDigit>>/, q{Match unanchored <?isHexDigit>} );

# Hyphen


ok("\c[HYPHEN-MINUS]" ~~ m/^<+<?isHyphen>>$/, q{Match <?isHyphen>} );
ok("\c[HYPHEN-MINUS]" ~~ m/^<[A]+<?isHyphen>>$/, q{Match compound <?isHyphen>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<-<?isHyphen>>$/ ), q{Don't match externally inverted <?isHyphen>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<[A]-<?isHyphen>>$/ ), q{Don't match compound inverted <?isHyphen>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<+<-isHyphen>>$/ ), q{Don't match internally inverted <?isHyphen>} );
ok(!( "\c[YI SYLLABLE WOX]"  ~~ m/^<+<?isHyphen>>$/ ), q{Don't match unrelated <?isHyphen>} );
ok("\c[YI SYLLABLE WOX]"  ~~ m/^<-<?isHyphen>>$/, q{Match unrelated externally inverted <?isHyphen>} );
ok("\c[YI SYLLABLE WOX]"  ~~ m/^<+<-isHyphen>>$/, q{Match unrelated internally inverted <?isHyphen>} );
ok("\c[YI SYLLABLE WOX]\c[HYPHEN-MINUS]" ~~ m/<+<?isHyphen>>/, q{Match unanchored <?isHyphen>} );

# Ideographic


ok("\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<+<?isIdeographic>>$/, q{Match <?isIdeographic>} );
ok("\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<[A]+<?isIdeographic>>$/, q{Match compound <?isIdeographic>} );
ok(!( "\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<-<?isIdeographic>>$/ ), q{Don't match externally inverted <?isIdeographic>} );
ok(!( "\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<[A]-<?isIdeographic>>$/ ), q{Don't match compound inverted <?isIdeographic>} );
ok(!( "\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<+<-isIdeographic>>$/ ), q{Don't match internally inverted <?isIdeographic>} );
ok(!( "\x[CB86]"  ~~ m/^<+<?isIdeographic>>$/ ), q{Don't match unrelated <?isIdeographic>} );
ok("\x[CB86]"  ~~ m/^<-<?isIdeographic>>$/, q{Match unrelated externally inverted <?isIdeographic>} );
ok("\x[CB86]"  ~~ m/^<+<-isIdeographic>>$/, q{Match unrelated internally inverted <?isIdeographic>} );
ok("\x[CB86]\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/<+<?isIdeographic>>/, q{Match unanchored <?isIdeographic>} );

# IDSBinaryOperator


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<?isIDSBinaryOperator>>$/, q{Match <?isIDSBinaryOperator>} );
ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]+<?isIDSBinaryOperator>>$/, q{Match compound <?isIDSBinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<-<?isIDSBinaryOperator>>$/ ), q{Don't match externally inverted <?isIDSBinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]-<?isIDSBinaryOperator>>$/ ), q{Don't match compound inverted <?isIDSBinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<-isIDSBinaryOperator>>$/ ), q{Don't match internally inverted <?isIDSBinaryOperator>} );
ok(!( "\x[54A0]"  ~~ m/^<+<?isIDSBinaryOperator>>$/ ), q{Don't match unrelated <?isIDSBinaryOperator>} );
ok("\x[54A0]"  ~~ m/^<-<?isIDSBinaryOperator>>$/, q{Match unrelated externally inverted <?isIDSBinaryOperator>} );
ok("\x[54A0]"  ~~ m/^<+<-isIDSBinaryOperator>>$/, q{Match unrelated internally inverted <?isIDSBinaryOperator>} );
ok("\x[54A0]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/<+<?isIDSBinaryOperator>>/, q{Match unanchored <?isIDSBinaryOperator>} );

# IDSTrinaryOperator


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<+<?isIDSTrinaryOperator>>$/, q{Match <?isIDSTrinaryOperator>} );
ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<[A]+<?isIDSTrinaryOperator>>$/, q{Match compound <?isIDSTrinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<-<?isIDSTrinaryOperator>>$/ ), q{Don't match externally inverted <?isIDSTrinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<[A]-<?isIDSTrinaryOperator>>$/ ), q{Don't match compound inverted <?isIDSTrinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<+<-isIDSTrinaryOperator>>$/ ), q{Don't match internally inverted <?isIDSTrinaryOperator>} );
ok(!( "\x[4900]"  ~~ m/^<+<?isIDSTrinaryOperator>>$/ ), q{Don't match unrelated <?isIDSTrinaryOperator>} );
ok("\x[4900]"  ~~ m/^<-<?isIDSTrinaryOperator>>$/, q{Match unrelated externally inverted <?isIDSTrinaryOperator>} );
ok("\x[4900]"  ~~ m/^<+<-isIDSTrinaryOperator>>$/, q{Match unrelated internally inverted <?isIDSTrinaryOperator>} );
ok("\x[4900]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/<+<?isIDSTrinaryOperator>>/, q{Match unanchored <?isIDSTrinaryOperator>} );

# JoinControl


ok("\c[ZERO WIDTH NON-JOINER]" ~~ m/^<+<?isJoinControl>>$/, q{Match <?isJoinControl>} );
ok("\c[ZERO WIDTH NON-JOINER]" ~~ m/^<[A]+<?isJoinControl>>$/, q{Match compound <?isJoinControl>} );
ok(!( "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<-<?isJoinControl>>$/ ), q{Don't match externally inverted <?isJoinControl>} );
ok(!( "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<[A]-<?isJoinControl>>$/ ), q{Don't match compound inverted <?isJoinControl>} );
ok(!( "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<+<-isJoinControl>>$/ ), q{Don't match internally inverted <?isJoinControl>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER OT]"  ~~ m/^<+<?isJoinControl>>$/ ), q{Don't match unrelated <?isJoinControl>} );
ok("\c[CYRILLIC CAPITAL LETTER OT]"  ~~ m/^<-<?isJoinControl>>$/, q{Match unrelated externally inverted <?isJoinControl>} );
ok("\c[CYRILLIC CAPITAL LETTER OT]"  ~~ m/^<+<-isJoinControl>>$/, q{Match unrelated internally inverted <?isJoinControl>} );
ok("\c[CYRILLIC CAPITAL LETTER OT]\c[ZERO WIDTH NON-JOINER]" ~~ m/<+<?isJoinControl>>/, q{Match unanchored <?isJoinControl>} );

# LogicalOrderException


ok("\c[THAI CHARACTER SARA E]" ~~ m/^<+<?isLogicalOrderException>>$/, q{Match <?isLogicalOrderException>} );
ok("\c[THAI CHARACTER SARA E]" ~~ m/^<[A]+<?isLogicalOrderException>>$/, q{Match compound <?isLogicalOrderException>} );
ok(!( "\c[THAI CHARACTER SARA E]" ~~ m/^<-<?isLogicalOrderException>>$/ ), q{Don't match externally inverted <?isLogicalOrderException>} );
ok(!( "\c[THAI CHARACTER SARA E]" ~~ m/^<[A]-<?isLogicalOrderException>>$/ ), q{Don't match compound inverted <?isLogicalOrderException>} );
ok(!( "\c[THAI CHARACTER SARA E]" ~~ m/^<+<-isLogicalOrderException>>$/ ), q{Don't match internally inverted <?isLogicalOrderException>} );
ok(!( "\x[88D2]"  ~~ m/^<+<?isLogicalOrderException>>$/ ), q{Don't match unrelated <?isLogicalOrderException>} );
ok("\x[88D2]"  ~~ m/^<-<?isLogicalOrderException>>$/, q{Match unrelated externally inverted <?isLogicalOrderException>} );
ok("\x[88D2]"  ~~ m/^<+<-isLogicalOrderException>>$/, q{Match unrelated internally inverted <?isLogicalOrderException>} );
ok(!( "\x[88D2]" ~~ m/^<+<?isLogicalOrderException>>$/ ), q{Don't match related <?isLogicalOrderException>} );
ok("\x[88D2]" ~~ m/^<+<-isLogicalOrderException>>$/, q{Match related internally inverted <?isLogicalOrderException>} );
ok("\x[88D2]" ~~ m/^<-<?isLogicalOrderException>>$/, q{Match related externally inverted <?isLogicalOrderException>} );
ok("\x[88D2]\x[88D2]\c[THAI CHARACTER SARA E]" ~~ m/<+<?isLogicalOrderException>>/, q{Match unanchored <?isLogicalOrderException>} );

# NoncharacterCodePoint


ok(!( "\c[CIRCLED HANGUL NIEUN A]"  ~~ m/^<+<?isNoncharacterCodePoint>>$/ ), q{Don't match unrelated <?isNoncharacterCodePoint>} );
ok("\c[CIRCLED HANGUL NIEUN A]"  ~~ m/^<-<?isNoncharacterCodePoint>>$/, q{Match unrelated externally inverted <?isNoncharacterCodePoint>} );
ok("\c[CIRCLED HANGUL NIEUN A]"  ~~ m/^<+<-isNoncharacterCodePoint>>$/, q{Match unrelated internally inverted <?isNoncharacterCodePoint>} );
ok(!( "\c[CIRCLED IDEOGRAPH ONE]" ~~ m/^<+<?isNoncharacterCodePoint>>$/ ), q{Don't match related <?isNoncharacterCodePoint>} );
ok("\c[CIRCLED IDEOGRAPH ONE]" ~~ m/^<+<-isNoncharacterCodePoint>>$/, q{Match related internally inverted <?isNoncharacterCodePoint>} );
ok("\c[CIRCLED IDEOGRAPH ONE]" ~~ m/^<-<?isNoncharacterCodePoint>>$/, q{Match related externally inverted <?isNoncharacterCodePoint>} );

# OtherAlphabetic


ok("\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<+<?isOtherAlphabetic>>$/, q{Match <?isOtherAlphabetic>} );
ok("\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<[A]+<?isOtherAlphabetic>>$/, q{Match compound <?isOtherAlphabetic>} );
ok(!( "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<-<?isOtherAlphabetic>>$/ ), q{Don't match externally inverted <?isOtherAlphabetic>} );
ok(!( "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<[A]-<?isOtherAlphabetic>>$/ ), q{Don't match compound inverted <?isOtherAlphabetic>} );
ok(!( "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<+<-isOtherAlphabetic>>$/ ), q{Don't match internally inverted <?isOtherAlphabetic>} );
ok(!( "\x[7B02]"  ~~ m/^<+<?isOtherAlphabetic>>$/ ), q{Don't match unrelated <?isOtherAlphabetic>} );
ok("\x[7B02]"  ~~ m/^<-<?isOtherAlphabetic>>$/, q{Match unrelated externally inverted <?isOtherAlphabetic>} );
ok("\x[7B02]"  ~~ m/^<+<-isOtherAlphabetic>>$/, q{Match unrelated internally inverted <?isOtherAlphabetic>} );
ok("\x[7B02]\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/<+<?isOtherAlphabetic>>/, q{Match unanchored <?isOtherAlphabetic>} );

# OtherDefaultIgnorableCodePoint


ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<?isOtherDefaultIgnorableCodePoint>>$/, q{Match <?isOtherDefaultIgnorableCodePoint>} );
ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]+<?isOtherDefaultIgnorableCodePoint>>$/, q{Match compound <?isOtherDefaultIgnorableCodePoint>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<-<?isOtherDefaultIgnorableCodePoint>>$/ ), q{Don't match externally inverted <?isOtherDefaultIgnorableCodePoint>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]-<?isOtherDefaultIgnorableCodePoint>>$/ ), q{Don't match compound inverted <?isOtherDefaultIgnorableCodePoint>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<-isOtherDefaultIgnorableCodePoint>>$/ ), q{Don't match internally inverted <?isOtherDefaultIgnorableCodePoint>} );
ok(!( "\x[42DA]"  ~~ m/^<+<?isOtherDefaultIgnorableCodePoint>>$/ ), q{Don't match unrelated <?isOtherDefaultIgnorableCodePoint>} );
ok("\x[42DA]"  ~~ m/^<-<?isOtherDefaultIgnorableCodePoint>>$/, q{Match unrelated externally inverted <?isOtherDefaultIgnorableCodePoint>} );
ok("\x[42DA]"  ~~ m/^<+<-isOtherDefaultIgnorableCodePoint>>$/, q{Match unrelated internally inverted <?isOtherDefaultIgnorableCodePoint>} );
ok("\x[42DA]\c[COMBINING GRAPHEME JOINER]" ~~ m/<+<?isOtherDefaultIgnorableCodePoint>>/, q{Match unanchored <?isOtherDefaultIgnorableCodePoint>} );

# OtherGraphemeExtend


ok("\c[BENGALI VOWEL SIGN AA]" ~~ m/^<+<?isOtherGraphemeExtend>>$/, q{Match <?isOtherGraphemeExtend>} );
ok("\c[BENGALI VOWEL SIGN AA]" ~~ m/^<[A]+<?isOtherGraphemeExtend>>$/, q{Match compound <?isOtherGraphemeExtend>} );
ok(!( "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<-<?isOtherGraphemeExtend>>$/ ), q{Don't match externally inverted <?isOtherGraphemeExtend>} );
ok(!( "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<[A]-<?isOtherGraphemeExtend>>$/ ), q{Don't match compound inverted <?isOtherGraphemeExtend>} );
ok(!( "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<+<-isOtherGraphemeExtend>>$/ ), q{Don't match internally inverted <?isOtherGraphemeExtend>} );
ok(!( "\c[TAI LE LETTER KHA]"  ~~ m/^<+<?isOtherGraphemeExtend>>$/ ), q{Don't match unrelated <?isOtherGraphemeExtend>} );
ok("\c[TAI LE LETTER KHA]"  ~~ m/^<-<?isOtherGraphemeExtend>>$/, q{Match unrelated externally inverted <?isOtherGraphemeExtend>} );
ok("\c[TAI LE LETTER KHA]"  ~~ m/^<+<-isOtherGraphemeExtend>>$/, q{Match unrelated internally inverted <?isOtherGraphemeExtend>} );
ok("\c[TAI LE LETTER KHA]\c[BENGALI VOWEL SIGN AA]" ~~ m/<+<?isOtherGraphemeExtend>>/, q{Match unanchored <?isOtherGraphemeExtend>} );

# OtherLowercase


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?isOtherLowercase>>$/, q{Match <?isOtherLowercase>} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?isOtherLowercase>>$/, q{Match compound <?isOtherLowercase>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?isOtherLowercase>>$/ ), q{Don't match externally inverted <?isOtherLowercase>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?isOtherLowercase>>$/ ), q{Don't match compound inverted <?isOtherLowercase>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-isOtherLowercase>>$/ ), q{Don't match internally inverted <?isOtherLowercase>} );
ok(!( "\x[8153]"  ~~ m/^<+<?isOtherLowercase>>$/ ), q{Don't match unrelated <?isOtherLowercase>} );
ok("\x[8153]"  ~~ m/^<-<?isOtherLowercase>>$/, q{Match unrelated externally inverted <?isOtherLowercase>} );
ok("\x[8153]"  ~~ m/^<+<-isOtherLowercase>>$/, q{Match unrelated internally inverted <?isOtherLowercase>} );
ok("\x[8153]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?isOtherLowercase>>/, q{Match unanchored <?isOtherLowercase>} );

# OtherMath


ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?isOtherMath>>$/, q{Match <?isOtherMath>} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?isOtherMath>>$/, q{Match compound <?isOtherMath>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?isOtherMath>>$/ ), q{Don't match externally inverted <?isOtherMath>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?isOtherMath>>$/ ), q{Don't match compound inverted <?isOtherMath>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-isOtherMath>>$/ ), q{Don't match internally inverted <?isOtherMath>} );
ok(!( "\x[6D2F]"  ~~ m/^<+<?isOtherMath>>$/ ), q{Don't match unrelated <?isOtherMath>} );
ok("\x[6D2F]"  ~~ m/^<-<?isOtherMath>>$/, q{Match unrelated externally inverted <?isOtherMath>} );
ok("\x[6D2F]"  ~~ m/^<+<-isOtherMath>>$/, q{Match unrelated internally inverted <?isOtherMath>} );
ok("\x[6D2F]\c[LEFT PARENTHESIS]" ~~ m/<+<?isOtherMath>>/, q{Match unanchored <?isOtherMath>} );

# OtherUppercase


ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<+<?isOtherUppercase>>$/, q{Match <?isOtherUppercase>} );
ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]+<?isOtherUppercase>>$/, q{Match compound <?isOtherUppercase>} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<-<?isOtherUppercase>>$/ ), q{Don't match externally inverted <?isOtherUppercase>} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]-<?isOtherUppercase>>$/ ), q{Don't match compound inverted <?isOtherUppercase>} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<+<-isOtherUppercase>>$/ ), q{Don't match internally inverted <?isOtherUppercase>} );
ok(!( "\x[A746]"  ~~ m/^<+<?isOtherUppercase>>$/ ), q{Don't match unrelated <?isOtherUppercase>} );
ok("\x[A746]"  ~~ m/^<-<?isOtherUppercase>>$/, q{Match unrelated externally inverted <?isOtherUppercase>} );
ok("\x[A746]"  ~~ m/^<+<-isOtherUppercase>>$/, q{Match unrelated internally inverted <?isOtherUppercase>} );
ok("\x[A746]\c[ROMAN NUMERAL ONE]" ~~ m/<+<?isOtherUppercase>>/, q{Match unanchored <?isOtherUppercase>} );

# QuotationMark


ok("\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<+<?isQuotationMark>>$/, q{Match <?isQuotationMark>} );
ok("\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<[A]+<?isQuotationMark>>$/, q{Match compound <?isQuotationMark>} );
ok(!( "\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<-<?isQuotationMark>>$/ ), q{Don't match externally inverted <?isQuotationMark>} );
ok(!( "\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<[A]-<?isQuotationMark>>$/ ), q{Don't match compound inverted <?isQuotationMark>} );
ok(!( "\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<+<-isQuotationMark>>$/ ), q{Don't match internally inverted <?isQuotationMark>} );
ok(!( "\c[GURMUKHI VOWEL SIGN AI]"  ~~ m/^<+<?isQuotationMark>>$/ ), q{Don't match unrelated <?isQuotationMark>} );
ok("\c[GURMUKHI VOWEL SIGN AI]"  ~~ m/^<-<?isQuotationMark>>$/, q{Match unrelated externally inverted <?isQuotationMark>} );
ok("\c[GURMUKHI VOWEL SIGN AI]"  ~~ m/^<+<-isQuotationMark>>$/, q{Match unrelated internally inverted <?isQuotationMark>} );
ok("\c[GURMUKHI VOWEL SIGN AI]\c[LEFT SINGLE QUOTATION MARK]" ~~ m/<+<?isQuotationMark>>/, q{Match unanchored <?isQuotationMark>} );

# Radical


ok("\c[CJK RADICAL REPEAT]" ~~ m/^<+<?isRadical>>$/, q{Match <?isRadical>} );
ok("\c[CJK RADICAL REPEAT]" ~~ m/^<[A]+<?isRadical>>$/, q{Match compound <?isRadical>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<-<?isRadical>>$/ ), q{Don't match externally inverted <?isRadical>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<[A]-<?isRadical>>$/ ), q{Don't match compound inverted <?isRadical>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<+<-isRadical>>$/ ), q{Don't match internally inverted <?isRadical>} );
ok(!( "\c[RUNIC LETTER ETH]"  ~~ m/^<+<?isRadical>>$/ ), q{Don't match unrelated <?isRadical>} );
ok("\c[RUNIC LETTER ETH]"  ~~ m/^<-<?isRadical>>$/, q{Match unrelated externally inverted <?isRadical>} );
ok("\c[RUNIC LETTER ETH]"  ~~ m/^<+<-isRadical>>$/, q{Match unrelated internally inverted <?isRadical>} );
ok("\c[RUNIC LETTER ETH]\c[CJK RADICAL REPEAT]" ~~ m/<+<?isRadical>>/, q{Match unanchored <?isRadical>} );

# SoftDotted


ok("\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<+<?isSoftDotted>>$/, q{Match <?isSoftDotted>} );
ok("\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<[A]+<?isSoftDotted>>$/, q{Match compound <?isSoftDotted>} );
ok(!( "\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<-<?isSoftDotted>>$/ ), q{Don't match externally inverted <?isSoftDotted>} );
ok(!( "\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<[A]-<?isSoftDotted>>$/ ), q{Don't match compound inverted <?isSoftDotted>} );
ok(!( "\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<+<-isSoftDotted>>$/ ), q{Don't match internally inverted <?isSoftDotted>} );
ok(!( "\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]"  ~~ m/^<+<?isSoftDotted>>$/ ), q{Don't match unrelated <?isSoftDotted>} );
ok("\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]"  ~~ m/^<-<?isSoftDotted>>$/, q{Match unrelated externally inverted <?isSoftDotted>} );
ok("\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]"  ~~ m/^<+<-isSoftDotted>>$/, q{Match unrelated internally inverted <?isSoftDotted>} );
ok(!( "\c[GREEK KORONIS]" ~~ m/^<+<?isSoftDotted>>$/ ), q{Don't match related <?isSoftDotted>} );
ok("\c[GREEK KORONIS]" ~~ m/^<+<-isSoftDotted>>$/, q{Match related internally inverted <?isSoftDotted>} );
ok("\c[GREEK KORONIS]" ~~ m/^<-<?isSoftDotted>>$/, q{Match related externally inverted <?isSoftDotted>} );
ok("\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]\c[GREEK KORONIS]\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/<+<?isSoftDotted>>/, q{Match unanchored <?isSoftDotted>} );

# TerminalPunctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?isTerminalPunctuation>>$/, q{Match <?isTerminalPunctuation>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?isTerminalPunctuation>>$/, q{Match compound <?isTerminalPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?isTerminalPunctuation>>$/ ), q{Don't match externally inverted <?isTerminalPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?isTerminalPunctuation>>$/ ), q{Don't match compound inverted <?isTerminalPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-isTerminalPunctuation>>$/ ), q{Don't match internally inverted <?isTerminalPunctuation>} );
ok(!( "\x[B43A]"  ~~ m/^<+<?isTerminalPunctuation>>$/ ), q{Don't match unrelated <?isTerminalPunctuation>} );
ok("\x[B43A]"  ~~ m/^<-<?isTerminalPunctuation>>$/, q{Match unrelated externally inverted <?isTerminalPunctuation>} );
ok("\x[B43A]"  ~~ m/^<+<-isTerminalPunctuation>>$/, q{Match unrelated internally inverted <?isTerminalPunctuation>} );
ok("\x[B43A]\c[EXCLAMATION MARK]" ~~ m/<+<?isTerminalPunctuation>>/, q{Match unanchored <?isTerminalPunctuation>} );

# UnifiedIdeograph


ok("\x[88D8]" ~~ m/^<+<?isUnifiedIdeograph>>$/, q{Match <?isUnifiedIdeograph>} );
ok("\x[88D8]" ~~ m/^<[A]+<?isUnifiedIdeograph>>$/, q{Match compound <?isUnifiedIdeograph>} );
ok(!( "\x[88D8]" ~~ m/^<-<?isUnifiedIdeograph>>$/ ), q{Don't match externally inverted <?isUnifiedIdeograph>} );
ok(!( "\x[88D8]" ~~ m/^<[A]-<?isUnifiedIdeograph>>$/ ), q{Don't match compound inverted <?isUnifiedIdeograph>} );
ok(!( "\x[88D8]" ~~ m/^<+<-isUnifiedIdeograph>>$/ ), q{Don't match internally inverted <?isUnifiedIdeograph>} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?isUnifiedIdeograph>>$/ ), q{Don't match unrelated <?isUnifiedIdeograph>} );
ok("\x[9FA6]"  ~~ m/^<-<?isUnifiedIdeograph>>$/, q{Match unrelated externally inverted <?isUnifiedIdeograph>} );
ok("\x[9FA6]"  ~~ m/^<+<-isUnifiedIdeograph>>$/, q{Match unrelated internally inverted <?isUnifiedIdeograph>} );
ok("\x[9FA6]\x[88D8]" ~~ m/<+<?isUnifiedIdeograph>>/, q{Match unanchored <?isUnifiedIdeograph>} );

# WhiteSpace


ok("\c[CHARACTER TABULATION]" ~~ m/^<+<?isWhiteSpace>>$/, q{Match <?isWhiteSpace>} );
ok("\c[CHARACTER TABULATION]" ~~ m/^<[A]+<?isWhiteSpace>>$/, q{Match compound <?isWhiteSpace>} );
ok(!( "\c[CHARACTER TABULATION]" ~~ m/^<-<?isWhiteSpace>>$/ ), q{Don't match externally inverted <?isWhiteSpace>} );
ok(!( "\c[CHARACTER TABULATION]" ~~ m/^<[A]-<?isWhiteSpace>>$/ ), q{Don't match compound inverted <?isWhiteSpace>} );
ok(!( "\c[CHARACTER TABULATION]" ~~ m/^<+<-isWhiteSpace>>$/ ), q{Don't match internally inverted <?isWhiteSpace>} );
ok(!( "\x[4345]"  ~~ m/^<+<?isWhiteSpace>>$/ ), q{Don't match unrelated <?isWhiteSpace>} );
ok("\x[4345]"  ~~ m/^<-<?isWhiteSpace>>$/, q{Match unrelated externally inverted <?isWhiteSpace>} );
ok("\x[4345]"  ~~ m/^<+<-isWhiteSpace>>$/, q{Match unrelated internally inverted <?isWhiteSpace>} );
ok("\x[4345]\c[CHARACTER TABULATION]" ~~ m/<+<?isWhiteSpace>>/, q{Match unanchored <?isWhiteSpace>} );

# Alphabetic      # Lu + Ll + Lt + Lm + Lo + OtherAlphabetic


ok("\x[3816]" ~~ m/^<+<?isAlphabetic>>$/, q{Match (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[3816]" ~~ m/^<[A]+<?isAlphabetic>>$/, q{Match compound (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[3816]" ~~ m/^<-<?isAlphabetic>>$/ ), q{Don't match externally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[3816]" ~~ m/^<[A]-<?isAlphabetic>>$/ ), q{Don't match compound inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[3816]" ~~ m/^<+<-isAlphabetic>>$/ ), q{Don't match internally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[4DB6]"  ~~ m/^<+<?isAlphabetic>>$/ ), q{Don't match unrelated (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[4DB6]"  ~~ m/^<-<?isAlphabetic>>$/, q{Match unrelated externally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[4DB6]"  ~~ m/^<+<-isAlphabetic>>$/, q{Match unrelated internally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[4DB6]\x[3816]" ~~ m/<+<?isAlphabetic>>/, q{Match unanchored (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );

# Lowercase       # Ll + OtherLowercase


ok("\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<+<?isLowercase>>$/, q{Match (Ll + OtherLowercase)} );
ok("\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<[A]+<?isLowercase>>$/, q{Match compound (Ll + OtherLowercase)} );
ok(!( "\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<-<?isLowercase>>$/ ), q{Don't match externally inverted (Ll + OtherLowercase)} );
ok(!( "\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<[A]-<?isLowercase>>$/ ), q{Don't match compound inverted (Ll + OtherLowercase)} );
ok(!( "\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<+<-isLowercase>>$/ ), q{Don't match internally inverted (Ll + OtherLowercase)} );
ok(!( "\x[0D3D]"  ~~ m/^<+<?isLowercase>>$/ ), q{Don't match unrelated (Ll + OtherLowercase)} );
ok("\x[0D3D]"  ~~ m/^<-<?isLowercase>>$/, q{Match unrelated externally inverted (Ll + OtherLowercase)} );
ok("\x[0D3D]"  ~~ m/^<+<-isLowercase>>$/, q{Match unrelated internally inverted (Ll + OtherLowercase)} );
ok(!( "\c[MALAYALAM LETTER VOCALIC RR]" ~~ m/^<+<?isLowercase>>$/ ), q{Don't match related (Ll + OtherLowercase)} );
ok("\c[MALAYALAM LETTER VOCALIC RR]" ~~ m/^<+<-isLowercase>>$/, q{Match related internally inverted (Ll + OtherLowercase)} );
ok("\c[MALAYALAM LETTER VOCALIC RR]" ~~ m/^<-<?isLowercase>>$/, q{Match related externally inverted (Ll + OtherLowercase)} );
ok("\x[0D3D]\c[MALAYALAM LETTER VOCALIC RR]\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/<+<?isLowercase>>/, q{Match unanchored (Ll + OtherLowercase)} );

# Uppercase       # Lu + OtherUppercase


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?isUppercase>>$/, q{Match (Lu + OtherUppercase)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?isUppercase>>$/, q{Match compound (Lu + OtherUppercase)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?isUppercase>>$/ ), q{Don't match externally inverted (Lu + OtherUppercase)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?isUppercase>>$/ ), q{Don't match compound inverted (Lu + OtherUppercase)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-isUppercase>>$/ ), q{Don't match internally inverted (Lu + OtherUppercase)} );
ok(!( "\x[C107]"  ~~ m/^<+<?isUppercase>>$/ ), q{Don't match unrelated (Lu + OtherUppercase)} );
ok("\x[C107]"  ~~ m/^<-<?isUppercase>>$/, q{Match unrelated externally inverted (Lu + OtherUppercase)} );
ok("\x[C107]"  ~~ m/^<+<-isUppercase>>$/, q{Match unrelated internally inverted (Lu + OtherUppercase)} );
ok("\x[C107]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?isUppercase>>/, q{Match unanchored (Lu + OtherUppercase)} );

# Math            # Sm + OtherMath


ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?isMath>>$/, q{Match (Sm + OtherMath)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?isMath>>$/, q{Match compound (Sm + OtherMath)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?isMath>>$/ ), q{Don't match externally inverted (Sm + OtherMath)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?isMath>>$/ ), q{Don't match compound inverted (Sm + OtherMath)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-isMath>>$/ ), q{Don't match internally inverted (Sm + OtherMath)} );
ok(!( "\x[D73F]"  ~~ m/^<+<?isMath>>$/ ), q{Don't match unrelated (Sm + OtherMath)} );
ok("\x[D73F]"  ~~ m/^<-<?isMath>>$/, q{Match unrelated externally inverted (Sm + OtherMath)} );
ok("\x[D73F]"  ~~ m/^<+<-isMath>>$/, q{Match unrelated internally inverted (Sm + OtherMath)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isMath>>$/ ), q{Don't match related (Sm + OtherMath)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isMath>>$/, q{Match related internally inverted (Sm + OtherMath)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isMath>>$/, q{Match related externally inverted (Sm + OtherMath)} );
ok("\x[D73F]\c[COMBINING GRAVE ACCENT]\c[LEFT PARENTHESIS]" ~~ m/<+<?isMath>>/, q{Match unanchored (Sm + OtherMath)} );

# ID_Start        # Lu + Ll + Lt + Lm + Lo + Nl


ok("\x[4E5B]" ~~ m/^<+<?isID_Start>>$/, q{Match (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[4E5B]" ~~ m/^<[A]+<?isID_Start>>$/, q{Match compound (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[4E5B]" ~~ m/^<-<?isID_Start>>$/ ), q{Don't match externally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[4E5B]" ~~ m/^<[A]-<?isID_Start>>$/ ), q{Don't match compound inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[4E5B]" ~~ m/^<+<-isID_Start>>$/ ), q{Don't match internally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?isID_Start>>$/ ), q{Don't match unrelated (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[9FA6]"  ~~ m/^<-<?isID_Start>>$/, q{Match unrelated externally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[9FA6]"  ~~ m/^<+<-isID_Start>>$/, q{Match unrelated internally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[9FA6]\x[4E5B]" ~~ m/<+<?isID_Start>>/, q{Match unanchored (Lu + Ll + Lt + Lm + Lo + Nl)} );

# ID_Continue     # ID_Start + Mn + Mc + Nd + Pc


ok("\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<+<?isID_Continue>>$/, q{Match (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<[A]+<?isID_Continue>>$/, q{Match compound (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<-<?isID_Continue>>$/ ), q{Don't match externally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<[A]-<?isID_Continue>>$/ ), q{Don't match compound inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<+<-isID_Continue>>$/ ), q{Don't match internally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[APL FUNCTIONAL SYMBOL UP TACK JOT]"  ~~ m/^<+<?isID_Continue>>$/ ), q{Don't match unrelated (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[APL FUNCTIONAL SYMBOL UP TACK JOT]"  ~~ m/^<-<?isID_Continue>>$/, q{Match unrelated externally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[APL FUNCTIONAL SYMBOL UP TACK JOT]"  ~~ m/^<+<-isID_Continue>>$/, q{Match unrelated internally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[APL FUNCTIONAL SYMBOL UP TACK JOT]\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/<+<?isID_Continue>>/, q{Match unanchored (ID_Start + Mn + Mc + Nd + Pc)} );

# Any             # Any character


ok("\c[SYRIAC LETTER TAW]" ~~ m/^<+<?isAny>>$/, q{Match (Any character)} );
ok("\c[SYRIAC LETTER TAW]" ~~ m/^<[A]+<?isAny>>$/, q{Match compound (Any character)} );
ok(!( "\c[SYRIAC LETTER TAW]" ~~ m/^<-<?isAny>>$/ ), q{Don't match externally inverted (Any character)} );
ok(!( "\c[SYRIAC LETTER TAW]" ~~ m/^<[A]-<?isAny>>$/ ), q{Don't match compound inverted (Any character)} );
ok(!( "\c[SYRIAC LETTER TAW]" ~~ m/^<+<-isAny>>$/ ), q{Don't match internally inverted (Any character)} );
ok("\c[SYRIAC LETTER TAW]" ~~ m/<+<?isAny>>/, q{Match unanchored (Any character)} );

# Assigned        # Any non-Cn character (i.e. synonym for \P{Cn})


ok("\x[AC00]" ~~ m/^<+<?isAssigned>>$/, q{Match (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AC00]" ~~ m/^<[A]+<?isAssigned>>$/, q{Match compound (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AC00]" ~~ m/^<-<?isAssigned>>$/ ), q{Don't match externally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AC00]" ~~ m/^<[A]-<?isAssigned>>$/ ), q{Don't match compound inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AC00]" ~~ m/^<+<-isAssigned>>$/ ), q{Don't match internally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AB08]"  ~~ m/^<+<?isAssigned>>$/ ), q{Don't match unrelated (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AB08]"  ~~ m/^<-<?isAssigned>>$/, q{Match unrelated externally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AB08]"  ~~ m/^<+<-isAssigned>>$/, q{Match unrelated internally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AB08]\x[AC00]" ~~ m/<+<?isAssigned>>/, q{Match unanchored (Any non-Cn character (i.e. synonym for \P{Cn}))} );

# Unassigned      # Synonym for \p{Cn}


ok("\x[1738]" ~~ m/^<+<?isUnassigned>>$/, q{Match (Synonym for \p{Cn})} );
ok("\x[1738]" ~~ m/^<[A]+<?isUnassigned>>$/, q{Match compound (Synonym for \p{Cn})} );
ok(!( "\x[1738]" ~~ m/^<-<?isUnassigned>>$/ ), q{Don't match externally inverted (Synonym for \p{Cn})} );
ok(!( "\x[1738]" ~~ m/^<[A]-<?isUnassigned>>$/ ), q{Don't match compound inverted (Synonym for \p{Cn})} );
ok(!( "\x[1738]" ~~ m/^<+<-isUnassigned>>$/ ), q{Don't match internally inverted (Synonym for \p{Cn})} );
ok(!( "\c[BUHID LETTER A]"  ~~ m/^<+<?isUnassigned>>$/ ), q{Don't match unrelated (Synonym for \p{Cn})} );
ok("\c[BUHID LETTER A]"  ~~ m/^<-<?isUnassigned>>$/, q{Match unrelated externally inverted (Synonym for \p{Cn})} );
ok("\c[BUHID LETTER A]"  ~~ m/^<+<-isUnassigned>>$/, q{Match unrelated internally inverted (Synonym for \p{Cn})} );
ok("\c[BUHID LETTER A]\x[1738]" ~~ m/<+<?isUnassigned>>/, q{Match unanchored (Synonym for \p{Cn})} );

# Common          # Codepoint not explicitly assigned to a script


ok("\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<+<?isCommon>>$/, q{Match (Codepoint not explicitly assigned to a script)} );
ok("\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]+<?isCommon>>$/, q{Match compound (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<-<?isCommon>>$/ ), q{Don't match externally inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]-<?isCommon>>$/ ), q{Don't match compound inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<+<-isCommon>>$/ ), q{Don't match internally inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[CJK RADICAL REPEAT]"  ~~ m/^<+<?isCommon>>$/ ), q{Don't match unrelated (Codepoint not explicitly assigned to a script)} );
ok("\c[CJK RADICAL REPEAT]"  ~~ m/^<-<?isCommon>>$/, q{Match unrelated externally inverted (Codepoint not explicitly assigned to a script)} );
ok("\c[CJK RADICAL REPEAT]"  ~~ m/^<+<-isCommon>>$/, q{Match unrelated internally inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[ARABIC END OF AYAH]" ~~ m/^<+<?isCommon>>$/ ), q{Don't match related (Codepoint not explicitly assigned to a script)} );
ok("\c[ARABIC END OF AYAH]" ~~ m/^<+<-isCommon>>$/, q{Match related internally inverted (Codepoint not explicitly assigned to a script)} );
ok("\c[ARABIC END OF AYAH]" ~~ m/^<-<?isCommon>>$/, q{Match related externally inverted (Codepoint not explicitly assigned to a script)} );
ok("\c[CJK RADICAL REPEAT]\c[ARABIC END OF AYAH]\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/<+<?isCommon>>/, q{Match unanchored (Codepoint not explicitly assigned to a script)} );

# InAlphabeticPresentationForms


ok(!( "\x[5A81]"  ~~ m/^<+<?isInAlphabeticPresentationForms>>$/ ), q{Don't match unrelated <?isInAlphabeticPresentationForms>} );
ok("\x[5A81]"  ~~ m/^<-<?isInAlphabeticPresentationForms>>$/, q{Match unrelated externally inverted <?isInAlphabeticPresentationForms>} );
ok("\x[5A81]"  ~~ m/^<+<-isInAlphabeticPresentationForms>>$/, q{Match unrelated internally inverted <?isInAlphabeticPresentationForms>} );

# InArabic


ok("\c[ARABIC NUMBER SIGN]" ~~ m/^<+<?isInArabic>>$/, q{Match <?isInArabic>} );
ok("\c[ARABIC NUMBER SIGN]" ~~ m/^<[A]+<?isInArabic>>$/, q{Match compound <?isInArabic>} );
ok(!( "\c[ARABIC NUMBER SIGN]" ~~ m/^<-<?isInArabic>>$/ ), q{Don't match externally inverted <?isInArabic>} );
ok(!( "\c[ARABIC NUMBER SIGN]" ~~ m/^<[A]-<?isInArabic>>$/ ), q{Don't match compound inverted <?isInArabic>} );
ok(!( "\c[ARABIC NUMBER SIGN]" ~~ m/^<+<-isInArabic>>$/ ), q{Don't match internally inverted <?isInArabic>} );
ok(!( "\x[8D16]"  ~~ m/^<+<?isInArabic>>$/ ), q{Don't match unrelated <?isInArabic>} );
ok("\x[8D16]"  ~~ m/^<-<?isInArabic>>$/, q{Match unrelated externally inverted <?isInArabic>} );
ok("\x[8D16]"  ~~ m/^<+<-isInArabic>>$/, q{Match unrelated internally inverted <?isInArabic>} );
ok("\x[8D16]\c[ARABIC NUMBER SIGN]" ~~ m/<+<?isInArabic>>/, q{Match unanchored <?isInArabic>} );

# InArabicPresentationFormsA


ok(!( "\x[C775]"  ~~ m/^<+<?isInArabicPresentationFormsA>>$/ ), q{Don't match unrelated <?isInArabicPresentationFormsA>} );
ok("\x[C775]"  ~~ m/^<-<?isInArabicPresentationFormsA>>$/, q{Match unrelated externally inverted <?isInArabicPresentationFormsA>} );
ok("\x[C775]"  ~~ m/^<+<-isInArabicPresentationFormsA>>$/, q{Match unrelated internally inverted <?isInArabicPresentationFormsA>} );

# InArabicPresentationFormsB


ok(!( "\x[B2EA]"  ~~ m/^<+<?isInArabicPresentationFormsB>>$/ ), q{Don't match unrelated <?isInArabicPresentationFormsB>} );
ok("\x[B2EA]"  ~~ m/^<-<?isInArabicPresentationFormsB>>$/, q{Match unrelated externally inverted <?isInArabicPresentationFormsB>} );
ok("\x[B2EA]"  ~~ m/^<+<-isInArabicPresentationFormsB>>$/, q{Match unrelated internally inverted <?isInArabicPresentationFormsB>} );

# InArmenian


ok("\x[0530]" ~~ m/^<+<?isInArmenian>>$/, q{Match <?isInArmenian>} );
ok("\x[0530]" ~~ m/^<[A]+<?isInArmenian>>$/, q{Match compound <?isInArmenian>} );
ok(!( "\x[0530]" ~~ m/^<-<?isInArmenian>>$/ ), q{Don't match externally inverted <?isInArmenian>} );
ok(!( "\x[0530]" ~~ m/^<[A]-<?isInArmenian>>$/ ), q{Don't match compound inverted <?isInArmenian>} );
ok(!( "\x[0530]" ~~ m/^<+<-isInArmenian>>$/ ), q{Don't match internally inverted <?isInArmenian>} );
ok(!( "\c[ETHIOPIC SYLLABLE QHWAA]"  ~~ m/^<+<?isInArmenian>>$/ ), q{Don't match unrelated <?isInArmenian>} );
ok("\c[ETHIOPIC SYLLABLE QHWAA]"  ~~ m/^<-<?isInArmenian>>$/, q{Match unrelated externally inverted <?isInArmenian>} );
ok("\c[ETHIOPIC SYLLABLE QHWAA]"  ~~ m/^<+<-isInArmenian>>$/, q{Match unrelated internally inverted <?isInArmenian>} );
ok("\c[ETHIOPIC SYLLABLE QHWAA]\x[0530]" ~~ m/<+<?isInArmenian>>/, q{Match unanchored <?isInArmenian>} );

# InArrows


ok("\c[LEFTWARDS ARROW]" ~~ m/^<+<?isInArrows>>$/, q{Match <?isInArrows>} );
ok("\c[LEFTWARDS ARROW]" ~~ m/^<[A]+<?isInArrows>>$/, q{Match compound <?isInArrows>} );
ok(!( "\c[LEFTWARDS ARROW]" ~~ m/^<-<?isInArrows>>$/ ), q{Don't match externally inverted <?isInArrows>} );
ok(!( "\c[LEFTWARDS ARROW]" ~~ m/^<[A]-<?isInArrows>>$/ ), q{Don't match compound inverted <?isInArrows>} );
ok(!( "\c[LEFTWARDS ARROW]" ~~ m/^<+<-isInArrows>>$/ ), q{Don't match internally inverted <?isInArrows>} );
ok(!( "\x[74FA]"  ~~ m/^<+<?isInArrows>>$/ ), q{Don't match unrelated <?isInArrows>} );
ok("\x[74FA]"  ~~ m/^<-<?isInArrows>>$/, q{Match unrelated externally inverted <?isInArrows>} );
ok("\x[74FA]"  ~~ m/^<+<-isInArrows>>$/, q{Match unrelated internally inverted <?isInArrows>} );
ok("\x[74FA]\c[LEFTWARDS ARROW]" ~~ m/<+<?isInArrows>>/, q{Match unanchored <?isInArrows>} );

# InBasicLatin


ok("\c[NULL]" ~~ m/^<+<?isInBasicLatin>>$/, q{Match <?isInBasicLatin>} );
ok("\c[NULL]" ~~ m/^<[A]+<?isInBasicLatin>>$/, q{Match compound <?isInBasicLatin>} );
ok(!( "\c[NULL]" ~~ m/^<-<?isInBasicLatin>>$/ ), q{Don't match externally inverted <?isInBasicLatin>} );
ok(!( "\c[NULL]" ~~ m/^<[A]-<?isInBasicLatin>>$/ ), q{Don't match compound inverted <?isInBasicLatin>} );
ok(!( "\c[NULL]" ~~ m/^<+<-isInBasicLatin>>$/ ), q{Don't match internally inverted <?isInBasicLatin>} );
ok(!( "\x[ADFF]"  ~~ m/^<+<?isInBasicLatin>>$/ ), q{Don't match unrelated <?isInBasicLatin>} );
ok("\x[ADFF]"  ~~ m/^<-<?isInBasicLatin>>$/, q{Match unrelated externally inverted <?isInBasicLatin>} );
ok("\x[ADFF]"  ~~ m/^<+<-isInBasicLatin>>$/, q{Match unrelated internally inverted <?isInBasicLatin>} );
ok("\x[ADFF]\c[NULL]" ~~ m/<+<?isInBasicLatin>>/, q{Match unanchored <?isInBasicLatin>} );

# InBengali


ok("\x[0980]" ~~ m/^<+<?isInBengali>>$/, q{Match <?isInBengali>} );
ok("\x[0980]" ~~ m/^<[A]+<?isInBengali>>$/, q{Match compound <?isInBengali>} );
ok(!( "\x[0980]" ~~ m/^<-<?isInBengali>>$/ ), q{Don't match externally inverted <?isInBengali>} );
ok(!( "\x[0980]" ~~ m/^<[A]-<?isInBengali>>$/ ), q{Don't match compound inverted <?isInBengali>} );
ok(!( "\x[0980]" ~~ m/^<+<-isInBengali>>$/ ), q{Don't match internally inverted <?isInBengali>} );
ok(!( "\x[3409]"  ~~ m/^<+<?isInBengali>>$/ ), q{Don't match unrelated <?isInBengali>} );
ok("\x[3409]"  ~~ m/^<-<?isInBengali>>$/, q{Match unrelated externally inverted <?isInBengali>} );
ok("\x[3409]"  ~~ m/^<+<-isInBengali>>$/, q{Match unrelated internally inverted <?isInBengali>} );
ok("\x[3409]\x[0980]" ~~ m/<+<?isInBengali>>/, q{Match unanchored <?isInBengali>} );

# InBlockElements


ok("\c[UPPER HALF BLOCK]" ~~ m/^<+<?isInBlockElements>>$/, q{Match <?isInBlockElements>} );
ok("\c[UPPER HALF BLOCK]" ~~ m/^<[A]+<?isInBlockElements>>$/, q{Match compound <?isInBlockElements>} );
ok(!( "\c[UPPER HALF BLOCK]" ~~ m/^<-<?isInBlockElements>>$/ ), q{Don't match externally inverted <?isInBlockElements>} );
ok(!( "\c[UPPER HALF BLOCK]" ~~ m/^<[A]-<?isInBlockElements>>$/ ), q{Don't match compound inverted <?isInBlockElements>} );
ok(!( "\c[UPPER HALF BLOCK]" ~~ m/^<+<-isInBlockElements>>$/ ), q{Don't match internally inverted <?isInBlockElements>} );
ok(!( "\x[77B1]"  ~~ m/^<+<?isInBlockElements>>$/ ), q{Don't match unrelated <?isInBlockElements>} );
ok("\x[77B1]"  ~~ m/^<-<?isInBlockElements>>$/, q{Match unrelated externally inverted <?isInBlockElements>} );
ok("\x[77B1]"  ~~ m/^<+<-isInBlockElements>>$/, q{Match unrelated internally inverted <?isInBlockElements>} );
ok("\x[77B1]\c[UPPER HALF BLOCK]" ~~ m/<+<?isInBlockElements>>/, q{Match unanchored <?isInBlockElements>} );

# InBopomofo


ok("\x[3100]" ~~ m/^<+<?isInBopomofo>>$/, q{Match <?isInBopomofo>} );
ok("\x[3100]" ~~ m/^<[A]+<?isInBopomofo>>$/, q{Match compound <?isInBopomofo>} );
ok(!( "\x[3100]" ~~ m/^<-<?isInBopomofo>>$/ ), q{Don't match externally inverted <?isInBopomofo>} );
ok(!( "\x[3100]" ~~ m/^<[A]-<?isInBopomofo>>$/ ), q{Don't match compound inverted <?isInBopomofo>} );
ok(!( "\x[3100]" ~~ m/^<+<-isInBopomofo>>$/ ), q{Don't match internally inverted <?isInBopomofo>} );
ok(!( "\x[701E]"  ~~ m/^<+<?isInBopomofo>>$/ ), q{Don't match unrelated <?isInBopomofo>} );
ok("\x[701E]"  ~~ m/^<-<?isInBopomofo>>$/, q{Match unrelated externally inverted <?isInBopomofo>} );
ok("\x[701E]"  ~~ m/^<+<-isInBopomofo>>$/, q{Match unrelated internally inverted <?isInBopomofo>} );
ok("\x[701E]\x[3100]" ~~ m/<+<?isInBopomofo>>/, q{Match unanchored <?isInBopomofo>} );

# InBopomofoExtended


ok("\c[BOPOMOFO LETTER BU]" ~~ m/^<+<?isInBopomofoExtended>>$/, q{Match <?isInBopomofoExtended>} );
ok("\c[BOPOMOFO LETTER BU]" ~~ m/^<[A]+<?isInBopomofoExtended>>$/, q{Match compound <?isInBopomofoExtended>} );
ok(!( "\c[BOPOMOFO LETTER BU]" ~~ m/^<-<?isInBopomofoExtended>>$/ ), q{Don't match externally inverted <?isInBopomofoExtended>} );
ok(!( "\c[BOPOMOFO LETTER BU]" ~~ m/^<[A]-<?isInBopomofoExtended>>$/ ), q{Don't match compound inverted <?isInBopomofoExtended>} );
ok(!( "\c[BOPOMOFO LETTER BU]" ~~ m/^<+<-isInBopomofoExtended>>$/ ), q{Don't match internally inverted <?isInBopomofoExtended>} );
ok(!( "\c[YI SYLLABLE TIE]"  ~~ m/^<+<?isInBopomofoExtended>>$/ ), q{Don't match unrelated <?isInBopomofoExtended>} );
ok("\c[YI SYLLABLE TIE]"  ~~ m/^<-<?isInBopomofoExtended>>$/, q{Match unrelated externally inverted <?isInBopomofoExtended>} );
ok("\c[YI SYLLABLE TIE]"  ~~ m/^<+<-isInBopomofoExtended>>$/, q{Match unrelated internally inverted <?isInBopomofoExtended>} );
ok("\c[YI SYLLABLE TIE]\c[BOPOMOFO LETTER BU]" ~~ m/<+<?isInBopomofoExtended>>/, q{Match unanchored <?isInBopomofoExtended>} );

# InBoxDrawing


ok("\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<+<?isInBoxDrawing>>$/, q{Match <?isInBoxDrawing>} );
ok("\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<[A]+<?isInBoxDrawing>>$/, q{Match compound <?isInBoxDrawing>} );
ok(!( "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<-<?isInBoxDrawing>>$/ ), q{Don't match externally inverted <?isInBoxDrawing>} );
ok(!( "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<[A]-<?isInBoxDrawing>>$/ ), q{Don't match compound inverted <?isInBoxDrawing>} );
ok(!( "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<+<-isInBoxDrawing>>$/ ), q{Don't match internally inverted <?isInBoxDrawing>} );
ok(!( "\x[D2FB]"  ~~ m/^<+<?isInBoxDrawing>>$/ ), q{Don't match unrelated <?isInBoxDrawing>} );
ok("\x[D2FB]"  ~~ m/^<-<?isInBoxDrawing>>$/, q{Match unrelated externally inverted <?isInBoxDrawing>} );
ok("\x[D2FB]"  ~~ m/^<+<-isInBoxDrawing>>$/, q{Match unrelated internally inverted <?isInBoxDrawing>} );
ok("\x[D2FB]\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/<+<?isInBoxDrawing>>/, q{Match unanchored <?isInBoxDrawing>} );

# InBraillePatterns


ok("\c[BRAILLE PATTERN BLANK]" ~~ m/^<+<?isInBraillePatterns>>$/, q{Match <?isInBraillePatterns>} );
ok("\c[BRAILLE PATTERN BLANK]" ~~ m/^<[A]+<?isInBraillePatterns>>$/, q{Match compound <?isInBraillePatterns>} );
ok(!( "\c[BRAILLE PATTERN BLANK]" ~~ m/^<-<?isInBraillePatterns>>$/ ), q{Don't match externally inverted <?isInBraillePatterns>} );
ok(!( "\c[BRAILLE PATTERN BLANK]" ~~ m/^<[A]-<?isInBraillePatterns>>$/ ), q{Don't match compound inverted <?isInBraillePatterns>} );
ok(!( "\c[BRAILLE PATTERN BLANK]" ~~ m/^<+<-isInBraillePatterns>>$/ ), q{Don't match internally inverted <?isInBraillePatterns>} );
ok(!( "\x[4FE4]"  ~~ m/^<+<?isInBraillePatterns>>$/ ), q{Don't match unrelated <?isInBraillePatterns>} );
ok("\x[4FE4]"  ~~ m/^<-<?isInBraillePatterns>>$/, q{Match unrelated externally inverted <?isInBraillePatterns>} );
ok("\x[4FE4]"  ~~ m/^<+<-isInBraillePatterns>>$/, q{Match unrelated internally inverted <?isInBraillePatterns>} );
ok("\x[4FE4]\c[BRAILLE PATTERN BLANK]" ~~ m/<+<?isInBraillePatterns>>/, q{Match unanchored <?isInBraillePatterns>} );

# InBuhid


ok("\c[BUHID LETTER A]" ~~ m/^<+<?isInBuhid>>$/, q{Match <?isInBuhid>} );
ok("\c[BUHID LETTER A]" ~~ m/^<[A]+<?isInBuhid>>$/, q{Match compound <?isInBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<-<?isInBuhid>>$/ ), q{Don't match externally inverted <?isInBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<[A]-<?isInBuhid>>$/ ), q{Don't match compound inverted <?isInBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<+<-isInBuhid>>$/ ), q{Don't match internally inverted <?isInBuhid>} );
ok(!( "\x[996F]"  ~~ m/^<+<?isInBuhid>>$/ ), q{Don't match unrelated <?isInBuhid>} );
ok("\x[996F]"  ~~ m/^<-<?isInBuhid>>$/, q{Match unrelated externally inverted <?isInBuhid>} );
ok("\x[996F]"  ~~ m/^<+<-isInBuhid>>$/, q{Match unrelated internally inverted <?isInBuhid>} );
ok("\x[996F]\c[BUHID LETTER A]" ~~ m/<+<?isInBuhid>>/, q{Match unanchored <?isInBuhid>} );

# InByzantineMusicalSymbols


ok(!( "\x[802A]"  ~~ m/^<+<?isInByzantineMusicalSymbols>>$/ ), q{Don't match unrelated <?isInByzantineMusicalSymbols>} );
ok("\x[802A]"  ~~ m/^<-<?isInByzantineMusicalSymbols>>$/, q{Match unrelated externally inverted <?isInByzantineMusicalSymbols>} );
ok("\x[802A]"  ~~ m/^<+<-isInByzantineMusicalSymbols>>$/, q{Match unrelated internally inverted <?isInByzantineMusicalSymbols>} );

# InCJKCompatibility


ok("\c[SQUARE APAATO]" ~~ m/^<+<?isInCJKCompatibility>>$/, q{Match <?isInCJKCompatibility>} );
ok("\c[SQUARE APAATO]" ~~ m/^<[A]+<?isInCJKCompatibility>>$/, q{Match compound <?isInCJKCompatibility>} );
ok(!( "\c[SQUARE APAATO]" ~~ m/^<-<?isInCJKCompatibility>>$/ ), q{Don't match externally inverted <?isInCJKCompatibility>} );
ok(!( "\c[SQUARE APAATO]" ~~ m/^<[A]-<?isInCJKCompatibility>>$/ ), q{Don't match compound inverted <?isInCJKCompatibility>} );
ok(!( "\c[SQUARE APAATO]" ~~ m/^<+<-isInCJKCompatibility>>$/ ), q{Don't match internally inverted <?isInCJKCompatibility>} );
ok(!( "\x[2B99]"  ~~ m/^<+<?isInCJKCompatibility>>$/ ), q{Don't match unrelated <?isInCJKCompatibility>} );
ok("\x[2B99]"  ~~ m/^<-<?isInCJKCompatibility>>$/, q{Match unrelated externally inverted <?isInCJKCompatibility>} );
ok("\x[2B99]"  ~~ m/^<+<-isInCJKCompatibility>>$/, q{Match unrelated internally inverted <?isInCJKCompatibility>} );
ok("\x[2B99]\c[SQUARE APAATO]" ~~ m/<+<?isInCJKCompatibility>>/, q{Match unanchored <?isInCJKCompatibility>} );

# InCJKCompatibilityForms


ok(!( "\x[342B]"  ~~ m/^<+<?isInCJKCompatibilityForms>>$/ ), q{Don't match unrelated <?isInCJKCompatibilityForms>} );
ok("\x[342B]"  ~~ m/^<-<?isInCJKCompatibilityForms>>$/, q{Match unrelated externally inverted <?isInCJKCompatibilityForms>} );
ok("\x[342B]"  ~~ m/^<+<-isInCJKCompatibilityForms>>$/, q{Match unrelated internally inverted <?isInCJKCompatibilityForms>} );

# InCJKCompatibilityIdeographs


ok(!( "\c[BLACK SQUARE]"  ~~ m/^<+<?isInCJKCompatibilityIdeographs>>$/ ), q{Don't match unrelated <?isInCJKCompatibilityIdeographs>} );
ok("\c[BLACK SQUARE]"  ~~ m/^<-<?isInCJKCompatibilityIdeographs>>$/, q{Match unrelated externally inverted <?isInCJKCompatibilityIdeographs>} );
ok("\c[BLACK SQUARE]"  ~~ m/^<+<-isInCJKCompatibilityIdeographs>>$/, q{Match unrelated internally inverted <?isInCJKCompatibilityIdeographs>} );

# InCJKCompatibilityIdeographsSupplement


ok(!( "\x[A90E]"  ~~ m/^<+<?isInCJKCompatibilityIdeographsSupplement>>$/ ), q{Don't match unrelated <?isInCJKCompatibilityIdeographsSupplement>} );
ok("\x[A90E]"  ~~ m/^<-<?isInCJKCompatibilityIdeographsSupplement>>$/, q{Match unrelated externally inverted <?isInCJKCompatibilityIdeographsSupplement>} );
ok("\x[A90E]"  ~~ m/^<+<-isInCJKCompatibilityIdeographsSupplement>>$/, q{Match unrelated internally inverted <?isInCJKCompatibilityIdeographsSupplement>} );

# InCJKRadicalsSupplement


ok("\c[CJK RADICAL REPEAT]" ~~ m/^<+<?isInCJKRadicalsSupplement>>$/, q{Match <?isInCJKRadicalsSupplement>} );
ok("\c[CJK RADICAL REPEAT]" ~~ m/^<[A]+<?isInCJKRadicalsSupplement>>$/, q{Match compound <?isInCJKRadicalsSupplement>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<-<?isInCJKRadicalsSupplement>>$/ ), q{Don't match externally inverted <?isInCJKRadicalsSupplement>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<[A]-<?isInCJKRadicalsSupplement>>$/ ), q{Don't match compound inverted <?isInCJKRadicalsSupplement>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<+<-isInCJKRadicalsSupplement>>$/ ), q{Don't match internally inverted <?isInCJKRadicalsSupplement>} );
ok(!( "\x[563B]"  ~~ m/^<+<?isInCJKRadicalsSupplement>>$/ ), q{Don't match unrelated <?isInCJKRadicalsSupplement>} );
ok("\x[563B]"  ~~ m/^<-<?isInCJKRadicalsSupplement>>$/, q{Match unrelated externally inverted <?isInCJKRadicalsSupplement>} );
ok("\x[563B]"  ~~ m/^<+<-isInCJKRadicalsSupplement>>$/, q{Match unrelated internally inverted <?isInCJKRadicalsSupplement>} );
ok("\x[563B]\c[CJK RADICAL REPEAT]" ~~ m/<+<?isInCJKRadicalsSupplement>>/, q{Match unanchored <?isInCJKRadicalsSupplement>} );

# InCJKSymbolsAndPunctuation


ok("\c[IDEOGRAPHIC SPACE]" ~~ m/^<+<?isInCJKSymbolsAndPunctuation>>$/, q{Match <?isInCJKSymbolsAndPunctuation>} );
ok("\c[IDEOGRAPHIC SPACE]" ~~ m/^<[A]+<?isInCJKSymbolsAndPunctuation>>$/, q{Match compound <?isInCJKSymbolsAndPunctuation>} );
ok(!( "\c[IDEOGRAPHIC SPACE]" ~~ m/^<-<?isInCJKSymbolsAndPunctuation>>$/ ), q{Don't match externally inverted <?isInCJKSymbolsAndPunctuation>} );
ok(!( "\c[IDEOGRAPHIC SPACE]" ~~ m/^<[A]-<?isInCJKSymbolsAndPunctuation>>$/ ), q{Don't match compound inverted <?isInCJKSymbolsAndPunctuation>} );
ok(!( "\c[IDEOGRAPHIC SPACE]" ~~ m/^<+<-isInCJKSymbolsAndPunctuation>>$/ ), q{Don't match internally inverted <?isInCJKSymbolsAndPunctuation>} );
ok(!( "\x[3BE6]"  ~~ m/^<+<?isInCJKSymbolsAndPunctuation>>$/ ), q{Don't match unrelated <?isInCJKSymbolsAndPunctuation>} );
ok("\x[3BE6]"  ~~ m/^<-<?isInCJKSymbolsAndPunctuation>>$/, q{Match unrelated externally inverted <?isInCJKSymbolsAndPunctuation>} );
ok("\x[3BE6]"  ~~ m/^<+<-isInCJKSymbolsAndPunctuation>>$/, q{Match unrelated internally inverted <?isInCJKSymbolsAndPunctuation>} );
ok("\x[3BE6]\c[IDEOGRAPHIC SPACE]" ~~ m/<+<?isInCJKSymbolsAndPunctuation>>/, q{Match unanchored <?isInCJKSymbolsAndPunctuation>} );

# InCJKUnifiedIdeographs


ok("\x[4E00]" ~~ m/^<+<?isInCJKUnifiedIdeographs>>$/, q{Match <?isInCJKUnifiedIdeographs>} );
ok("\x[4E00]" ~~ m/^<[A]+<?isInCJKUnifiedIdeographs>>$/, q{Match compound <?isInCJKUnifiedIdeographs>} );
ok(!( "\x[4E00]" ~~ m/^<-<?isInCJKUnifiedIdeographs>>$/ ), q{Don't match externally inverted <?isInCJKUnifiedIdeographs>} );
ok(!( "\x[4E00]" ~~ m/^<[A]-<?isInCJKUnifiedIdeographs>>$/ ), q{Don't match compound inverted <?isInCJKUnifiedIdeographs>} );
ok(!( "\x[4E00]" ~~ m/^<+<-isInCJKUnifiedIdeographs>>$/ ), q{Don't match internally inverted <?isInCJKUnifiedIdeographs>} );
ok(!( "\x[436E]"  ~~ m/^<+<?isInCJKUnifiedIdeographs>>$/ ), q{Don't match unrelated <?isInCJKUnifiedIdeographs>} );
ok("\x[436E]"  ~~ m/^<-<?isInCJKUnifiedIdeographs>>$/, q{Match unrelated externally inverted <?isInCJKUnifiedIdeographs>} );
ok("\x[436E]"  ~~ m/^<+<-isInCJKUnifiedIdeographs>>$/, q{Match unrelated internally inverted <?isInCJKUnifiedIdeographs>} );
ok("\x[436E]\x[4E00]" ~~ m/<+<?isInCJKUnifiedIdeographs>>/, q{Match unanchored <?isInCJKUnifiedIdeographs>} );

# InCJKUnifiedIdeographsExtensionA


ok("\x[4993]" ~~ m/^<+<?isInCJKUnifiedIdeographsExtensionA>>$/, q{Match <?isInCJKUnifiedIdeographsExtensionA>} );
ok("\x[4993]" ~~ m/^<[A]+<?isInCJKUnifiedIdeographsExtensionA>>$/, q{Match compound <?isInCJKUnifiedIdeographsExtensionA>} );
ok(!( "\x[4993]" ~~ m/^<-<?isInCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match externally inverted <?isInCJKUnifiedIdeographsExtensionA>} );
ok(!( "\x[4993]" ~~ m/^<[A]-<?isInCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match compound inverted <?isInCJKUnifiedIdeographsExtensionA>} );
ok(!( "\x[4993]" ~~ m/^<+<-isInCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match internally inverted <?isInCJKUnifiedIdeographsExtensionA>} );
ok(!( "\c[HEXAGRAM FOR THE CREATIVE HEAVEN]"  ~~ m/^<+<?isInCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match unrelated <?isInCJKUnifiedIdeographsExtensionA>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]"  ~~ m/^<-<?isInCJKUnifiedIdeographsExtensionA>>$/, q{Match unrelated externally inverted <?isInCJKUnifiedIdeographsExtensionA>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]"  ~~ m/^<+<-isInCJKUnifiedIdeographsExtensionA>>$/, q{Match unrelated internally inverted <?isInCJKUnifiedIdeographsExtensionA>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]\x[4993]" ~~ m/<+<?isInCJKUnifiedIdeographsExtensionA>>/, q{Match unanchored <?isInCJKUnifiedIdeographsExtensionA>} );

# InCJKUnifiedIdeographsExtensionB


ok(!( "\x[3E5A]"  ~~ m/^<+<?isInCJKUnifiedIdeographsExtensionB>>$/ ), q{Don't match unrelated <?isInCJKUnifiedIdeographsExtensionB>} );
ok("\x[3E5A]"  ~~ m/^<-<?isInCJKUnifiedIdeographsExtensionB>>$/, q{Match unrelated externally inverted <?isInCJKUnifiedIdeographsExtensionB>} );
ok("\x[3E5A]"  ~~ m/^<+<-isInCJKUnifiedIdeographsExtensionB>>$/, q{Match unrelated internally inverted <?isInCJKUnifiedIdeographsExtensionB>} );

# InCherokee


ok("\c[CHEROKEE LETTER A]" ~~ m/^<+<?isInCherokee>>$/, q{Match <?isInCherokee>} );
ok("\c[CHEROKEE LETTER A]" ~~ m/^<[A]+<?isInCherokee>>$/, q{Match compound <?isInCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<-<?isInCherokee>>$/ ), q{Don't match externally inverted <?isInCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<[A]-<?isInCherokee>>$/ ), q{Don't match compound inverted <?isInCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<+<-isInCherokee>>$/ ), q{Don't match internally inverted <?isInCherokee>} );
ok(!( "\x[B311]"  ~~ m/^<+<?isInCherokee>>$/ ), q{Don't match unrelated <?isInCherokee>} );
ok("\x[B311]"  ~~ m/^<-<?isInCherokee>>$/, q{Match unrelated externally inverted <?isInCherokee>} );
ok("\x[B311]"  ~~ m/^<+<-isInCherokee>>$/, q{Match unrelated internally inverted <?isInCherokee>} );
ok("\x[B311]\c[CHEROKEE LETTER A]" ~~ m/<+<?isInCherokee>>/, q{Match unanchored <?isInCherokee>} );

# InCombiningDiacriticalMarks


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?isInCombiningDiacriticalMarks>>$/, q{Match <?isInCombiningDiacriticalMarks>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?isInCombiningDiacriticalMarks>>$/, q{Match compound <?isInCombiningDiacriticalMarks>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?isInCombiningDiacriticalMarks>>$/ ), q{Don't match externally inverted <?isInCombiningDiacriticalMarks>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?isInCombiningDiacriticalMarks>>$/ ), q{Don't match compound inverted <?isInCombiningDiacriticalMarks>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-isInCombiningDiacriticalMarks>>$/ ), q{Don't match internally inverted <?isInCombiningDiacriticalMarks>} );
ok(!( "\x[81B8]"  ~~ m/^<+<?isInCombiningDiacriticalMarks>>$/ ), q{Don't match unrelated <?isInCombiningDiacriticalMarks>} );
ok("\x[81B8]"  ~~ m/^<-<?isInCombiningDiacriticalMarks>>$/, q{Match unrelated externally inverted <?isInCombiningDiacriticalMarks>} );
ok("\x[81B8]"  ~~ m/^<+<-isInCombiningDiacriticalMarks>>$/, q{Match unrelated internally inverted <?isInCombiningDiacriticalMarks>} );
ok("\x[81B8]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?isInCombiningDiacriticalMarks>>/, q{Match unanchored <?isInCombiningDiacriticalMarks>} );

# InCombiningDiacriticalMarksforSymbols


ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<?isInCombiningDiacriticalMarksforSymbols>>$/, q{Match <?isInCombiningDiacriticalMarksforSymbols>} );
ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<[A]+<?isInCombiningDiacriticalMarksforSymbols>>$/, q{Match compound <?isInCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<-<?isInCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match externally inverted <?isInCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<[A]-<?isInCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match compound inverted <?isInCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<-isInCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match internally inverted <?isInCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[YI SYLLABLE NZOX]"  ~~ m/^<+<?isInCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match unrelated <?isInCombiningDiacriticalMarksforSymbols>} );
ok("\c[YI SYLLABLE NZOX]"  ~~ m/^<-<?isInCombiningDiacriticalMarksforSymbols>>$/, q{Match unrelated externally inverted <?isInCombiningDiacriticalMarksforSymbols>} );
ok("\c[YI SYLLABLE NZOX]"  ~~ m/^<+<-isInCombiningDiacriticalMarksforSymbols>>$/, q{Match unrelated internally inverted <?isInCombiningDiacriticalMarksforSymbols>} );
ok("\c[YI SYLLABLE NZOX]\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/<+<?isInCombiningDiacriticalMarksforSymbols>>/, q{Match unanchored <?isInCombiningDiacriticalMarksforSymbols>} );

# InCombiningHalfMarks


ok(!( "\x[7140]"  ~~ m/^<+<?isInCombiningHalfMarks>>$/ ), q{Don't match unrelated <?isInCombiningHalfMarks>} );
ok("\x[7140]"  ~~ m/^<-<?isInCombiningHalfMarks>>$/, q{Match unrelated externally inverted <?isInCombiningHalfMarks>} );
ok("\x[7140]"  ~~ m/^<+<-isInCombiningHalfMarks>>$/, q{Match unrelated internally inverted <?isInCombiningHalfMarks>} );

# InControlPictures


ok("\c[SYMBOL FOR NULL]" ~~ m/^<+<?isInControlPictures>>$/, q{Match <?isInControlPictures>} );
ok("\c[SYMBOL FOR NULL]" ~~ m/^<[A]+<?isInControlPictures>>$/, q{Match compound <?isInControlPictures>} );
ok(!( "\c[SYMBOL FOR NULL]" ~~ m/^<-<?isInControlPictures>>$/ ), q{Don't match externally inverted <?isInControlPictures>} );
ok(!( "\c[SYMBOL FOR NULL]" ~~ m/^<[A]-<?isInControlPictures>>$/ ), q{Don't match compound inverted <?isInControlPictures>} );
ok(!( "\c[SYMBOL FOR NULL]" ~~ m/^<+<-isInControlPictures>>$/ ), q{Don't match internally inverted <?isInControlPictures>} );
ok(!( "\x[CBBF]"  ~~ m/^<+<?isInControlPictures>>$/ ), q{Don't match unrelated <?isInControlPictures>} );
ok("\x[CBBF]"  ~~ m/^<-<?isInControlPictures>>$/, q{Match unrelated externally inverted <?isInControlPictures>} );
ok("\x[CBBF]"  ~~ m/^<+<-isInControlPictures>>$/, q{Match unrelated internally inverted <?isInControlPictures>} );
ok("\x[CBBF]\c[SYMBOL FOR NULL]" ~~ m/<+<?isInControlPictures>>/, q{Match unanchored <?isInControlPictures>} );

# InCurrencySymbols


ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<+<?isInCurrencySymbols>>$/, q{Match <?isInCurrencySymbols>} );
ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]+<?isInCurrencySymbols>>$/, q{Match compound <?isInCurrencySymbols>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<-<?isInCurrencySymbols>>$/ ), q{Don't match externally inverted <?isInCurrencySymbols>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]-<?isInCurrencySymbols>>$/ ), q{Don't match compound inverted <?isInCurrencySymbols>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<+<-isInCurrencySymbols>>$/ ), q{Don't match internally inverted <?isInCurrencySymbols>} );
ok(!( "\x[D040]"  ~~ m/^<+<?isInCurrencySymbols>>$/ ), q{Don't match unrelated <?isInCurrencySymbols>} );
ok("\x[D040]"  ~~ m/^<-<?isInCurrencySymbols>>$/, q{Match unrelated externally inverted <?isInCurrencySymbols>} );
ok("\x[D040]"  ~~ m/^<+<-isInCurrencySymbols>>$/, q{Match unrelated internally inverted <?isInCurrencySymbols>} );
ok("\x[D040]\c[EURO-CURRENCY SIGN]" ~~ m/<+<?isInCurrencySymbols>>/, q{Match unanchored <?isInCurrencySymbols>} );

# InCyrillic


ok("\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<+<?isInCyrillic>>$/, q{Match <?isInCyrillic>} );
ok("\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<[A]+<?isInCyrillic>>$/, q{Match compound <?isInCyrillic>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<-<?isInCyrillic>>$/ ), q{Don't match externally inverted <?isInCyrillic>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<[A]-<?isInCyrillic>>$/ ), q{Don't match compound inverted <?isInCyrillic>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<+<-isInCyrillic>>$/ ), q{Don't match internally inverted <?isInCyrillic>} );
ok(!( "\x[9C58]"  ~~ m/^<+<?isInCyrillic>>$/ ), q{Don't match unrelated <?isInCyrillic>} );
ok("\x[9C58]"  ~~ m/^<-<?isInCyrillic>>$/, q{Match unrelated externally inverted <?isInCyrillic>} );
ok("\x[9C58]"  ~~ m/^<+<-isInCyrillic>>$/, q{Match unrelated internally inverted <?isInCyrillic>} );
ok("\x[9C58]\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/<+<?isInCyrillic>>/, q{Match unanchored <?isInCyrillic>} );

# InCyrillicSupplementary


ok("\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<+<?isInCyrillicSupplementary>>$/, q{Match <?isInCyrillicSupplementary>} );
ok("\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<[A]+<?isInCyrillicSupplementary>>$/, q{Match compound <?isInCyrillicSupplementary>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<-<?isInCyrillicSupplementary>>$/ ), q{Don't match externally inverted <?isInCyrillicSupplementary>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<[A]-<?isInCyrillicSupplementary>>$/ ), q{Don't match compound inverted <?isInCyrillicSupplementary>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<+<-isInCyrillicSupplementary>>$/ ), q{Don't match internally inverted <?isInCyrillicSupplementary>} );
ok(!( "\x[857A]"  ~~ m/^<+<?isInCyrillicSupplementary>>$/ ), q{Don't match unrelated <?isInCyrillicSupplementary>} );
ok("\x[857A]"  ~~ m/^<-<?isInCyrillicSupplementary>>$/, q{Match unrelated externally inverted <?isInCyrillicSupplementary>} );
ok("\x[857A]"  ~~ m/^<+<-isInCyrillicSupplementary>>$/, q{Match unrelated internally inverted <?isInCyrillicSupplementary>} );
ok("\x[857A]\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/<+<?isInCyrillicSupplementary>>/, q{Match unanchored <?isInCyrillicSupplementary>} );

# InDeseret


ok(!( "\c[LATIN SMALL LETTER I WITH DOUBLE GRAVE]"  ~~ m/^<+<?isInDeseret>>$/ ), q{Don't match unrelated <?isInDeseret>} );
ok("\c[LATIN SMALL LETTER I WITH DOUBLE GRAVE]"  ~~ m/^<-<?isInDeseret>>$/, q{Match unrelated externally inverted <?isInDeseret>} );
ok("\c[LATIN SMALL LETTER I WITH DOUBLE GRAVE]"  ~~ m/^<+<-isInDeseret>>$/, q{Match unrelated internally inverted <?isInDeseret>} );

# InDevanagari


ok("\x[0900]" ~~ m/^<+<?isInDevanagari>>$/, q{Match <?isInDevanagari>} );
ok("\x[0900]" ~~ m/^<[A]+<?isInDevanagari>>$/, q{Match compound <?isInDevanagari>} );
ok(!( "\x[0900]" ~~ m/^<-<?isInDevanagari>>$/ ), q{Don't match externally inverted <?isInDevanagari>} );
ok(!( "\x[0900]" ~~ m/^<[A]-<?isInDevanagari>>$/ ), q{Don't match compound inverted <?isInDevanagari>} );
ok(!( "\x[0900]" ~~ m/^<+<-isInDevanagari>>$/ ), q{Don't match internally inverted <?isInDevanagari>} );
ok(!( "\x[3837]"  ~~ m/^<+<?isInDevanagari>>$/ ), q{Don't match unrelated <?isInDevanagari>} );
ok("\x[3837]"  ~~ m/^<-<?isInDevanagari>>$/, q{Match unrelated externally inverted <?isInDevanagari>} );
ok("\x[3837]"  ~~ m/^<+<-isInDevanagari>>$/, q{Match unrelated internally inverted <?isInDevanagari>} );
ok("\x[3837]\x[0900]" ~~ m/<+<?isInDevanagari>>/, q{Match unanchored <?isInDevanagari>} );

# InDingbats


ok("\x[2700]" ~~ m/^<+<?isInDingbats>>$/, q{Match <?isInDingbats>} );
ok("\x[2700]" ~~ m/^<[A]+<?isInDingbats>>$/, q{Match compound <?isInDingbats>} );
ok(!( "\x[2700]" ~~ m/^<-<?isInDingbats>>$/ ), q{Don't match externally inverted <?isInDingbats>} );
ok(!( "\x[2700]" ~~ m/^<[A]-<?isInDingbats>>$/ ), q{Don't match compound inverted <?isInDingbats>} );
ok(!( "\x[2700]" ~~ m/^<+<-isInDingbats>>$/ ), q{Don't match internally inverted <?isInDingbats>} );
ok(!( "\x[C9CC]"  ~~ m/^<+<?isInDingbats>>$/ ), q{Don't match unrelated <?isInDingbats>} );
ok("\x[C9CC]"  ~~ m/^<-<?isInDingbats>>$/, q{Match unrelated externally inverted <?isInDingbats>} );
ok("\x[C9CC]"  ~~ m/^<+<-isInDingbats>>$/, q{Match unrelated internally inverted <?isInDingbats>} );
ok("\x[C9CC]\x[2700]" ~~ m/<+<?isInDingbats>>/, q{Match unanchored <?isInDingbats>} );

# InEnclosedAlphanumerics


ok("\c[CIRCLED DIGIT ONE]" ~~ m/^<+<?isInEnclosedAlphanumerics>>$/, q{Match <?isInEnclosedAlphanumerics>} );
ok("\c[CIRCLED DIGIT ONE]" ~~ m/^<[A]+<?isInEnclosedAlphanumerics>>$/, q{Match compound <?isInEnclosedAlphanumerics>} );
ok(!( "\c[CIRCLED DIGIT ONE]" ~~ m/^<-<?isInEnclosedAlphanumerics>>$/ ), q{Don't match externally inverted <?isInEnclosedAlphanumerics>} );
ok(!( "\c[CIRCLED DIGIT ONE]" ~~ m/^<[A]-<?isInEnclosedAlphanumerics>>$/ ), q{Don't match compound inverted <?isInEnclosedAlphanumerics>} );
ok(!( "\c[CIRCLED DIGIT ONE]" ~~ m/^<+<-isInEnclosedAlphanumerics>>$/ ), q{Don't match internally inverted <?isInEnclosedAlphanumerics>} );
ok(!( "\x[CCB8]"  ~~ m/^<+<?isInEnclosedAlphanumerics>>$/ ), q{Don't match unrelated <?isInEnclosedAlphanumerics>} );
ok("\x[CCB8]"  ~~ m/^<-<?isInEnclosedAlphanumerics>>$/, q{Match unrelated externally inverted <?isInEnclosedAlphanumerics>} );
ok("\x[CCB8]"  ~~ m/^<+<-isInEnclosedAlphanumerics>>$/, q{Match unrelated internally inverted <?isInEnclosedAlphanumerics>} );
ok("\x[CCB8]\c[CIRCLED DIGIT ONE]" ~~ m/<+<?isInEnclosedAlphanumerics>>/, q{Match unanchored <?isInEnclosedAlphanumerics>} );

# InEnclosedCJKLettersAndMonths


ok("\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<+<?isInEnclosedCJKLettersAndMonths>>$/, q{Match <?isInEnclosedCJKLettersAndMonths>} );
ok("\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<[A]+<?isInEnclosedCJKLettersAndMonths>>$/, q{Match compound <?isInEnclosedCJKLettersAndMonths>} );
ok(!( "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<-<?isInEnclosedCJKLettersAndMonths>>$/ ), q{Don't match externally inverted <?isInEnclosedCJKLettersAndMonths>} );
ok(!( "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<[A]-<?isInEnclosedCJKLettersAndMonths>>$/ ), q{Don't match compound inverted <?isInEnclosedCJKLettersAndMonths>} );
ok(!( "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<+<-isInEnclosedCJKLettersAndMonths>>$/ ), q{Don't match internally inverted <?isInEnclosedCJKLettersAndMonths>} );
ok(!( "\x[8883]"  ~~ m/^<+<?isInEnclosedCJKLettersAndMonths>>$/ ), q{Don't match unrelated <?isInEnclosedCJKLettersAndMonths>} );
ok("\x[8883]"  ~~ m/^<-<?isInEnclosedCJKLettersAndMonths>>$/, q{Match unrelated externally inverted <?isInEnclosedCJKLettersAndMonths>} );
ok("\x[8883]"  ~~ m/^<+<-isInEnclosedCJKLettersAndMonths>>$/, q{Match unrelated internally inverted <?isInEnclosedCJKLettersAndMonths>} );
ok("\x[8883]\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/<+<?isInEnclosedCJKLettersAndMonths>>/, q{Match unanchored <?isInEnclosedCJKLettersAndMonths>} );

# InEthiopic


ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<?isInEthiopic>>$/, q{Match <?isInEthiopic>} );
ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]+<?isInEthiopic>>$/, q{Match compound <?isInEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<-<?isInEthiopic>>$/ ), q{Don't match externally inverted <?isInEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]-<?isInEthiopic>>$/ ), q{Don't match compound inverted <?isInEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<-isInEthiopic>>$/ ), q{Don't match internally inverted <?isInEthiopic>} );
ok(!( "\c[MALAYALAM DIGIT NINE]"  ~~ m/^<+<?isInEthiopic>>$/ ), q{Don't match unrelated <?isInEthiopic>} );
ok("\c[MALAYALAM DIGIT NINE]"  ~~ m/^<-<?isInEthiopic>>$/, q{Match unrelated externally inverted <?isInEthiopic>} );
ok("\c[MALAYALAM DIGIT NINE]"  ~~ m/^<+<-isInEthiopic>>$/, q{Match unrelated internally inverted <?isInEthiopic>} );
ok("\c[MALAYALAM DIGIT NINE]\c[ETHIOPIC SYLLABLE HA]" ~~ m/<+<?isInEthiopic>>/, q{Match unanchored <?isInEthiopic>} );

# InGeneralPunctuation


ok("\c[EN QUAD]" ~~ m/^<+<?isInGeneralPunctuation>>$/, q{Match <?isInGeneralPunctuation>} );
ok("\c[EN QUAD]" ~~ m/^<[A]+<?isInGeneralPunctuation>>$/, q{Match compound <?isInGeneralPunctuation>} );
ok(!( "\c[EN QUAD]" ~~ m/^<-<?isInGeneralPunctuation>>$/ ), q{Don't match externally inverted <?isInGeneralPunctuation>} );
ok(!( "\c[EN QUAD]" ~~ m/^<[A]-<?isInGeneralPunctuation>>$/ ), q{Don't match compound inverted <?isInGeneralPunctuation>} );
ok(!( "\c[EN QUAD]" ~~ m/^<+<-isInGeneralPunctuation>>$/ ), q{Don't match internally inverted <?isInGeneralPunctuation>} );
ok(!( "\x[BBC9]"  ~~ m/^<+<?isInGeneralPunctuation>>$/ ), q{Don't match unrelated <?isInGeneralPunctuation>} );
ok("\x[BBC9]"  ~~ m/^<-<?isInGeneralPunctuation>>$/, q{Match unrelated externally inverted <?isInGeneralPunctuation>} );
ok("\x[BBC9]"  ~~ m/^<+<-isInGeneralPunctuation>>$/, q{Match unrelated internally inverted <?isInGeneralPunctuation>} );
ok("\x[BBC9]\c[EN QUAD]" ~~ m/<+<?isInGeneralPunctuation>>/, q{Match unanchored <?isInGeneralPunctuation>} );

# InGeometricShapes


ok("\c[BLACK SQUARE]" ~~ m/^<+<?isInGeometricShapes>>$/, q{Match <?isInGeometricShapes>} );
ok("\c[BLACK SQUARE]" ~~ m/^<[A]+<?isInGeometricShapes>>$/, q{Match compound <?isInGeometricShapes>} );
ok(!( "\c[BLACK SQUARE]" ~~ m/^<-<?isInGeometricShapes>>$/ ), q{Don't match externally inverted <?isInGeometricShapes>} );
ok(!( "\c[BLACK SQUARE]" ~~ m/^<[A]-<?isInGeometricShapes>>$/ ), q{Don't match compound inverted <?isInGeometricShapes>} );
ok(!( "\c[BLACK SQUARE]" ~~ m/^<+<-isInGeometricShapes>>$/ ), q{Don't match internally inverted <?isInGeometricShapes>} );
ok(!( "\x[C58A]"  ~~ m/^<+<?isInGeometricShapes>>$/ ), q{Don't match unrelated <?isInGeometricShapes>} );
ok("\x[C58A]"  ~~ m/^<-<?isInGeometricShapes>>$/, q{Match unrelated externally inverted <?isInGeometricShapes>} );
ok("\x[C58A]"  ~~ m/^<+<-isInGeometricShapes>>$/, q{Match unrelated internally inverted <?isInGeometricShapes>} );
ok("\x[C58A]\c[BLACK SQUARE]" ~~ m/<+<?isInGeometricShapes>>/, q{Match unanchored <?isInGeometricShapes>} );

# InGeorgian


ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<?isInGeorgian>>$/, q{Match <?isInGeorgian>} );
ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]+<?isInGeorgian>>$/, q{Match compound <?isInGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<-<?isInGeorgian>>$/ ), q{Don't match externally inverted <?isInGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]-<?isInGeorgian>>$/ ), q{Don't match compound inverted <?isInGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<-isInGeorgian>>$/ ), q{Don't match internally inverted <?isInGeorgian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER BEN]"  ~~ m/^<+<?isInGeorgian>>$/ ), q{Don't match unrelated <?isInGeorgian>} );
ok("\c[ARMENIAN CAPITAL LETTER BEN]"  ~~ m/^<-<?isInGeorgian>>$/, q{Match unrelated externally inverted <?isInGeorgian>} );
ok("\c[ARMENIAN CAPITAL LETTER BEN]"  ~~ m/^<+<-isInGeorgian>>$/, q{Match unrelated internally inverted <?isInGeorgian>} );
ok("\c[ARMENIAN CAPITAL LETTER BEN]\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/<+<?isInGeorgian>>/, q{Match unanchored <?isInGeorgian>} );

# InGothic


ok(!( "\x[1A5A]"  ~~ m/^<+<?isInGothic>>$/ ), q{Don't match unrelated <?isInGothic>} );
ok("\x[1A5A]"  ~~ m/^<-<?isInGothic>>$/, q{Match unrelated externally inverted <?isInGothic>} );
ok("\x[1A5A]"  ~~ m/^<+<-isInGothic>>$/, q{Match unrelated internally inverted <?isInGothic>} );

# InGreekExtended


ok("\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<+<?isInGreekExtended>>$/, q{Match <?isInGreekExtended>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<[A]+<?isInGreekExtended>>$/, q{Match compound <?isInGreekExtended>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<-<?isInGreekExtended>>$/ ), q{Don't match externally inverted <?isInGreekExtended>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<[A]-<?isInGreekExtended>>$/ ), q{Don't match compound inverted <?isInGreekExtended>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<+<-isInGreekExtended>>$/ ), q{Don't match internally inverted <?isInGreekExtended>} );
ok(!( "\x[39F3]"  ~~ m/^<+<?isInGreekExtended>>$/ ), q{Don't match unrelated <?isInGreekExtended>} );
ok("\x[39F3]"  ~~ m/^<-<?isInGreekExtended>>$/, q{Match unrelated externally inverted <?isInGreekExtended>} );
ok("\x[39F3]"  ~~ m/^<+<-isInGreekExtended>>$/, q{Match unrelated internally inverted <?isInGreekExtended>} );
ok("\x[39F3]\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/<+<?isInGreekExtended>>/, q{Match unanchored <?isInGreekExtended>} );

# InGreekAndCoptic


ok("\x[0370]" ~~ m/^<+<?isInGreekAndCoptic>>$/, q{Match <?isInGreekAndCoptic>} );
ok("\x[0370]" ~~ m/^<[A]+<?isInGreekAndCoptic>>$/, q{Match compound <?isInGreekAndCoptic>} );
ok(!( "\x[0370]" ~~ m/^<-<?isInGreekAndCoptic>>$/ ), q{Don't match externally inverted <?isInGreekAndCoptic>} );
ok(!( "\x[0370]" ~~ m/^<[A]-<?isInGreekAndCoptic>>$/ ), q{Don't match compound inverted <?isInGreekAndCoptic>} );
ok(!( "\x[0370]" ~~ m/^<+<-isInGreekAndCoptic>>$/ ), q{Don't match internally inverted <?isInGreekAndCoptic>} );
ok(!( "\x[8CFE]"  ~~ m/^<+<?isInGreekAndCoptic>>$/ ), q{Don't match unrelated <?isInGreekAndCoptic>} );
ok("\x[8CFE]"  ~~ m/^<-<?isInGreekAndCoptic>>$/, q{Match unrelated externally inverted <?isInGreekAndCoptic>} );
ok("\x[8CFE]"  ~~ m/^<+<-isInGreekAndCoptic>>$/, q{Match unrelated internally inverted <?isInGreekAndCoptic>} );
ok("\x[8CFE]\x[0370]" ~~ m/<+<?isInGreekAndCoptic>>/, q{Match unanchored <?isInGreekAndCoptic>} );

# InGujarati


ok("\x[0A80]" ~~ m/^<+<?isInGujarati>>$/, q{Match <?isInGujarati>} );
ok("\x[0A80]" ~~ m/^<[A]+<?isInGujarati>>$/, q{Match compound <?isInGujarati>} );
ok(!( "\x[0A80]" ~~ m/^<-<?isInGujarati>>$/ ), q{Don't match externally inverted <?isInGujarati>} );
ok(!( "\x[0A80]" ~~ m/^<[A]-<?isInGujarati>>$/ ), q{Don't match compound inverted <?isInGujarati>} );
ok(!( "\x[0A80]" ~~ m/^<+<-isInGujarati>>$/ ), q{Don't match internally inverted <?isInGujarati>} );
ok(!( "\x[B022]"  ~~ m/^<+<?isInGujarati>>$/ ), q{Don't match unrelated <?isInGujarati>} );
ok("\x[B022]"  ~~ m/^<-<?isInGujarati>>$/, q{Match unrelated externally inverted <?isInGujarati>} );
ok("\x[B022]"  ~~ m/^<+<-isInGujarati>>$/, q{Match unrelated internally inverted <?isInGujarati>} );
ok("\x[B022]\x[0A80]" ~~ m/<+<?isInGujarati>>/, q{Match unanchored <?isInGujarati>} );

# InGurmukhi


ok("\x[0A00]" ~~ m/^<+<?isInGurmukhi>>$/, q{Match <?isInGurmukhi>} );
ok("\x[0A00]" ~~ m/^<[A]+<?isInGurmukhi>>$/, q{Match compound <?isInGurmukhi>} );
ok(!( "\x[0A00]" ~~ m/^<-<?isInGurmukhi>>$/ ), q{Don't match externally inverted <?isInGurmukhi>} );
ok(!( "\x[0A00]" ~~ m/^<[A]-<?isInGurmukhi>>$/ ), q{Don't match compound inverted <?isInGurmukhi>} );
ok(!( "\x[0A00]" ~~ m/^<+<-isInGurmukhi>>$/ ), q{Don't match internally inverted <?isInGurmukhi>} );
ok(!( "\x[8FC3]"  ~~ m/^<+<?isInGurmukhi>>$/ ), q{Don't match unrelated <?isInGurmukhi>} );
ok("\x[8FC3]"  ~~ m/^<-<?isInGurmukhi>>$/, q{Match unrelated externally inverted <?isInGurmukhi>} );
ok("\x[8FC3]"  ~~ m/^<+<-isInGurmukhi>>$/, q{Match unrelated internally inverted <?isInGurmukhi>} );
ok("\x[8FC3]\x[0A00]" ~~ m/<+<?isInGurmukhi>>/, q{Match unanchored <?isInGurmukhi>} );

# InHalfwidthAndFullwidthForms


ok(!( "\x[36A3]"  ~~ m/^<+<?isInHalfwidthAndFullwidthForms>>$/ ), q{Don't match unrelated <?isInHalfwidthAndFullwidthForms>} );
ok("\x[36A3]"  ~~ m/^<-<?isInHalfwidthAndFullwidthForms>>$/, q{Match unrelated externally inverted <?isInHalfwidthAndFullwidthForms>} );
ok("\x[36A3]"  ~~ m/^<+<-isInHalfwidthAndFullwidthForms>>$/, q{Match unrelated internally inverted <?isInHalfwidthAndFullwidthForms>} );

# InHangulCompatibilityJamo


ok("\x[3130]" ~~ m/^<+<?isInHangulCompatibilityJamo>>$/, q{Match <?isInHangulCompatibilityJamo>} );
ok("\x[3130]" ~~ m/^<[A]+<?isInHangulCompatibilityJamo>>$/, q{Match compound <?isInHangulCompatibilityJamo>} );
ok(!( "\x[3130]" ~~ m/^<-<?isInHangulCompatibilityJamo>>$/ ), q{Don't match externally inverted <?isInHangulCompatibilityJamo>} );
ok(!( "\x[3130]" ~~ m/^<[A]-<?isInHangulCompatibilityJamo>>$/ ), q{Don't match compound inverted <?isInHangulCompatibilityJamo>} );
ok(!( "\x[3130]" ~~ m/^<+<-isInHangulCompatibilityJamo>>$/ ), q{Don't match internally inverted <?isInHangulCompatibilityJamo>} );
ok(!( "\x[BAF0]"  ~~ m/^<+<?isInHangulCompatibilityJamo>>$/ ), q{Don't match unrelated <?isInHangulCompatibilityJamo>} );
ok("\x[BAF0]"  ~~ m/^<-<?isInHangulCompatibilityJamo>>$/, q{Match unrelated externally inverted <?isInHangulCompatibilityJamo>} );
ok("\x[BAF0]"  ~~ m/^<+<-isInHangulCompatibilityJamo>>$/, q{Match unrelated internally inverted <?isInHangulCompatibilityJamo>} );
ok("\x[BAF0]\x[3130]" ~~ m/<+<?isInHangulCompatibilityJamo>>/, q{Match unanchored <?isInHangulCompatibilityJamo>} );

# InHangulJamo


ok("\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<+<?isInHangulJamo>>$/, q{Match <?isInHangulJamo>} );
ok("\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<[A]+<?isInHangulJamo>>$/, q{Match compound <?isInHangulJamo>} );
ok(!( "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<-<?isInHangulJamo>>$/ ), q{Don't match externally inverted <?isInHangulJamo>} );
ok(!( "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<[A]-<?isInHangulJamo>>$/ ), q{Don't match compound inverted <?isInHangulJamo>} );
ok(!( "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<+<-isInHangulJamo>>$/ ), q{Don't match internally inverted <?isInHangulJamo>} );
ok(!( "\x[4EB3]"  ~~ m/^<+<?isInHangulJamo>>$/ ), q{Don't match unrelated <?isInHangulJamo>} );
ok("\x[4EB3]"  ~~ m/^<-<?isInHangulJamo>>$/, q{Match unrelated externally inverted <?isInHangulJamo>} );
ok("\x[4EB3]"  ~~ m/^<+<-isInHangulJamo>>$/, q{Match unrelated internally inverted <?isInHangulJamo>} );
ok("\x[4EB3]\c[HANGUL CHOSEONG KIYEOK]" ~~ m/<+<?isInHangulJamo>>/, q{Match unanchored <?isInHangulJamo>} );

# InHangulSyllables


ok("\x[AC00]" ~~ m/^<+<?isInHangulSyllables>>$/, q{Match <?isInHangulSyllables>} );
ok("\x[AC00]" ~~ m/^<[A]+<?isInHangulSyllables>>$/, q{Match compound <?isInHangulSyllables>} );
ok(!( "\x[AC00]" ~~ m/^<-<?isInHangulSyllables>>$/ ), q{Don't match externally inverted <?isInHangulSyllables>} );
ok(!( "\x[AC00]" ~~ m/^<[A]-<?isInHangulSyllables>>$/ ), q{Don't match compound inverted <?isInHangulSyllables>} );
ok(!( "\x[AC00]" ~~ m/^<+<-isInHangulSyllables>>$/ ), q{Don't match internally inverted <?isInHangulSyllables>} );
ok(!( "\x[7D7E]"  ~~ m/^<+<?isInHangulSyllables>>$/ ), q{Don't match unrelated <?isInHangulSyllables>} );
ok("\x[7D7E]"  ~~ m/^<-<?isInHangulSyllables>>$/, q{Match unrelated externally inverted <?isInHangulSyllables>} );
ok("\x[7D7E]"  ~~ m/^<+<-isInHangulSyllables>>$/, q{Match unrelated internally inverted <?isInHangulSyllables>} );
ok("\x[7D7E]\x[AC00]" ~~ m/<+<?isInHangulSyllables>>/, q{Match unanchored <?isInHangulSyllables>} );

# InHanunoo


ok("\c[HANUNOO LETTER A]" ~~ m/^<+<?isInHanunoo>>$/, q{Match <?isInHanunoo>} );
ok("\c[HANUNOO LETTER A]" ~~ m/^<[A]+<?isInHanunoo>>$/, q{Match compound <?isInHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<-<?isInHanunoo>>$/ ), q{Don't match externally inverted <?isInHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<[A]-<?isInHanunoo>>$/ ), q{Don't match compound inverted <?isInHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<+<-isInHanunoo>>$/ ), q{Don't match internally inverted <?isInHanunoo>} );
ok(!( "\x[BD8A]"  ~~ m/^<+<?isInHanunoo>>$/ ), q{Don't match unrelated <?isInHanunoo>} );
ok("\x[BD8A]"  ~~ m/^<-<?isInHanunoo>>$/, q{Match unrelated externally inverted <?isInHanunoo>} );
ok("\x[BD8A]"  ~~ m/^<+<-isInHanunoo>>$/, q{Match unrelated internally inverted <?isInHanunoo>} );
ok("\x[BD8A]\c[HANUNOO LETTER A]" ~~ m/<+<?isInHanunoo>>/, q{Match unanchored <?isInHanunoo>} );

# InHebrew


ok("\x[0590]" ~~ m/^<+<?isInHebrew>>$/, q{Match <?isInHebrew>} );
ok("\x[0590]" ~~ m/^<[A]+<?isInHebrew>>$/, q{Match compound <?isInHebrew>} );
ok(!( "\x[0590]" ~~ m/^<-<?isInHebrew>>$/ ), q{Don't match externally inverted <?isInHebrew>} );
ok(!( "\x[0590]" ~~ m/^<[A]-<?isInHebrew>>$/ ), q{Don't match compound inverted <?isInHebrew>} );
ok(!( "\x[0590]" ~~ m/^<+<-isInHebrew>>$/ ), q{Don't match internally inverted <?isInHebrew>} );
ok(!( "\x[7EB3]"  ~~ m/^<+<?isInHebrew>>$/ ), q{Don't match unrelated <?isInHebrew>} );
ok("\x[7EB3]"  ~~ m/^<-<?isInHebrew>>$/, q{Match unrelated externally inverted <?isInHebrew>} );
ok("\x[7EB3]"  ~~ m/^<+<-isInHebrew>>$/, q{Match unrelated internally inverted <?isInHebrew>} );
ok("\x[7EB3]\x[0590]" ~~ m/<+<?isInHebrew>>/, q{Match unanchored <?isInHebrew>} );

# InHighPrivateUseSurrogates


ok(!( "\x[9EC6]"  ~~ m/^<+<?isInHighPrivateUseSurrogates>>$/ ), q{Don't match unrelated <?isInHighPrivateUseSurrogates>} );
ok("\x[9EC6]"  ~~ m/^<-<?isInHighPrivateUseSurrogates>>$/, q{Match unrelated externally inverted <?isInHighPrivateUseSurrogates>} );
ok("\x[9EC6]"  ~~ m/^<+<-isInHighPrivateUseSurrogates>>$/, q{Match unrelated internally inverted <?isInHighPrivateUseSurrogates>} );

# InHighSurrogates


ok(!( "\x[BC8F]"  ~~ m/^<+<?isInHighSurrogates>>$/ ), q{Don't match unrelated <?isInHighSurrogates>} );
ok("\x[BC8F]"  ~~ m/^<-<?isInHighSurrogates>>$/, q{Match unrelated externally inverted <?isInHighSurrogates>} );
ok("\x[BC8F]"  ~~ m/^<+<-isInHighSurrogates>>$/, q{Match unrelated internally inverted <?isInHighSurrogates>} );

# InHiragana


ok("\x[3040]" ~~ m/^<+<?isInHiragana>>$/, q{Match <?isInHiragana>} );
ok("\x[3040]" ~~ m/^<[A]+<?isInHiragana>>$/, q{Match compound <?isInHiragana>} );
ok(!( "\x[3040]" ~~ m/^<-<?isInHiragana>>$/ ), q{Don't match externally inverted <?isInHiragana>} );
ok(!( "\x[3040]" ~~ m/^<[A]-<?isInHiragana>>$/ ), q{Don't match compound inverted <?isInHiragana>} );
ok(!( "\x[3040]" ~~ m/^<+<-isInHiragana>>$/ ), q{Don't match internally inverted <?isInHiragana>} );
ok(!( "\c[ARABIC SIGN ALAYHE ASSALLAM]"  ~~ m/^<+<?isInHiragana>>$/ ), q{Don't match unrelated <?isInHiragana>} );
ok("\c[ARABIC SIGN ALAYHE ASSALLAM]"  ~~ m/^<-<?isInHiragana>>$/, q{Match unrelated externally inverted <?isInHiragana>} );
ok("\c[ARABIC SIGN ALAYHE ASSALLAM]"  ~~ m/^<+<-isInHiragana>>$/, q{Match unrelated internally inverted <?isInHiragana>} );
ok("\c[ARABIC SIGN ALAYHE ASSALLAM]\x[3040]" ~~ m/<+<?isInHiragana>>/, q{Match unanchored <?isInHiragana>} );

# InIPAExtensions


ok("\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<+<?isInIPAExtensions>>$/, q{Match <?isInIPAExtensions>} );
ok("\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<[A]+<?isInIPAExtensions>>$/, q{Match compound <?isInIPAExtensions>} );
ok(!( "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<-<?isInIPAExtensions>>$/ ), q{Don't match externally inverted <?isInIPAExtensions>} );
ok(!( "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<[A]-<?isInIPAExtensions>>$/ ), q{Don't match compound inverted <?isInIPAExtensions>} );
ok(!( "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<+<-isInIPAExtensions>>$/ ), q{Don't match internally inverted <?isInIPAExtensions>} );
ok(!( "\x[0DFC]"  ~~ m/^<+<?isInIPAExtensions>>$/ ), q{Don't match unrelated <?isInIPAExtensions>} );
ok("\x[0DFC]"  ~~ m/^<-<?isInIPAExtensions>>$/, q{Match unrelated externally inverted <?isInIPAExtensions>} );
ok("\x[0DFC]"  ~~ m/^<+<-isInIPAExtensions>>$/, q{Match unrelated internally inverted <?isInIPAExtensions>} );
ok("\x[0DFC]\c[LATIN SMALL LETTER TURNED A]" ~~ m/<+<?isInIPAExtensions>>/, q{Match unanchored <?isInIPAExtensions>} );

# InIdeographicDescriptionCharacters


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<?isInIdeographicDescriptionCharacters>>$/, q{Match <?isInIdeographicDescriptionCharacters>} );
ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]+<?isInIdeographicDescriptionCharacters>>$/, q{Match compound <?isInIdeographicDescriptionCharacters>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<-<?isInIdeographicDescriptionCharacters>>$/ ), q{Don't match externally inverted <?isInIdeographicDescriptionCharacters>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]-<?isInIdeographicDescriptionCharacters>>$/ ), q{Don't match compound inverted <?isInIdeographicDescriptionCharacters>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<-isInIdeographicDescriptionCharacters>>$/ ), q{Don't match internally inverted <?isInIdeographicDescriptionCharacters>} );
ok(!( "\c[LATIN CAPITAL LETTER AE]"  ~~ m/^<+<?isInIdeographicDescriptionCharacters>>$/ ), q{Don't match unrelated <?isInIdeographicDescriptionCharacters>} );
ok("\c[LATIN CAPITAL LETTER AE]"  ~~ m/^<-<?isInIdeographicDescriptionCharacters>>$/, q{Match unrelated externally inverted <?isInIdeographicDescriptionCharacters>} );
ok("\c[LATIN CAPITAL LETTER AE]"  ~~ m/^<+<-isInIdeographicDescriptionCharacters>>$/, q{Match unrelated internally inverted <?isInIdeographicDescriptionCharacters>} );
ok("\c[LATIN CAPITAL LETTER AE]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/<+<?isInIdeographicDescriptionCharacters>>/, q{Match unanchored <?isInIdeographicDescriptionCharacters>} );

# InKanbun


ok("\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<+<?isInKanbun>>$/, q{Match <?isInKanbun>} );
ok("\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<[A]+<?isInKanbun>>$/, q{Match compound <?isInKanbun>} );
ok(!( "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<-<?isInKanbun>>$/ ), q{Don't match externally inverted <?isInKanbun>} );
ok(!( "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<[A]-<?isInKanbun>>$/ ), q{Don't match compound inverted <?isInKanbun>} );
ok(!( "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<+<-isInKanbun>>$/ ), q{Don't match internally inverted <?isInKanbun>} );
ok(!( "\x[93AB]"  ~~ m/^<+<?isInKanbun>>$/ ), q{Don't match unrelated <?isInKanbun>} );
ok("\x[93AB]"  ~~ m/^<-<?isInKanbun>>$/, q{Match unrelated externally inverted <?isInKanbun>} );
ok("\x[93AB]"  ~~ m/^<+<-isInKanbun>>$/, q{Match unrelated internally inverted <?isInKanbun>} );
ok("\x[93AB]\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/<+<?isInKanbun>>/, q{Match unanchored <?isInKanbun>} );

# InKangxiRadicals


ok("\c[KANGXI RADICAL ONE]" ~~ m/^<+<?isInKangxiRadicals>>$/, q{Match <?isInKangxiRadicals>} );
ok("\c[KANGXI RADICAL ONE]" ~~ m/^<[A]+<?isInKangxiRadicals>>$/, q{Match compound <?isInKangxiRadicals>} );
ok(!( "\c[KANGXI RADICAL ONE]" ~~ m/^<-<?isInKangxiRadicals>>$/ ), q{Don't match externally inverted <?isInKangxiRadicals>} );
ok(!( "\c[KANGXI RADICAL ONE]" ~~ m/^<[A]-<?isInKangxiRadicals>>$/ ), q{Don't match compound inverted <?isInKangxiRadicals>} );
ok(!( "\c[KANGXI RADICAL ONE]" ~~ m/^<+<-isInKangxiRadicals>>$/ ), q{Don't match internally inverted <?isInKangxiRadicals>} );
ok(!( "\x[363D]"  ~~ m/^<+<?isInKangxiRadicals>>$/ ), q{Don't match unrelated <?isInKangxiRadicals>} );
ok("\x[363D]"  ~~ m/^<-<?isInKangxiRadicals>>$/, q{Match unrelated externally inverted <?isInKangxiRadicals>} );
ok("\x[363D]"  ~~ m/^<+<-isInKangxiRadicals>>$/, q{Match unrelated internally inverted <?isInKangxiRadicals>} );
ok("\x[363D]\c[KANGXI RADICAL ONE]" ~~ m/<+<?isInKangxiRadicals>>/, q{Match unanchored <?isInKangxiRadicals>} );

# InKannada


ok("\x[0C80]" ~~ m/^<+<?isInKannada>>$/, q{Match <?isInKannada>} );
ok("\x[0C80]" ~~ m/^<[A]+<?isInKannada>>$/, q{Match compound <?isInKannada>} );
ok(!( "\x[0C80]" ~~ m/^<-<?isInKannada>>$/ ), q{Don't match externally inverted <?isInKannada>} );
ok(!( "\x[0C80]" ~~ m/^<[A]-<?isInKannada>>$/ ), q{Don't match compound inverted <?isInKannada>} );
ok(!( "\x[0C80]" ~~ m/^<+<-isInKannada>>$/ ), q{Don't match internally inverted <?isInKannada>} );
ok(!( "\x[9093]"  ~~ m/^<+<?isInKannada>>$/ ), q{Don't match unrelated <?isInKannada>} );
ok("\x[9093]"  ~~ m/^<-<?isInKannada>>$/, q{Match unrelated externally inverted <?isInKannada>} );
ok("\x[9093]"  ~~ m/^<+<-isInKannada>>$/, q{Match unrelated internally inverted <?isInKannada>} );
ok("\x[9093]\x[0C80]" ~~ m/<+<?isInKannada>>/, q{Match unanchored <?isInKannada>} );

# InKatakana


ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<?isInKatakana>>$/, q{Match <?isInKatakana>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]+<?isInKatakana>>$/, q{Match compound <?isInKatakana>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<-<?isInKatakana>>$/ ), q{Don't match externally inverted <?isInKatakana>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]-<?isInKatakana>>$/ ), q{Don't match compound inverted <?isInKatakana>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<-isInKatakana>>$/ ), q{Don't match internally inverted <?isInKatakana>} );
ok(!( "\x[75DA]"  ~~ m/^<+<?isInKatakana>>$/ ), q{Don't match unrelated <?isInKatakana>} );
ok("\x[75DA]"  ~~ m/^<-<?isInKatakana>>$/, q{Match unrelated externally inverted <?isInKatakana>} );
ok("\x[75DA]"  ~~ m/^<+<-isInKatakana>>$/, q{Match unrelated internally inverted <?isInKatakana>} );
ok("\x[75DA]\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/<+<?isInKatakana>>/, q{Match unanchored <?isInKatakana>} );

# InKatakanaPhoneticExtensions


ok("\c[KATAKANA LETTER SMALL KU]" ~~ m/^<+<?isInKatakanaPhoneticExtensions>>$/, q{Match <?isInKatakanaPhoneticExtensions>} );
ok("\c[KATAKANA LETTER SMALL KU]" ~~ m/^<[A]+<?isInKatakanaPhoneticExtensions>>$/, q{Match compound <?isInKatakanaPhoneticExtensions>} );
ok(!( "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<-<?isInKatakanaPhoneticExtensions>>$/ ), q{Don't match externally inverted <?isInKatakanaPhoneticExtensions>} );
ok(!( "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<[A]-<?isInKatakanaPhoneticExtensions>>$/ ), q{Don't match compound inverted <?isInKatakanaPhoneticExtensions>} );
ok(!( "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<+<-isInKatakanaPhoneticExtensions>>$/ ), q{Don't match internally inverted <?isInKatakanaPhoneticExtensions>} );
ok(!( "\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]"  ~~ m/^<+<?isInKatakanaPhoneticExtensions>>$/ ), q{Don't match unrelated <?isInKatakanaPhoneticExtensions>} );
ok("\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]"  ~~ m/^<-<?isInKatakanaPhoneticExtensions>>$/, q{Match unrelated externally inverted <?isInKatakanaPhoneticExtensions>} );
ok("\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]"  ~~ m/^<+<-isInKatakanaPhoneticExtensions>>$/, q{Match unrelated internally inverted <?isInKatakanaPhoneticExtensions>} );
ok("\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]\c[KATAKANA LETTER SMALL KU]" ~~ m/<+<?isInKatakanaPhoneticExtensions>>/, q{Match unanchored <?isInKatakanaPhoneticExtensions>} );

# InKhmer


ok("\c[KHMER LETTER KA]" ~~ m/^<+<?isInKhmer>>$/, q{Match <?isInKhmer>} );
ok("\c[KHMER LETTER KA]" ~~ m/^<[A]+<?isInKhmer>>$/, q{Match compound <?isInKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<-<?isInKhmer>>$/ ), q{Don't match externally inverted <?isInKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<[A]-<?isInKhmer>>$/ ), q{Don't match compound inverted <?isInKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<+<-isInKhmer>>$/ ), q{Don't match internally inverted <?isInKhmer>} );
ok(!( "\x[508C]"  ~~ m/^<+<?isInKhmer>>$/ ), q{Don't match unrelated <?isInKhmer>} );
ok("\x[508C]"  ~~ m/^<-<?isInKhmer>>$/, q{Match unrelated externally inverted <?isInKhmer>} );
ok("\x[508C]"  ~~ m/^<+<-isInKhmer>>$/, q{Match unrelated internally inverted <?isInKhmer>} );
ok("\x[508C]\c[KHMER LETTER KA]" ~~ m/<+<?isInKhmer>>/, q{Match unanchored <?isInKhmer>} );

# InLao


ok("\x[0E80]" ~~ m/^<+<?isInLao>>$/, q{Match <?isInLao>} );
ok("\x[0E80]" ~~ m/^<[A]+<?isInLao>>$/, q{Match compound <?isInLao>} );
ok(!( "\x[0E80]" ~~ m/^<-<?isInLao>>$/ ), q{Don't match externally inverted <?isInLao>} );
ok(!( "\x[0E80]" ~~ m/^<[A]-<?isInLao>>$/ ), q{Don't match compound inverted <?isInLao>} );
ok(!( "\x[0E80]" ~~ m/^<+<-isInLao>>$/ ), q{Don't match internally inverted <?isInLao>} );
ok(!( "\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]"  ~~ m/^<+<?isInLao>>$/ ), q{Don't match unrelated <?isInLao>} );
ok("\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]"  ~~ m/^<-<?isInLao>>$/, q{Match unrelated externally inverted <?isInLao>} );
ok("\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]"  ~~ m/^<+<-isInLao>>$/, q{Match unrelated internally inverted <?isInLao>} );
ok("\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]\x[0E80]" ~~ m/<+<?isInLao>>/, q{Match unanchored <?isInLao>} );

# InLatin1Supplement


ok("\x[0080]" ~~ m/^<+<?isInLatin1Supplement>>$/, q{Match <?isInLatin1Supplement>} );
ok("\x[0080]" ~~ m/^<[A]+<?isInLatin1Supplement>>$/, q{Match compound <?isInLatin1Supplement>} );
ok(!( "\x[0080]" ~~ m/^<-<?isInLatin1Supplement>>$/ ), q{Don't match externally inverted <?isInLatin1Supplement>} );
ok(!( "\x[0080]" ~~ m/^<[A]-<?isInLatin1Supplement>>$/ ), q{Don't match compound inverted <?isInLatin1Supplement>} );
ok(!( "\x[0080]" ~~ m/^<+<-isInLatin1Supplement>>$/ ), q{Don't match internally inverted <?isInLatin1Supplement>} );
ok(!( "\x[3A43]"  ~~ m/^<+<?isInLatin1Supplement>>$/ ), q{Don't match unrelated <?isInLatin1Supplement>} );
ok("\x[3A43]"  ~~ m/^<-<?isInLatin1Supplement>>$/, q{Match unrelated externally inverted <?isInLatin1Supplement>} );
ok("\x[3A43]"  ~~ m/^<+<-isInLatin1Supplement>>$/, q{Match unrelated internally inverted <?isInLatin1Supplement>} );
ok("\x[3A43]\x[0080]" ~~ m/<+<?isInLatin1Supplement>>/, q{Match unanchored <?isInLatin1Supplement>} );

# InLatinExtendedA


ok("\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<+<?isInLatinExtendedA>>$/, q{Match <?isInLatinExtendedA>} );
ok("\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<[A]+<?isInLatinExtendedA>>$/, q{Match compound <?isInLatinExtendedA>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<-<?isInLatinExtendedA>>$/ ), q{Don't match externally inverted <?isInLatinExtendedA>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<[A]-<?isInLatinExtendedA>>$/ ), q{Don't match compound inverted <?isInLatinExtendedA>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<+<-isInLatinExtendedA>>$/ ), q{Don't match internally inverted <?isInLatinExtendedA>} );
ok(!( "\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]"  ~~ m/^<+<?isInLatinExtendedA>>$/ ), q{Don't match unrelated <?isInLatinExtendedA>} );
ok("\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]"  ~~ m/^<-<?isInLatinExtendedA>>$/, q{Match unrelated externally inverted <?isInLatinExtendedA>} );
ok("\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]"  ~~ m/^<+<-isInLatinExtendedA>>$/, q{Match unrelated internally inverted <?isInLatinExtendedA>} );
ok("\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/<+<?isInLatinExtendedA>>/, q{Match unanchored <?isInLatinExtendedA>} );

# InLatinExtendedAdditional


ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<?isInLatinExtendedAdditional>>$/, q{Match <?isInLatinExtendedAdditional>} );
ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]+<?isInLatinExtendedAdditional>>$/, q{Match compound <?isInLatinExtendedAdditional>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<-<?isInLatinExtendedAdditional>>$/ ), q{Don't match externally inverted <?isInLatinExtendedAdditional>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]-<?isInLatinExtendedAdditional>>$/ ), q{Don't match compound inverted <?isInLatinExtendedAdditional>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<-isInLatinExtendedAdditional>>$/ ), q{Don't match internally inverted <?isInLatinExtendedAdditional>} );
ok(!( "\x[36E7]"  ~~ m/^<+<?isInLatinExtendedAdditional>>$/ ), q{Don't match unrelated <?isInLatinExtendedAdditional>} );
ok("\x[36E7]"  ~~ m/^<-<?isInLatinExtendedAdditional>>$/, q{Match unrelated externally inverted <?isInLatinExtendedAdditional>} );
ok("\x[36E7]"  ~~ m/^<+<-isInLatinExtendedAdditional>>$/, q{Match unrelated internally inverted <?isInLatinExtendedAdditional>} );
ok("\x[36E7]\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/<+<?isInLatinExtendedAdditional>>/, q{Match unanchored <?isInLatinExtendedAdditional>} );

# InLatinExtendedB


ok("\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<+<?isInLatinExtendedB>>$/, q{Match <?isInLatinExtendedB>} );
ok("\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<[A]+<?isInLatinExtendedB>>$/, q{Match compound <?isInLatinExtendedB>} );
ok(!( "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<-<?isInLatinExtendedB>>$/ ), q{Don't match externally inverted <?isInLatinExtendedB>} );
ok(!( "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<[A]-<?isInLatinExtendedB>>$/ ), q{Don't match compound inverted <?isInLatinExtendedB>} );
ok(!( "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<+<-isInLatinExtendedB>>$/ ), q{Don't match internally inverted <?isInLatinExtendedB>} );
ok(!( "\x[877E]"  ~~ m/^<+<?isInLatinExtendedB>>$/ ), q{Don't match unrelated <?isInLatinExtendedB>} );
ok("\x[877E]"  ~~ m/^<-<?isInLatinExtendedB>>$/, q{Match unrelated externally inverted <?isInLatinExtendedB>} );
ok("\x[877E]"  ~~ m/^<+<-isInLatinExtendedB>>$/, q{Match unrelated internally inverted <?isInLatinExtendedB>} );
ok("\x[877E]\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/<+<?isInLatinExtendedB>>/, q{Match unanchored <?isInLatinExtendedB>} );

# InLetterlikeSymbols


ok("\c[ACCOUNT OF]" ~~ m/^<+<?isInLetterlikeSymbols>>$/, q{Match <?isInLetterlikeSymbols>} );
ok("\c[ACCOUNT OF]" ~~ m/^<[A]+<?isInLetterlikeSymbols>>$/, q{Match compound <?isInLetterlikeSymbols>} );
ok(!( "\c[ACCOUNT OF]" ~~ m/^<-<?isInLetterlikeSymbols>>$/ ), q{Don't match externally inverted <?isInLetterlikeSymbols>} );
ok(!( "\c[ACCOUNT OF]" ~~ m/^<[A]-<?isInLetterlikeSymbols>>$/ ), q{Don't match compound inverted <?isInLetterlikeSymbols>} );
ok(!( "\c[ACCOUNT OF]" ~~ m/^<+<-isInLetterlikeSymbols>>$/ ), q{Don't match internally inverted <?isInLetterlikeSymbols>} );
ok(!( "\c[CIRCLED IDEOGRAPH TWO]"  ~~ m/^<+<?isInLetterlikeSymbols>>$/ ), q{Don't match unrelated <?isInLetterlikeSymbols>} );
ok("\c[CIRCLED IDEOGRAPH TWO]"  ~~ m/^<-<?isInLetterlikeSymbols>>$/, q{Match unrelated externally inverted <?isInLetterlikeSymbols>} );
ok("\c[CIRCLED IDEOGRAPH TWO]"  ~~ m/^<+<-isInLetterlikeSymbols>>$/, q{Match unrelated internally inverted <?isInLetterlikeSymbols>} );
ok("\c[CIRCLED IDEOGRAPH TWO]\c[ACCOUNT OF]" ~~ m/<+<?isInLetterlikeSymbols>>/, q{Match unanchored <?isInLetterlikeSymbols>} );

# InLowSurrogates


ok(!( "\x[B611]"  ~~ m/^<+<?isInLowSurrogates>>$/ ), q{Don't match unrelated <?isInLowSurrogates>} );
ok("\x[B611]"  ~~ m/^<-<?isInLowSurrogates>>$/, q{Match unrelated externally inverted <?isInLowSurrogates>} );
ok("\x[B611]"  ~~ m/^<+<-isInLowSurrogates>>$/, q{Match unrelated internally inverted <?isInLowSurrogates>} );

# InMalayalam


ok("\x[0D00]" ~~ m/^<+<?isInMalayalam>>$/, q{Match <?isInMalayalam>} );
ok("\x[0D00]" ~~ m/^<[A]+<?isInMalayalam>>$/, q{Match compound <?isInMalayalam>} );
ok(!( "\x[0D00]" ~~ m/^<-<?isInMalayalam>>$/ ), q{Don't match externally inverted <?isInMalayalam>} );
ok(!( "\x[0D00]" ~~ m/^<[A]-<?isInMalayalam>>$/ ), q{Don't match compound inverted <?isInMalayalam>} );
ok(!( "\x[0D00]" ~~ m/^<+<-isInMalayalam>>$/ ), q{Don't match internally inverted <?isInMalayalam>} );
ok(!( "\x[C011]"  ~~ m/^<+<?isInMalayalam>>$/ ), q{Don't match unrelated <?isInMalayalam>} );
ok("\x[C011]"  ~~ m/^<-<?isInMalayalam>>$/, q{Match unrelated externally inverted <?isInMalayalam>} );
ok("\x[C011]"  ~~ m/^<+<-isInMalayalam>>$/, q{Match unrelated internally inverted <?isInMalayalam>} );
ok("\x[C011]\x[0D00]" ~~ m/<+<?isInMalayalam>>/, q{Match unanchored <?isInMalayalam>} );

# InMathematicalAlphanumericSymbols


ok(!( "\x[73FA]"  ~~ m/^<+<?isInMathematicalAlphanumericSymbols>>$/ ), q{Don't match unrelated <?isInMathematicalAlphanumericSymbols>} );
ok("\x[73FA]"  ~~ m/^<-<?isInMathematicalAlphanumericSymbols>>$/, q{Match unrelated externally inverted <?isInMathematicalAlphanumericSymbols>} );
ok("\x[73FA]"  ~~ m/^<+<-isInMathematicalAlphanumericSymbols>>$/, q{Match unrelated internally inverted <?isInMathematicalAlphanumericSymbols>} );

# InMathematicalOperators


ok("\c[FOR ALL]" ~~ m/^<+<?isInMathematicalOperators>>$/, q{Match <?isInMathematicalOperators>} );
ok("\c[FOR ALL]" ~~ m/^<[A]+<?isInMathematicalOperators>>$/, q{Match compound <?isInMathematicalOperators>} );
ok(!( "\c[FOR ALL]" ~~ m/^<-<?isInMathematicalOperators>>$/ ), q{Don't match externally inverted <?isInMathematicalOperators>} );
ok(!( "\c[FOR ALL]" ~~ m/^<[A]-<?isInMathematicalOperators>>$/ ), q{Don't match compound inverted <?isInMathematicalOperators>} );
ok(!( "\c[FOR ALL]" ~~ m/^<+<-isInMathematicalOperators>>$/ ), q{Don't match internally inverted <?isInMathematicalOperators>} );
ok(!( "\x[B389]"  ~~ m/^<+<?isInMathematicalOperators>>$/ ), q{Don't match unrelated <?isInMathematicalOperators>} );
ok("\x[B389]"  ~~ m/^<-<?isInMathematicalOperators>>$/, q{Match unrelated externally inverted <?isInMathematicalOperators>} );
ok("\x[B389]"  ~~ m/^<+<-isInMathematicalOperators>>$/, q{Match unrelated internally inverted <?isInMathematicalOperators>} );
ok("\x[B389]\c[FOR ALL]" ~~ m/<+<?isInMathematicalOperators>>/, q{Match unanchored <?isInMathematicalOperators>} );

# InMiscellaneousMathematicalSymbolsA


ok("\x[27C0]" ~~ m/^<+<?isInMiscellaneousMathematicalSymbolsA>>$/, q{Match <?isInMiscellaneousMathematicalSymbolsA>} );
ok("\x[27C0]" ~~ m/^<[A]+<?isInMiscellaneousMathematicalSymbolsA>>$/, q{Match compound <?isInMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[27C0]" ~~ m/^<-<?isInMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match externally inverted <?isInMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[27C0]" ~~ m/^<[A]-<?isInMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match compound inverted <?isInMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[27C0]" ~~ m/^<+<-isInMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match internally inverted <?isInMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[AAB2]"  ~~ m/^<+<?isInMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match unrelated <?isInMiscellaneousMathematicalSymbolsA>} );
ok("\x[AAB2]"  ~~ m/^<-<?isInMiscellaneousMathematicalSymbolsA>>$/, q{Match unrelated externally inverted <?isInMiscellaneousMathematicalSymbolsA>} );
ok("\x[AAB2]"  ~~ m/^<+<-isInMiscellaneousMathematicalSymbolsA>>$/, q{Match unrelated internally inverted <?isInMiscellaneousMathematicalSymbolsA>} );
ok("\x[AAB2]\x[27C0]" ~~ m/<+<?isInMiscellaneousMathematicalSymbolsA>>/, q{Match unanchored <?isInMiscellaneousMathematicalSymbolsA>} );

# InMiscellaneousMathematicalSymbolsB


ok("\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<+<?isInMiscellaneousMathematicalSymbolsB>>$/, q{Match <?isInMiscellaneousMathematicalSymbolsB>} );
ok("\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<[A]+<?isInMiscellaneousMathematicalSymbolsB>>$/, q{Match compound <?isInMiscellaneousMathematicalSymbolsB>} );
ok(!( "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<-<?isInMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match externally inverted <?isInMiscellaneousMathematicalSymbolsB>} );
ok(!( "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<[A]-<?isInMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match compound inverted <?isInMiscellaneousMathematicalSymbolsB>} );
ok(!( "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<+<-isInMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match internally inverted <?isInMiscellaneousMathematicalSymbolsB>} );
ok(!( "\x[5793]"  ~~ m/^<+<?isInMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match unrelated <?isInMiscellaneousMathematicalSymbolsB>} );
ok("\x[5793]"  ~~ m/^<-<?isInMiscellaneousMathematicalSymbolsB>>$/, q{Match unrelated externally inverted <?isInMiscellaneousMathematicalSymbolsB>} );
ok("\x[5793]"  ~~ m/^<+<-isInMiscellaneousMathematicalSymbolsB>>$/, q{Match unrelated internally inverted <?isInMiscellaneousMathematicalSymbolsB>} );
ok("\x[5793]\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/<+<?isInMiscellaneousMathematicalSymbolsB>>/, q{Match unanchored <?isInMiscellaneousMathematicalSymbolsB>} );

# InMiscellaneousSymbols


ok("\c[BLACK SUN WITH RAYS]" ~~ m/^<+<?isInMiscellaneousSymbols>>$/, q{Match <?isInMiscellaneousSymbols>} );
ok("\c[BLACK SUN WITH RAYS]" ~~ m/^<[A]+<?isInMiscellaneousSymbols>>$/, q{Match compound <?isInMiscellaneousSymbols>} );
ok(!( "\c[BLACK SUN WITH RAYS]" ~~ m/^<-<?isInMiscellaneousSymbols>>$/ ), q{Don't match externally inverted <?isInMiscellaneousSymbols>} );
ok(!( "\c[BLACK SUN WITH RAYS]" ~~ m/^<[A]-<?isInMiscellaneousSymbols>>$/ ), q{Don't match compound inverted <?isInMiscellaneousSymbols>} );
ok(!( "\c[BLACK SUN WITH RAYS]" ~~ m/^<+<-isInMiscellaneousSymbols>>$/ ), q{Don't match internally inverted <?isInMiscellaneousSymbols>} );
ok(!( "\x[39D9]"  ~~ m/^<+<?isInMiscellaneousSymbols>>$/ ), q{Don't match unrelated <?isInMiscellaneousSymbols>} );
ok("\x[39D9]"  ~~ m/^<-<?isInMiscellaneousSymbols>>$/, q{Match unrelated externally inverted <?isInMiscellaneousSymbols>} );
ok("\x[39D9]"  ~~ m/^<+<-isInMiscellaneousSymbols>>$/, q{Match unrelated internally inverted <?isInMiscellaneousSymbols>} );
ok("\x[39D9]\c[BLACK SUN WITH RAYS]" ~~ m/<+<?isInMiscellaneousSymbols>>/, q{Match unanchored <?isInMiscellaneousSymbols>} );

# InMiscellaneousTechnical


ok("\c[DIAMETER SIGN]" ~~ m/^<+<?isInMiscellaneousTechnical>>$/, q{Match <?isInMiscellaneousTechnical>} );
ok("\c[DIAMETER SIGN]" ~~ m/^<[A]+<?isInMiscellaneousTechnical>>$/, q{Match compound <?isInMiscellaneousTechnical>} );
ok(!( "\c[DIAMETER SIGN]" ~~ m/^<-<?isInMiscellaneousTechnical>>$/ ), q{Don't match externally inverted <?isInMiscellaneousTechnical>} );
ok(!( "\c[DIAMETER SIGN]" ~~ m/^<[A]-<?isInMiscellaneousTechnical>>$/ ), q{Don't match compound inverted <?isInMiscellaneousTechnical>} );
ok(!( "\c[DIAMETER SIGN]" ~~ m/^<+<-isInMiscellaneousTechnical>>$/ ), q{Don't match internally inverted <?isInMiscellaneousTechnical>} );
ok(!( "\x[528A]"  ~~ m/^<+<?isInMiscellaneousTechnical>>$/ ), q{Don't match unrelated <?isInMiscellaneousTechnical>} );
ok("\x[528A]"  ~~ m/^<-<?isInMiscellaneousTechnical>>$/, q{Match unrelated externally inverted <?isInMiscellaneousTechnical>} );
ok("\x[528A]"  ~~ m/^<+<-isInMiscellaneousTechnical>>$/, q{Match unrelated internally inverted <?isInMiscellaneousTechnical>} );
ok("\x[528A]\c[DIAMETER SIGN]" ~~ m/<+<?isInMiscellaneousTechnical>>/, q{Match unanchored <?isInMiscellaneousTechnical>} );

# InMongolian


ok("\c[MONGOLIAN BIRGA]" ~~ m/^<+<?isInMongolian>>$/, q{Match <?isInMongolian>} );
ok("\c[MONGOLIAN BIRGA]" ~~ m/^<[A]+<?isInMongolian>>$/, q{Match compound <?isInMongolian>} );
ok(!( "\c[MONGOLIAN BIRGA]" ~~ m/^<-<?isInMongolian>>$/ ), q{Don't match externally inverted <?isInMongolian>} );
ok(!( "\c[MONGOLIAN BIRGA]" ~~ m/^<[A]-<?isInMongolian>>$/ ), q{Don't match compound inverted <?isInMongolian>} );
ok(!( "\c[MONGOLIAN BIRGA]" ~~ m/^<+<-isInMongolian>>$/ ), q{Don't match internally inverted <?isInMongolian>} );
ok(!( "\x[3985]"  ~~ m/^<+<?isInMongolian>>$/ ), q{Don't match unrelated <?isInMongolian>} );
ok("\x[3985]"  ~~ m/^<-<?isInMongolian>>$/, q{Match unrelated externally inverted <?isInMongolian>} );
ok("\x[3985]"  ~~ m/^<+<-isInMongolian>>$/, q{Match unrelated internally inverted <?isInMongolian>} );
ok("\x[3985]\c[MONGOLIAN BIRGA]" ~~ m/<+<?isInMongolian>>/, q{Match unanchored <?isInMongolian>} );

# InMusicalSymbols


ok(!( "\x[7A59]"  ~~ m/^<+<?isInMusicalSymbols>>$/ ), q{Don't match unrelated <?isInMusicalSymbols>} );
ok("\x[7A59]"  ~~ m/^<-<?isInMusicalSymbols>>$/, q{Match unrelated externally inverted <?isInMusicalSymbols>} );
ok("\x[7A59]"  ~~ m/^<+<-isInMusicalSymbols>>$/, q{Match unrelated internally inverted <?isInMusicalSymbols>} );

# InMyanmar


ok("\c[MYANMAR LETTER KA]" ~~ m/^<+<?isInMyanmar>>$/, q{Match <?isInMyanmar>} );
ok("\c[MYANMAR LETTER KA]" ~~ m/^<[A]+<?isInMyanmar>>$/, q{Match compound <?isInMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<-<?isInMyanmar>>$/ ), q{Don't match externally inverted <?isInMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<[A]-<?isInMyanmar>>$/ ), q{Don't match compound inverted <?isInMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<+<-isInMyanmar>>$/ ), q{Don't match internally inverted <?isInMyanmar>} );
ok(!( "\x[5698]"  ~~ m/^<+<?isInMyanmar>>$/ ), q{Don't match unrelated <?isInMyanmar>} );
ok("\x[5698]"  ~~ m/^<-<?isInMyanmar>>$/, q{Match unrelated externally inverted <?isInMyanmar>} );
ok("\x[5698]"  ~~ m/^<+<-isInMyanmar>>$/, q{Match unrelated internally inverted <?isInMyanmar>} );
ok("\x[5698]\c[MYANMAR LETTER KA]" ~~ m/<+<?isInMyanmar>>/, q{Match unanchored <?isInMyanmar>} );

# InNumberForms


ok("\x[2150]" ~~ m/^<+<?isInNumberForms>>$/, q{Match <?isInNumberForms>} );
ok("\x[2150]" ~~ m/^<[A]+<?isInNumberForms>>$/, q{Match compound <?isInNumberForms>} );
ok(!( "\x[2150]" ~~ m/^<-<?isInNumberForms>>$/ ), q{Don't match externally inverted <?isInNumberForms>} );
ok(!( "\x[2150]" ~~ m/^<[A]-<?isInNumberForms>>$/ ), q{Don't match compound inverted <?isInNumberForms>} );
ok(!( "\x[2150]" ~~ m/^<+<-isInNumberForms>>$/ ), q{Don't match internally inverted <?isInNumberForms>} );
ok(!( "\x[C41B]"  ~~ m/^<+<?isInNumberForms>>$/ ), q{Don't match unrelated <?isInNumberForms>} );
ok("\x[C41B]"  ~~ m/^<-<?isInNumberForms>>$/, q{Match unrelated externally inverted <?isInNumberForms>} );
ok("\x[C41B]"  ~~ m/^<+<-isInNumberForms>>$/, q{Match unrelated internally inverted <?isInNumberForms>} );
ok("\x[C41B]\x[2150]" ~~ m/<+<?isInNumberForms>>/, q{Match unanchored <?isInNumberForms>} );

# InOgham


ok("\c[OGHAM SPACE MARK]" ~~ m/^<+<?isInOgham>>$/, q{Match <?isInOgham>} );
ok("\c[OGHAM SPACE MARK]" ~~ m/^<[A]+<?isInOgham>>$/, q{Match compound <?isInOgham>} );
ok(!( "\c[OGHAM SPACE MARK]" ~~ m/^<-<?isInOgham>>$/ ), q{Don't match externally inverted <?isInOgham>} );
ok(!( "\c[OGHAM SPACE MARK]" ~~ m/^<[A]-<?isInOgham>>$/ ), q{Don't match compound inverted <?isInOgham>} );
ok(!( "\c[OGHAM SPACE MARK]" ~~ m/^<+<-isInOgham>>$/ ), q{Don't match internally inverted <?isInOgham>} );
ok(!( "\x[2C8C]"  ~~ m/^<+<?isInOgham>>$/ ), q{Don't match unrelated <?isInOgham>} );
ok("\x[2C8C]"  ~~ m/^<-<?isInOgham>>$/, q{Match unrelated externally inverted <?isInOgham>} );
ok("\x[2C8C]"  ~~ m/^<+<-isInOgham>>$/, q{Match unrelated internally inverted <?isInOgham>} );
ok("\x[2C8C]\c[OGHAM SPACE MARK]" ~~ m/<+<?isInOgham>>/, q{Match unanchored <?isInOgham>} );

# InOldItalic


ok(!( "\c[YI SYLLABLE MGAT]"  ~~ m/^<+<?isInOldItalic>>$/ ), q{Don't match unrelated <?isInOldItalic>} );
ok("\c[YI SYLLABLE MGAT]"  ~~ m/^<-<?isInOldItalic>>$/, q{Match unrelated externally inverted <?isInOldItalic>} );
ok("\c[YI SYLLABLE MGAT]"  ~~ m/^<+<-isInOldItalic>>$/, q{Match unrelated internally inverted <?isInOldItalic>} );

# InOpticalCharacterRecognition


ok("\c[OCR HOOK]" ~~ m/^<+<?isInOpticalCharacterRecognition>>$/, q{Match <?isInOpticalCharacterRecognition>} );
ok("\c[OCR HOOK]" ~~ m/^<[A]+<?isInOpticalCharacterRecognition>>$/, q{Match compound <?isInOpticalCharacterRecognition>} );
ok(!( "\c[OCR HOOK]" ~~ m/^<-<?isInOpticalCharacterRecognition>>$/ ), q{Don't match externally inverted <?isInOpticalCharacterRecognition>} );
ok(!( "\c[OCR HOOK]" ~~ m/^<[A]-<?isInOpticalCharacterRecognition>>$/ ), q{Don't match compound inverted <?isInOpticalCharacterRecognition>} );
ok(!( "\c[OCR HOOK]" ~~ m/^<+<-isInOpticalCharacterRecognition>>$/ ), q{Don't match internally inverted <?isInOpticalCharacterRecognition>} );
ok(!( "\x[CB83]"  ~~ m/^<+<?isInOpticalCharacterRecognition>>$/ ), q{Don't match unrelated <?isInOpticalCharacterRecognition>} );
ok("\x[CB83]"  ~~ m/^<-<?isInOpticalCharacterRecognition>>$/, q{Match unrelated externally inverted <?isInOpticalCharacterRecognition>} );
ok("\x[CB83]"  ~~ m/^<+<-isInOpticalCharacterRecognition>>$/, q{Match unrelated internally inverted <?isInOpticalCharacterRecognition>} );
ok("\x[CB83]\c[OCR HOOK]" ~~ m/<+<?isInOpticalCharacterRecognition>>/, q{Match unanchored <?isInOpticalCharacterRecognition>} );

# InOriya


ok("\x[0B00]" ~~ m/^<+<?isInOriya>>$/, q{Match <?isInOriya>} );
ok("\x[0B00]" ~~ m/^<[A]+<?isInOriya>>$/, q{Match compound <?isInOriya>} );
ok(!( "\x[0B00]" ~~ m/^<-<?isInOriya>>$/ ), q{Don't match externally inverted <?isInOriya>} );
ok(!( "\x[0B00]" ~~ m/^<[A]-<?isInOriya>>$/ ), q{Don't match compound inverted <?isInOriya>} );
ok(!( "\x[0B00]" ~~ m/^<+<-isInOriya>>$/ ), q{Don't match internally inverted <?isInOriya>} );
ok(!( "\x[6CE7]"  ~~ m/^<+<?isInOriya>>$/ ), q{Don't match unrelated <?isInOriya>} );
ok("\x[6CE7]"  ~~ m/^<-<?isInOriya>>$/, q{Match unrelated externally inverted <?isInOriya>} );
ok("\x[6CE7]"  ~~ m/^<+<-isInOriya>>$/, q{Match unrelated internally inverted <?isInOriya>} );
ok("\x[6CE7]\x[0B00]" ~~ m/<+<?isInOriya>>/, q{Match unanchored <?isInOriya>} );

# InPrivateUseArea


ok(!( "\x[7662]"  ~~ m/^<+<?isInPrivateUseArea>>$/ ), q{Don't match unrelated <?isInPrivateUseArea>} );
ok("\x[7662]"  ~~ m/^<-<?isInPrivateUseArea>>$/, q{Match unrelated externally inverted <?isInPrivateUseArea>} );
ok("\x[7662]"  ~~ m/^<+<-isInPrivateUseArea>>$/, q{Match unrelated internally inverted <?isInPrivateUseArea>} );

# InRunic


ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<?isInRunic>>$/, q{Match <?isInRunic>} );
ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]+<?isInRunic>>$/, q{Match compound <?isInRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<-<?isInRunic>>$/ ), q{Don't match externally inverted <?isInRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]-<?isInRunic>>$/ ), q{Don't match compound inverted <?isInRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<-isInRunic>>$/ ), q{Don't match internally inverted <?isInRunic>} );
ok(!( "\x[47A1]"  ~~ m/^<+<?isInRunic>>$/ ), q{Don't match unrelated <?isInRunic>} );
ok("\x[47A1]"  ~~ m/^<-<?isInRunic>>$/, q{Match unrelated externally inverted <?isInRunic>} );
ok("\x[47A1]"  ~~ m/^<+<-isInRunic>>$/, q{Match unrelated internally inverted <?isInRunic>} );
ok("\x[47A1]\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/<+<?isInRunic>>/, q{Match unanchored <?isInRunic>} );

# InSinhala


ok("\x[0D80]" ~~ m/^<+<?isInSinhala>>$/, q{Match <?isInSinhala>} );
ok("\x[0D80]" ~~ m/^<[A]+<?isInSinhala>>$/, q{Match compound <?isInSinhala>} );
ok(!( "\x[0D80]" ~~ m/^<-<?isInSinhala>>$/ ), q{Don't match externally inverted <?isInSinhala>} );
ok(!( "\x[0D80]" ~~ m/^<[A]-<?isInSinhala>>$/ ), q{Don't match compound inverted <?isInSinhala>} );
ok(!( "\x[0D80]" ~~ m/^<+<-isInSinhala>>$/ ), q{Don't match internally inverted <?isInSinhala>} );
ok(!( "\x[1C39]"  ~~ m/^<+<?isInSinhala>>$/ ), q{Don't match unrelated <?isInSinhala>} );
ok("\x[1C39]"  ~~ m/^<-<?isInSinhala>>$/, q{Match unrelated externally inverted <?isInSinhala>} );
ok("\x[1C39]"  ~~ m/^<+<-isInSinhala>>$/, q{Match unrelated internally inverted <?isInSinhala>} );
ok("\x[1C39]\x[0D80]" ~~ m/<+<?isInSinhala>>/, q{Match unanchored <?isInSinhala>} );

# InSmallFormVariants


ok(!( "\c[YI SYLLABLE FAP]"  ~~ m/^<+<?isInSmallFormVariants>>$/ ), q{Don't match unrelated <?isInSmallFormVariants>} );
ok("\c[YI SYLLABLE FAP]"  ~~ m/^<-<?isInSmallFormVariants>>$/, q{Match unrelated externally inverted <?isInSmallFormVariants>} );
ok("\c[YI SYLLABLE FAP]"  ~~ m/^<+<-isInSmallFormVariants>>$/, q{Match unrelated internally inverted <?isInSmallFormVariants>} );

# InSpacingModifierLetters


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?isInSpacingModifierLetters>>$/, q{Match <?isInSpacingModifierLetters>} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?isInSpacingModifierLetters>>$/, q{Match compound <?isInSpacingModifierLetters>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?isInSpacingModifierLetters>>$/ ), q{Don't match externally inverted <?isInSpacingModifierLetters>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?isInSpacingModifierLetters>>$/ ), q{Don't match compound inverted <?isInSpacingModifierLetters>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-isInSpacingModifierLetters>>$/ ), q{Don't match internally inverted <?isInSpacingModifierLetters>} );
ok(!( "\x[08E8]"  ~~ m/^<+<?isInSpacingModifierLetters>>$/ ), q{Don't match unrelated <?isInSpacingModifierLetters>} );
ok("\x[08E8]"  ~~ m/^<-<?isInSpacingModifierLetters>>$/, q{Match unrelated externally inverted <?isInSpacingModifierLetters>} );
ok("\x[08E8]"  ~~ m/^<+<-isInSpacingModifierLetters>>$/, q{Match unrelated internally inverted <?isInSpacingModifierLetters>} );
ok("\x[08E8]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?isInSpacingModifierLetters>>/, q{Match unanchored <?isInSpacingModifierLetters>} );

# InSpecials


ok(!( "\x[0C7E]"  ~~ m/^<+<?isInSpecials>>$/ ), q{Don't match unrelated <?isInSpecials>} );
ok("\x[0C7E]"  ~~ m/^<-<?isInSpecials>>$/, q{Match unrelated externally inverted <?isInSpecials>} );
ok("\x[0C7E]"  ~~ m/^<+<-isInSpecials>>$/, q{Match unrelated internally inverted <?isInSpecials>} );

# InSuperscriptsAndSubscripts


ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<+<?isInSuperscriptsAndSubscripts>>$/, q{Match <?isInSuperscriptsAndSubscripts>} );
ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<[A]+<?isInSuperscriptsAndSubscripts>>$/, q{Match compound <?isInSuperscriptsAndSubscripts>} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<-<?isInSuperscriptsAndSubscripts>>$/ ), q{Don't match externally inverted <?isInSuperscriptsAndSubscripts>} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<[A]-<?isInSuperscriptsAndSubscripts>>$/ ), q{Don't match compound inverted <?isInSuperscriptsAndSubscripts>} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<+<-isInSuperscriptsAndSubscripts>>$/ ), q{Don't match internally inverted <?isInSuperscriptsAndSubscripts>} );
ok(!( "\x[D378]"  ~~ m/^<+<?isInSuperscriptsAndSubscripts>>$/ ), q{Don't match unrelated <?isInSuperscriptsAndSubscripts>} );
ok("\x[D378]"  ~~ m/^<-<?isInSuperscriptsAndSubscripts>>$/, q{Match unrelated externally inverted <?isInSuperscriptsAndSubscripts>} );
ok("\x[D378]"  ~~ m/^<+<-isInSuperscriptsAndSubscripts>>$/, q{Match unrelated internally inverted <?isInSuperscriptsAndSubscripts>} );
ok("\x[D378]\c[SUPERSCRIPT ZERO]" ~~ m/<+<?isInSuperscriptsAndSubscripts>>/, q{Match unanchored <?isInSuperscriptsAndSubscripts>} );

# InSupplementalArrowsA


ok("\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<+<?isInSupplementalArrowsA>>$/, q{Match <?isInSupplementalArrowsA>} );
ok("\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<[A]+<?isInSupplementalArrowsA>>$/, q{Match compound <?isInSupplementalArrowsA>} );
ok(!( "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<-<?isInSupplementalArrowsA>>$/ ), q{Don't match externally inverted <?isInSupplementalArrowsA>} );
ok(!( "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<[A]-<?isInSupplementalArrowsA>>$/ ), q{Don't match compound inverted <?isInSupplementalArrowsA>} );
ok(!( "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<+<-isInSupplementalArrowsA>>$/ ), q{Don't match internally inverted <?isInSupplementalArrowsA>} );
ok(!( "\c[LIMBU DIGIT SEVEN]"  ~~ m/^<+<?isInSupplementalArrowsA>>$/ ), q{Don't match unrelated <?isInSupplementalArrowsA>} );
ok("\c[LIMBU DIGIT SEVEN]"  ~~ m/^<-<?isInSupplementalArrowsA>>$/, q{Match unrelated externally inverted <?isInSupplementalArrowsA>} );
ok("\c[LIMBU DIGIT SEVEN]"  ~~ m/^<+<-isInSupplementalArrowsA>>$/, q{Match unrelated internally inverted <?isInSupplementalArrowsA>} );
ok("\c[LIMBU DIGIT SEVEN]\c[UPWARDS QUADRUPLE ARROW]" ~~ m/<+<?isInSupplementalArrowsA>>/, q{Match unanchored <?isInSupplementalArrowsA>} );

# InSupplementalArrowsB


ok("\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<+<?isInSupplementalArrowsB>>$/, q{Match <?isInSupplementalArrowsB>} );
ok("\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]+<?isInSupplementalArrowsB>>$/, q{Match compound <?isInSupplementalArrowsB>} );
ok(!( "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<-<?isInSupplementalArrowsB>>$/ ), q{Don't match externally inverted <?isInSupplementalArrowsB>} );
ok(!( "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]-<?isInSupplementalArrowsB>>$/ ), q{Don't match compound inverted <?isInSupplementalArrowsB>} );
ok(!( "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<+<-isInSupplementalArrowsB>>$/ ), q{Don't match internally inverted <?isInSupplementalArrowsB>} );
ok(!( "\x[1D7D]"  ~~ m/^<+<?isInSupplementalArrowsB>>$/ ), q{Don't match unrelated <?isInSupplementalArrowsB>} );
ok("\x[1D7D]"  ~~ m/^<-<?isInSupplementalArrowsB>>$/, q{Match unrelated externally inverted <?isInSupplementalArrowsB>} );
ok("\x[1D7D]"  ~~ m/^<+<-isInSupplementalArrowsB>>$/, q{Match unrelated internally inverted <?isInSupplementalArrowsB>} );
ok("\x[1D7D]\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/<+<?isInSupplementalArrowsB>>/, q{Match unanchored <?isInSupplementalArrowsB>} );

# InSupplementalMathematicalOperators


ok("\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<+<?isInSupplementalMathematicalOperators>>$/, q{Match <?isInSupplementalMathematicalOperators>} );
ok("\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<[A]+<?isInSupplementalMathematicalOperators>>$/, q{Match compound <?isInSupplementalMathematicalOperators>} );
ok(!( "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<-<?isInSupplementalMathematicalOperators>>$/ ), q{Don't match externally inverted <?isInSupplementalMathematicalOperators>} );
ok(!( "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<[A]-<?isInSupplementalMathematicalOperators>>$/ ), q{Don't match compound inverted <?isInSupplementalMathematicalOperators>} );
ok(!( "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<+<-isInSupplementalMathematicalOperators>>$/ ), q{Don't match internally inverted <?isInSupplementalMathematicalOperators>} );
ok(!( "\c[YI SYLLABLE TAX]"  ~~ m/^<+<?isInSupplementalMathematicalOperators>>$/ ), q{Don't match unrelated <?isInSupplementalMathematicalOperators>} );
ok("\c[YI SYLLABLE TAX]"  ~~ m/^<-<?isInSupplementalMathematicalOperators>>$/, q{Match unrelated externally inverted <?isInSupplementalMathematicalOperators>} );
ok("\c[YI SYLLABLE TAX]"  ~~ m/^<+<-isInSupplementalMathematicalOperators>>$/, q{Match unrelated internally inverted <?isInSupplementalMathematicalOperators>} );
ok("\c[YI SYLLABLE TAX]\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/<+<?isInSupplementalMathematicalOperators>>/, q{Match unanchored <?isInSupplementalMathematicalOperators>} );

# InSupplementaryPrivateUseAreaA


ok(!( "\c[TIBETAN LETTER PHA]"  ~~ m/^<+<?isInSupplementaryPrivateUseAreaA>>$/ ), q{Don't match unrelated <?isInSupplementaryPrivateUseAreaA>} );
ok("\c[TIBETAN LETTER PHA]"  ~~ m/^<-<?isInSupplementaryPrivateUseAreaA>>$/, q{Match unrelated externally inverted <?isInSupplementaryPrivateUseAreaA>} );
ok("\c[TIBETAN LETTER PHA]"  ~~ m/^<+<-isInSupplementaryPrivateUseAreaA>>$/, q{Match unrelated internally inverted <?isInSupplementaryPrivateUseAreaA>} );

# InSupplementaryPrivateUseAreaB


ok(!( "\x[7E65]"  ~~ m/^<+<?isInSupplementaryPrivateUseAreaB>>$/ ), q{Don't match unrelated <?isInSupplementaryPrivateUseAreaB>} );
ok("\x[7E65]"  ~~ m/^<-<?isInSupplementaryPrivateUseAreaB>>$/, q{Match unrelated externally inverted <?isInSupplementaryPrivateUseAreaB>} );
ok("\x[7E65]"  ~~ m/^<+<-isInSupplementaryPrivateUseAreaB>>$/, q{Match unrelated internally inverted <?isInSupplementaryPrivateUseAreaB>} );

# InSyriac


ok("\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<+<?isInSyriac>>$/, q{Match <?isInSyriac>} );
ok("\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<[A]+<?isInSyriac>>$/, q{Match compound <?isInSyriac>} );
ok(!( "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<-<?isInSyriac>>$/ ), q{Don't match externally inverted <?isInSyriac>} );
ok(!( "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<[A]-<?isInSyriac>>$/ ), q{Don't match compound inverted <?isInSyriac>} );
ok(!( "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<+<-isInSyriac>>$/ ), q{Don't match internally inverted <?isInSyriac>} );
ok(!( "\x[CA1C]"  ~~ m/^<+<?isInSyriac>>$/ ), q{Don't match unrelated <?isInSyriac>} );
ok("\x[CA1C]"  ~~ m/^<-<?isInSyriac>>$/, q{Match unrelated externally inverted <?isInSyriac>} );
ok("\x[CA1C]"  ~~ m/^<+<-isInSyriac>>$/, q{Match unrelated internally inverted <?isInSyriac>} );
ok("\x[CA1C]\c[SYRIAC END OF PARAGRAPH]" ~~ m/<+<?isInSyriac>>/, q{Match unanchored <?isInSyriac>} );

# InTagalog


ok("\c[TAGALOG LETTER A]" ~~ m/^<+<?isInTagalog>>$/, q{Match <?isInTagalog>} );
ok("\c[TAGALOG LETTER A]" ~~ m/^<[A]+<?isInTagalog>>$/, q{Match compound <?isInTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<-<?isInTagalog>>$/ ), q{Don't match externally inverted <?isInTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<[A]-<?isInTagalog>>$/ ), q{Don't match compound inverted <?isInTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<+<-isInTagalog>>$/ ), q{Don't match internally inverted <?isInTagalog>} );
ok(!( "\x[D49E]"  ~~ m/^<+<?isInTagalog>>$/ ), q{Don't match unrelated <?isInTagalog>} );
ok("\x[D49E]"  ~~ m/^<-<?isInTagalog>>$/, q{Match unrelated externally inverted <?isInTagalog>} );
ok("\x[D49E]"  ~~ m/^<+<-isInTagalog>>$/, q{Match unrelated internally inverted <?isInTagalog>} );
ok("\x[D49E]\c[TAGALOG LETTER A]" ~~ m/<+<?isInTagalog>>/, q{Match unanchored <?isInTagalog>} );

# InTagbanwa


ok("\c[TAGBANWA LETTER A]" ~~ m/^<+<?isInTagbanwa>>$/, q{Match <?isInTagbanwa>} );
ok("\c[TAGBANWA LETTER A]" ~~ m/^<[A]+<?isInTagbanwa>>$/, q{Match compound <?isInTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<-<?isInTagbanwa>>$/ ), q{Don't match externally inverted <?isInTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<[A]-<?isInTagbanwa>>$/ ), q{Don't match compound inverted <?isInTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<+<-isInTagbanwa>>$/ ), q{Don't match internally inverted <?isInTagbanwa>} );
ok(!( "\x[AFAA]"  ~~ m/^<+<?isInTagbanwa>>$/ ), q{Don't match unrelated <?isInTagbanwa>} );
ok("\x[AFAA]"  ~~ m/^<-<?isInTagbanwa>>$/, q{Match unrelated externally inverted <?isInTagbanwa>} );
ok("\x[AFAA]"  ~~ m/^<+<-isInTagbanwa>>$/, q{Match unrelated internally inverted <?isInTagbanwa>} );
ok("\x[AFAA]\c[TAGBANWA LETTER A]" ~~ m/<+<?isInTagbanwa>>/, q{Match unanchored <?isInTagbanwa>} );

# InTags


ok(!( "\x[CA38]"  ~~ m/^<+<?isInTags>>$/ ), q{Don't match unrelated <?isInTags>} );
ok("\x[CA38]"  ~~ m/^<-<?isInTags>>$/, q{Match unrelated externally inverted <?isInTags>} );
ok("\x[CA38]"  ~~ m/^<+<-isInTags>>$/, q{Match unrelated internally inverted <?isInTags>} );

# InTamil


ok("\x[0B80]" ~~ m/^<+<?isInTamil>>$/, q{Match <?isInTamil>} );
ok("\x[0B80]" ~~ m/^<[A]+<?isInTamil>>$/, q{Match compound <?isInTamil>} );
ok(!( "\x[0B80]" ~~ m/^<-<?isInTamil>>$/ ), q{Don't match externally inverted <?isInTamil>} );
ok(!( "\x[0B80]" ~~ m/^<[A]-<?isInTamil>>$/ ), q{Don't match compound inverted <?isInTamil>} );
ok(!( "\x[0B80]" ~~ m/^<+<-isInTamil>>$/ ), q{Don't match internally inverted <?isInTamil>} );
ok(!( "\x[D44B]"  ~~ m/^<+<?isInTamil>>$/ ), q{Don't match unrelated <?isInTamil>} );
ok("\x[D44B]"  ~~ m/^<-<?isInTamil>>$/, q{Match unrelated externally inverted <?isInTamil>} );
ok("\x[D44B]"  ~~ m/^<+<-isInTamil>>$/, q{Match unrelated internally inverted <?isInTamil>} );
ok("\x[D44B]\x[0B80]" ~~ m/<+<?isInTamil>>/, q{Match unanchored <?isInTamil>} );

# InTelugu


ok("\x[0C00]" ~~ m/^<+<?isInTelugu>>$/, q{Match <?isInTelugu>} );
ok("\x[0C00]" ~~ m/^<[A]+<?isInTelugu>>$/, q{Match compound <?isInTelugu>} );
ok(!( "\x[0C00]" ~~ m/^<-<?isInTelugu>>$/ ), q{Don't match externally inverted <?isInTelugu>} );
ok(!( "\x[0C00]" ~~ m/^<[A]-<?isInTelugu>>$/ ), q{Don't match compound inverted <?isInTelugu>} );
ok(!( "\x[0C00]" ~~ m/^<+<-isInTelugu>>$/ ), q{Don't match internally inverted <?isInTelugu>} );
ok(!( "\x[D3E7]"  ~~ m/^<+<?isInTelugu>>$/ ), q{Don't match unrelated <?isInTelugu>} );
ok("\x[D3E7]"  ~~ m/^<-<?isInTelugu>>$/, q{Match unrelated externally inverted <?isInTelugu>} );
ok("\x[D3E7]"  ~~ m/^<+<-isInTelugu>>$/, q{Match unrelated internally inverted <?isInTelugu>} );
ok("\x[D3E7]\x[0C00]" ~~ m/<+<?isInTelugu>>/, q{Match unanchored <?isInTelugu>} );

# InThaana


ok("\c[THAANA LETTER HAA]" ~~ m/^<+<?isInThaana>>$/, q{Match <?isInThaana>} );
ok("\c[THAANA LETTER HAA]" ~~ m/^<[A]+<?isInThaana>>$/, q{Match compound <?isInThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<-<?isInThaana>>$/ ), q{Don't match externally inverted <?isInThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<[A]-<?isInThaana>>$/ ), q{Don't match compound inverted <?isInThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<+<-isInThaana>>$/ ), q{Don't match internally inverted <?isInThaana>} );
ok(!( "\c[YI SYLLABLE QIT]"  ~~ m/^<+<?isInThaana>>$/ ), q{Don't match unrelated <?isInThaana>} );
ok("\c[YI SYLLABLE QIT]"  ~~ m/^<-<?isInThaana>>$/, q{Match unrelated externally inverted <?isInThaana>} );
ok("\c[YI SYLLABLE QIT]"  ~~ m/^<+<-isInThaana>>$/, q{Match unrelated internally inverted <?isInThaana>} );
ok("\c[YI SYLLABLE QIT]\c[THAANA LETTER HAA]" ~~ m/<+<?isInThaana>>/, q{Match unanchored <?isInThaana>} );

# InThai


ok("\x[0E00]" ~~ m/^<+<?isInThai>>$/, q{Match <?isInThai>} );
ok("\x[0E00]" ~~ m/^<[A]+<?isInThai>>$/, q{Match compound <?isInThai>} );
ok(!( "\x[0E00]" ~~ m/^<-<?isInThai>>$/ ), q{Don't match externally inverted <?isInThai>} );
ok(!( "\x[0E00]" ~~ m/^<[A]-<?isInThai>>$/ ), q{Don't match compound inverted <?isInThai>} );
ok(!( "\x[0E00]" ~~ m/^<+<-isInThai>>$/ ), q{Don't match internally inverted <?isInThai>} );
ok(!( "\x[BCED]"  ~~ m/^<+<?isInThai>>$/ ), q{Don't match unrelated <?isInThai>} );
ok("\x[BCED]"  ~~ m/^<-<?isInThai>>$/, q{Match unrelated externally inverted <?isInThai>} );
ok("\x[BCED]"  ~~ m/^<+<-isInThai>>$/, q{Match unrelated internally inverted <?isInThai>} );
ok("\x[BCED]\x[0E00]" ~~ m/<+<?isInThai>>/, q{Match unanchored <?isInThai>} );

# InTibetan


ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<?isInTibetan>>$/, q{Match <?isInTibetan>} );
ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]+<?isInTibetan>>$/, q{Match compound <?isInTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<-<?isInTibetan>>$/ ), q{Don't match externally inverted <?isInTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]-<?isInTibetan>>$/ ), q{Don't match compound inverted <?isInTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<-isInTibetan>>$/ ), q{Don't match internally inverted <?isInTibetan>} );
ok(!( "\c[ARABIC SIGN SINDHI AMPERSAND]"  ~~ m/^<+<?isInTibetan>>$/ ), q{Don't match unrelated <?isInTibetan>} );
ok("\c[ARABIC SIGN SINDHI AMPERSAND]"  ~~ m/^<-<?isInTibetan>>$/, q{Match unrelated externally inverted <?isInTibetan>} );
ok("\c[ARABIC SIGN SINDHI AMPERSAND]"  ~~ m/^<+<-isInTibetan>>$/, q{Match unrelated internally inverted <?isInTibetan>} );
ok("\c[ARABIC SIGN SINDHI AMPERSAND]\c[TIBETAN SYLLABLE OM]" ~~ m/<+<?isInTibetan>>/, q{Match unanchored <?isInTibetan>} );

# InUnifiedCanadianAboriginalSyllabics


ok("\x[1400]" ~~ m/^<+<?isInUnifiedCanadianAboriginalSyllabics>>$/, q{Match <?isInUnifiedCanadianAboriginalSyllabics>} );
ok("\x[1400]" ~~ m/^<[A]+<?isInUnifiedCanadianAboriginalSyllabics>>$/, q{Match compound <?isInUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[1400]" ~~ m/^<-<?isInUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match externally inverted <?isInUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[1400]" ~~ m/^<[A]-<?isInUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match compound inverted <?isInUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[1400]" ~~ m/^<+<-isInUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match internally inverted <?isInUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[49D8]"  ~~ m/^<+<?isInUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match unrelated <?isInUnifiedCanadianAboriginalSyllabics>} );
ok("\x[49D8]"  ~~ m/^<-<?isInUnifiedCanadianAboriginalSyllabics>>$/, q{Match unrelated externally inverted <?isInUnifiedCanadianAboriginalSyllabics>} );
ok("\x[49D8]"  ~~ m/^<+<-isInUnifiedCanadianAboriginalSyllabics>>$/, q{Match unrelated internally inverted <?isInUnifiedCanadianAboriginalSyllabics>} );
ok("\x[49D8]\x[1400]" ~~ m/<+<?isInUnifiedCanadianAboriginalSyllabics>>/, q{Match unanchored <?isInUnifiedCanadianAboriginalSyllabics>} );

# InVariationSelectors


ok(!( "\x[5307]"  ~~ m/^<+<?isInVariationSelectors>>$/ ), q{Don't match unrelated <?isInVariationSelectors>} );
ok("\x[5307]"  ~~ m/^<-<?isInVariationSelectors>>$/, q{Match unrelated externally inverted <?isInVariationSelectors>} );
ok("\x[5307]"  ~~ m/^<+<-isInVariationSelectors>>$/, q{Match unrelated internally inverted <?isInVariationSelectors>} );

# InYiRadicals


ok("\c[YI RADICAL QOT]" ~~ m/^<+<?isInYiRadicals>>$/, q{Match <?isInYiRadicals>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<[A]+<?isInYiRadicals>>$/, q{Match compound <?isInYiRadicals>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<-<?isInYiRadicals>>$/ ), q{Don't match externally inverted <?isInYiRadicals>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<[A]-<?isInYiRadicals>>$/ ), q{Don't match compound inverted <?isInYiRadicals>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<-isInYiRadicals>>$/ ), q{Don't match internally inverted <?isInYiRadicals>} );
ok(!( "\x[7CAD]"  ~~ m/^<+<?isInYiRadicals>>$/ ), q{Don't match unrelated <?isInYiRadicals>} );
ok("\x[7CAD]"  ~~ m/^<-<?isInYiRadicals>>$/, q{Match unrelated externally inverted <?isInYiRadicals>} );
ok("\x[7CAD]"  ~~ m/^<+<-isInYiRadicals>>$/, q{Match unrelated internally inverted <?isInYiRadicals>} );
ok("\x[7CAD]\c[YI RADICAL QOT]" ~~ m/<+<?isInYiRadicals>>/, q{Match unanchored <?isInYiRadicals>} );

# InYiSyllables


ok("\c[YI SYLLABLE IT]" ~~ m/^<+<?isInYiSyllables>>$/, q{Match <?isInYiSyllables>} );
ok("\c[YI SYLLABLE IT]" ~~ m/^<[A]+<?isInYiSyllables>>$/, q{Match compound <?isInYiSyllables>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<-<?isInYiSyllables>>$/ ), q{Don't match externally inverted <?isInYiSyllables>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<[A]-<?isInYiSyllables>>$/ ), q{Don't match compound inverted <?isInYiSyllables>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<+<-isInYiSyllables>>$/ ), q{Don't match internally inverted <?isInYiSyllables>} );
ok(!( "\c[BRAILLE PATTERN DOTS-1578]"  ~~ m/^<+<?isInYiSyllables>>$/ ), q{Don't match unrelated <?isInYiSyllables>} );
ok("\c[BRAILLE PATTERN DOTS-1578]"  ~~ m/^<-<?isInYiSyllables>>$/, q{Match unrelated externally inverted <?isInYiSyllables>} );
ok("\c[BRAILLE PATTERN DOTS-1578]"  ~~ m/^<+<-isInYiSyllables>>$/, q{Match unrelated internally inverted <?isInYiSyllables>} );
ok("\c[BRAILLE PATTERN DOTS-1578]\c[YI SYLLABLE IT]" ~~ m/<+<?isInYiSyllables>>/, q{Match unanchored <?isInYiSyllables>} );


}

