#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/properties_slow_to_compile.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 1881;

# L           Letter


ok("\x[846D]" ~~ m/^<L>$/, q{Match <L> (Letter)} );
ok(not "\x[846D]" ~~ m/^<!L>.$/, q{Don't match negated <L> (Letter)} );
ok(not "\x[846D]" ~~ m/^<-L>$/, q{Don't match inverted <L> (Letter)} );
ok(not "\x[9FA6]"  ~~ m/^<L>$/, q{Don't match unrelated <L> (Letter)} );
ok("\x[9FA6]"  ~~ m/^<!L>.$/, q{Match unrelated negated <L> (Letter)} );
ok("\x[9FA6]"  ~~ m/^<-L>$/, q{Match unrelated inverted <L> (Letter)} );
ok("\x[9FA6]\x[846D]" ~~ m/<L>/, q{Match unanchored <L> (Letter)} );

ok("\x[6DF7]" ~~ m/^<?Letter>$/, q{Match <?Letter>} );
ok(not "\x[6DF7]" ~~ m/^<!Letter>.$/, q{Don't match negated <?Letter>} );
ok(not "\x[6DF7]" ~~ m/^<-Letter>$/, q{Don't match inverted <?Letter>} );
ok(not "\x[9FA6]"  ~~ m/^<?Letter>$/, q{Don't match unrelated <?Letter>} );
ok("\x[9FA6]"  ~~ m/^<!Letter>.$/, q{Match unrelated negated <?Letter>} );
ok("\x[9FA6]"  ~~ m/^<-Letter>$/, q{Match unrelated inverted <?Letter>} );
ok("\x[9FA6]\x[6DF7]" ~~ m/<?Letter>/, q{Match unanchored <?Letter>} );

# Lu          UppercaseLetter


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<?Lu>$/, q{Match <?Lu> (UppercaseLetter)} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<!Lu>.$/, q{Don't match negated <?Lu> (UppercaseLetter)} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-Lu>$/, q{Don't match inverted <?Lu> (UppercaseLetter)} );
ok(not "\x[C767]"  ~~ m/^<?Lu>$/, q{Don't match unrelated <?Lu> (UppercaseLetter)} );
ok("\x[C767]"  ~~ m/^<!Lu>.$/, q{Match unrelated negated <?Lu> (UppercaseLetter)} );
ok("\x[C767]"  ~~ m/^<-Lu>$/, q{Match unrelated inverted <?Lu> (UppercaseLetter)} );
ok(not "\x[C767]" ~~ m/^<?Lu>$/, q{Don't match related <?Lu> (UppercaseLetter)} );
ok("\x[C767]" ~~ m/^<!Lu>.$/, q{Match related negated <?Lu> (UppercaseLetter)} );
ok("\x[C767]" ~~ m/^<-Lu>$/, q{Match related inverted <?Lu> (UppercaseLetter)} );
ok("\x[C767]\x[C767]\c[LATIN CAPITAL LETTER A]" ~~ m/<?Lu>/, q{Match unanchored <?Lu> (UppercaseLetter)} );

ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<?UppercaseLetter>$/, q{Match <?UppercaseLetter>} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<!UppercaseLetter>.$/, q{Don't match negated <?UppercaseLetter>} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-UppercaseLetter>$/, q{Don't match inverted <?UppercaseLetter>} );
ok(not "\c[YI SYLLABLE NBA]"  ~~ m/^<?UppercaseLetter>$/, q{Don't match unrelated <?UppercaseLetter>} );
ok("\c[YI SYLLABLE NBA]"  ~~ m/^<!UppercaseLetter>.$/, q{Match unrelated negated <?UppercaseLetter>} );
ok("\c[YI SYLLABLE NBA]"  ~~ m/^<-UppercaseLetter>$/, q{Match unrelated inverted <?UppercaseLetter>} );
ok("\c[YI SYLLABLE NBA]\c[LATIN CAPITAL LETTER A]" ~~ m/<?UppercaseLetter>/, q{Match unanchored <?UppercaseLetter>} );

# Ll          LowercaseLetter


ok("\c[LATIN SMALL LETTER A]" ~~ m/^<?Ll>$/, q{Match <?Ll> (LowercaseLetter)} );
ok(not "\c[LATIN SMALL LETTER A]" ~~ m/^<!Ll>.$/, q{Don't match negated <?Ll> (LowercaseLetter)} );
ok(not "\c[LATIN SMALL LETTER A]" ~~ m/^<-Ll>$/, q{Don't match inverted <?Ll> (LowercaseLetter)} );
ok(not "\c[BOPOMOFO FINAL LETTER H]"  ~~ m/^<?Ll>$/, q{Don't match unrelated <?Ll> (LowercaseLetter)} );
ok("\c[BOPOMOFO FINAL LETTER H]"  ~~ m/^<!Ll>.$/, q{Match unrelated negated <?Ll> (LowercaseLetter)} );
ok("\c[BOPOMOFO FINAL LETTER H]"  ~~ m/^<-Ll>$/, q{Match unrelated inverted <?Ll> (LowercaseLetter)} );
ok(not "\c[BOPOMOFO FINAL LETTER H]" ~~ m/^<?Ll>$/, q{Don't match related <?Ll> (LowercaseLetter)} );
ok("\c[BOPOMOFO FINAL LETTER H]" ~~ m/^<!Ll>.$/, q{Match related negated <?Ll> (LowercaseLetter)} );
ok("\c[BOPOMOFO FINAL LETTER H]" ~~ m/^<-Ll>$/, q{Match related inverted <?Ll> (LowercaseLetter)} );
ok("\c[BOPOMOFO FINAL LETTER H]\c[BOPOMOFO FINAL LETTER H]\c[LATIN SMALL LETTER A]" ~~ m/<?Ll>/, q{Match unanchored <?Ll> (LowercaseLetter)} );

ok("\c[LATIN SMALL LETTER A]" ~~ m/^<?LowercaseLetter>$/, q{Match <?LowercaseLetter>} );
ok(not "\c[LATIN SMALL LETTER A]" ~~ m/^<!LowercaseLetter>.$/, q{Don't match negated <?LowercaseLetter>} );
ok(not "\c[LATIN SMALL LETTER A]" ~~ m/^<-LowercaseLetter>$/, q{Don't match inverted <?LowercaseLetter>} );
ok(not "\x[86CA]"  ~~ m/^<?LowercaseLetter>$/, q{Don't match unrelated <?LowercaseLetter>} );
ok("\x[86CA]"  ~~ m/^<!LowercaseLetter>.$/, q{Match unrelated negated <?LowercaseLetter>} );
ok("\x[86CA]"  ~~ m/^<-LowercaseLetter>$/, q{Match unrelated inverted <?LowercaseLetter>} );
ok(not "\x[86CA]" ~~ m/^<?LowercaseLetter>$/, q{Don't match related <?LowercaseLetter>} );
ok("\x[86CA]" ~~ m/^<!LowercaseLetter>.$/, q{Match related negated <?LowercaseLetter>} );
ok("\x[86CA]" ~~ m/^<-LowercaseLetter>$/, q{Match related inverted <?LowercaseLetter>} );
ok("\x[86CA]\x[86CA]\c[LATIN SMALL LETTER A]" ~~ m/<?LowercaseLetter>/, q{Match unanchored <?LowercaseLetter>} );

# Lt          TitlecaseLetter


ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<?Lt>$/, q{Match <?Lt> (TitlecaseLetter)} );
ok(not "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<!Lt>.$/, q{Don't match negated <?Lt> (TitlecaseLetter)} );
ok(not "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<-Lt>$/, q{Don't match inverted <?Lt> (TitlecaseLetter)} );
ok(not "\x[6DC8]"  ~~ m/^<?Lt>$/, q{Don't match unrelated <?Lt> (TitlecaseLetter)} );
ok("\x[6DC8]"  ~~ m/^<!Lt>.$/, q{Match unrelated negated <?Lt> (TitlecaseLetter)} );
ok("\x[6DC8]"  ~~ m/^<-Lt>$/, q{Match unrelated inverted <?Lt> (TitlecaseLetter)} );
ok(not "\x[6DC8]" ~~ m/^<?Lt>$/, q{Don't match related <?Lt> (TitlecaseLetter)} );
ok("\x[6DC8]" ~~ m/^<!Lt>.$/, q{Match related negated <?Lt> (TitlecaseLetter)} );
ok("\x[6DC8]" ~~ m/^<-Lt>$/, q{Match related inverted <?Lt> (TitlecaseLetter)} );
ok("\x[6DC8]\x[6DC8]\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/<?Lt>/, q{Match unanchored <?Lt> (TitlecaseLetter)} );

ok("\c[GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI]" ~~ m/^<?TitlecaseLetter>$/, q{Match <?TitlecaseLetter>} );
ok(not "\c[GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI]" ~~ m/^<!TitlecaseLetter>.$/, q{Don't match negated <?TitlecaseLetter>} );
ok(not "\c[GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI]" ~~ m/^<-TitlecaseLetter>$/, q{Don't match inverted <?TitlecaseLetter>} );
ok(not "\x[0C4E]"  ~~ m/^<?TitlecaseLetter>$/, q{Don't match unrelated <?TitlecaseLetter>} );
ok("\x[0C4E]"  ~~ m/^<!TitlecaseLetter>.$/, q{Match unrelated negated <?TitlecaseLetter>} );
ok("\x[0C4E]"  ~~ m/^<-TitlecaseLetter>$/, q{Match unrelated inverted <?TitlecaseLetter>} );
ok("\x[0C4E]\c[GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI]" ~~ m/<?TitlecaseLetter>/, q{Match unanchored <?TitlecaseLetter>} );

# Lm          ModifierLetter


ok("\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<?Lm>$/, q{Match <?Lm> (ModifierLetter)} );
ok(not "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<!Lm>.$/, q{Don't match negated <?Lm> (ModifierLetter)} );
ok(not "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<-Lm>$/, q{Don't match inverted <?Lm> (ModifierLetter)} );
ok(not "\x[2B61]"  ~~ m/^<?Lm>$/, q{Don't match unrelated <?Lm> (ModifierLetter)} );
ok("\x[2B61]"  ~~ m/^<!Lm>.$/, q{Match unrelated negated <?Lm> (ModifierLetter)} );
ok("\x[2B61]"  ~~ m/^<-Lm>$/, q{Match unrelated inverted <?Lm> (ModifierLetter)} );
ok(not "\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<?Lm>$/, q{Don't match related <?Lm> (ModifierLetter)} );
ok("\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<!Lm>.$/, q{Match related negated <?Lm> (ModifierLetter)} );
ok("\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<-Lm>$/, q{Match related inverted <?Lm> (ModifierLetter)} );
ok("\x[2B61]\c[IDEOGRAPHIC CLOSING MARK]\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/<?Lm>/, q{Match unanchored <?Lm> (ModifierLetter)} );

ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<?ModifierLetter>$/, q{Match <?ModifierLetter>} );
ok(not "\c[MODIFIER LETTER SMALL H]" ~~ m/^<!ModifierLetter>.$/, q{Don't match negated <?ModifierLetter>} );
ok(not "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-ModifierLetter>$/, q{Don't match inverted <?ModifierLetter>} );
ok(not "\c[YI SYLLABLE HA]"  ~~ m/^<?ModifierLetter>$/, q{Don't match unrelated <?ModifierLetter>} );
ok("\c[YI SYLLABLE HA]"  ~~ m/^<!ModifierLetter>.$/, q{Match unrelated negated <?ModifierLetter>} );
ok("\c[YI SYLLABLE HA]"  ~~ m/^<-ModifierLetter>$/, q{Match unrelated inverted <?ModifierLetter>} );
ok("\c[YI SYLLABLE HA]\c[MODIFIER LETTER SMALL H]" ~~ m/<?ModifierLetter>/, q{Match unanchored <?ModifierLetter>} );

# Lo          OtherLetter


ok("\c[LATIN LETTER TWO WITH STROKE]" ~~ m/^<?Lo>$/, q{Match <?Lo> (OtherLetter)} );
ok(not "\c[LATIN LETTER TWO WITH STROKE]" ~~ m/^<!Lo>.$/, q{Don't match negated <?Lo> (OtherLetter)} );
ok(not "\c[LATIN LETTER TWO WITH STROKE]" ~~ m/^<-Lo>$/, q{Don't match inverted <?Lo> (OtherLetter)} );
ok(not "\c[LATIN SMALL LETTER TURNED DELTA]"  ~~ m/^<?Lo>$/, q{Don't match unrelated <?Lo> (OtherLetter)} );
ok("\c[LATIN SMALL LETTER TURNED DELTA]"  ~~ m/^<!Lo>.$/, q{Match unrelated negated <?Lo> (OtherLetter)} );
ok("\c[LATIN SMALL LETTER TURNED DELTA]"  ~~ m/^<-Lo>$/, q{Match unrelated inverted <?Lo> (OtherLetter)} );
ok(not "\c[LATIN SMALL LETTER TURNED DELTA]" ~~ m/^<?Lo>$/, q{Don't match related <?Lo> (OtherLetter)} );
ok("\c[LATIN SMALL LETTER TURNED DELTA]" ~~ m/^<!Lo>.$/, q{Match related negated <?Lo> (OtherLetter)} );
ok("\c[LATIN SMALL LETTER TURNED DELTA]" ~~ m/^<-Lo>$/, q{Match related inverted <?Lo> (OtherLetter)} );
ok("\c[LATIN SMALL LETTER TURNED DELTA]\c[LATIN SMALL LETTER TURNED DELTA]\c[LATIN LETTER TWO WITH STROKE]" ~~ m/<?Lo>/, q{Match unanchored <?Lo> (OtherLetter)} );

ok("\c[ETHIOPIC SYLLABLE GLOTTAL A]" ~~ m/^<?OtherLetter>$/, q{Match <?OtherLetter>} );
ok(not "\c[ETHIOPIC SYLLABLE GLOTTAL A]" ~~ m/^<!OtherLetter>.$/, q{Don't match negated <?OtherLetter>} );
ok(not "\c[ETHIOPIC SYLLABLE GLOTTAL A]" ~~ m/^<-OtherLetter>$/, q{Don't match inverted <?OtherLetter>} );
ok(not "\x[12AF]"  ~~ m/^<?OtherLetter>$/, q{Don't match unrelated <?OtherLetter>} );
ok("\x[12AF]"  ~~ m/^<!OtherLetter>.$/, q{Match unrelated negated <?OtherLetter>} );
ok("\x[12AF]"  ~~ m/^<-OtherLetter>$/, q{Match unrelated inverted <?OtherLetter>} );
ok("\x[12AF]\c[ETHIOPIC SYLLABLE GLOTTAL A]" ~~ m/<?OtherLetter>/, q{Match unanchored <?OtherLetter>} );

# Lr		 	# Alias for "Ll", "Lu", and "Lt".


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<?Lr>$/, q{Match (Alias for "Ll", "Lu", and "Lt".)} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<!Lr>.$/, q{Don't match negated (Alias for "Ll", "Lu", and "Lt".)} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-Lr>$/, q{Don't match inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(not "\x[87B5]"  ~~ m/^<?Lr>$/, q{Don't match unrelated (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[87B5]"  ~~ m/^<!Lr>.$/, q{Match unrelated negated (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[87B5]"  ~~ m/^<-Lr>$/, q{Match unrelated inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(not "\x[87B5]" ~~ m/^<?Lr>$/, q{Don't match related (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[87B5]" ~~ m/^<!Lr>.$/, q{Match related negated (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[87B5]" ~~ m/^<-Lr>$/, q{Match related inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[87B5]\x[87B5]\c[LATIN CAPITAL LETTER A]" ~~ m/<?Lr>/, q{Match unanchored (Alias for "Ll", "Lu", and "Lt".)} );

# M           Mark


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<M>$/, q{Match <M> (Mark)} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<!M>.$/, q{Don't match negated <M> (Mark)} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-M>$/, q{Don't match inverted <M> (Mark)} );
ok(not "\x[D0AA]"  ~~ m/^<M>$/, q{Don't match unrelated <M> (Mark)} );
ok("\x[D0AA]"  ~~ m/^<!M>.$/, q{Match unrelated negated <M> (Mark)} );
ok("\x[D0AA]"  ~~ m/^<-M>$/, q{Match unrelated inverted <M> (Mark)} );
ok("\x[D0AA]\c[COMBINING GRAVE ACCENT]" ~~ m/<M>/, q{Match unanchored <M> (Mark)} );

ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Mark>$/, q{Match <?Mark>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Mark>.$/, q{Don't match negated <?Mark>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Mark>$/, q{Don't match inverted <?Mark>} );
ok(not "\x[BE64]"  ~~ m/^<?Mark>$/, q{Don't match unrelated <?Mark>} );
ok("\x[BE64]"  ~~ m/^<!Mark>.$/, q{Match unrelated negated <?Mark>} );
ok("\x[BE64]"  ~~ m/^<-Mark>$/, q{Match unrelated inverted <?Mark>} );
ok("\x[BE64]\c[COMBINING GRAVE ACCENT]" ~~ m/<?Mark>/, q{Match unanchored <?Mark>} );

# Mn          NonspacingMark


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Mn>$/, q{Match <?Mn> (NonspacingMark)} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Mn>.$/, q{Don't match negated <?Mn> (NonspacingMark)} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Mn>$/, q{Don't match inverted <?Mn> (NonspacingMark)} );
ok(not "\x[47A5]"  ~~ m/^<?Mn>$/, q{Don't match unrelated <?Mn> (NonspacingMark)} );
ok("\x[47A5]"  ~~ m/^<!Mn>.$/, q{Match unrelated negated <?Mn> (NonspacingMark)} );
ok("\x[47A5]"  ~~ m/^<-Mn>$/, q{Match unrelated inverted <?Mn> (NonspacingMark)} );
ok(not "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<?Mn>$/, q{Don't match related <?Mn> (NonspacingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<!Mn>.$/, q{Match related negated <?Mn> (NonspacingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-Mn>$/, q{Match related inverted <?Mn> (NonspacingMark)} );
ok("\x[47A5]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]\c[COMBINING GRAVE ACCENT]" ~~ m/<?Mn>/, q{Match unanchored <?Mn> (NonspacingMark)} );

ok("\c[TAGALOG VOWEL SIGN I]" ~~ m/^<?NonspacingMark>$/, q{Match <?NonspacingMark>} );
ok(not "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<!NonspacingMark>.$/, q{Don't match negated <?NonspacingMark>} );
ok(not "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<-NonspacingMark>$/, q{Don't match inverted <?NonspacingMark>} );
ok(not "\c[CANADIAN SYLLABICS TYA]"  ~~ m/^<?NonspacingMark>$/, q{Don't match unrelated <?NonspacingMark>} );
ok("\c[CANADIAN SYLLABICS TYA]"  ~~ m/^<!NonspacingMark>.$/, q{Match unrelated negated <?NonspacingMark>} );
ok("\c[CANADIAN SYLLABICS TYA]"  ~~ m/^<-NonspacingMark>$/, q{Match unrelated inverted <?NonspacingMark>} );
ok("\c[CANADIAN SYLLABICS TYA]\c[TAGALOG VOWEL SIGN I]" ~~ m/<?NonspacingMark>/, q{Match unanchored <?NonspacingMark>} );

# Mc          SpacingMark


ok("\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<?Mc>$/, q{Match <?Mc> (SpacingMark)} );
ok(not "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<!Mc>.$/, q{Don't match negated <?Mc> (SpacingMark)} );
ok(not "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<-Mc>$/, q{Don't match inverted <?Mc> (SpacingMark)} );
ok(not "\x[9981]"  ~~ m/^<?Mc>$/, q{Don't match unrelated <?Mc> (SpacingMark)} );
ok("\x[9981]"  ~~ m/^<!Mc>.$/, q{Match unrelated negated <?Mc> (SpacingMark)} );
ok("\x[9981]"  ~~ m/^<-Mc>$/, q{Match unrelated inverted <?Mc> (SpacingMark)} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Mc>$/, q{Don't match related <?Mc> (SpacingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Mc>.$/, q{Match related negated <?Mc> (SpacingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Mc>$/, q{Match related inverted <?Mc> (SpacingMark)} );
ok("\x[9981]\c[COMBINING GRAVE ACCENT]\c[DEVANAGARI SIGN VISARGA]" ~~ m/<?Mc>/, q{Match unanchored <?Mc> (SpacingMark)} );

ok("\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<?SpacingMark>$/, q{Match <?SpacingMark>} );
ok(not "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<!SpacingMark>.$/, q{Don't match negated <?SpacingMark>} );
ok(not "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<-SpacingMark>$/, q{Don't match inverted <?SpacingMark>} );
ok(not "\x[35E3]"  ~~ m/^<?SpacingMark>$/, q{Don't match unrelated <?SpacingMark>} );
ok("\x[35E3]"  ~~ m/^<!SpacingMark>.$/, q{Match unrelated negated <?SpacingMark>} );
ok("\x[35E3]"  ~~ m/^<-SpacingMark>$/, q{Match unrelated inverted <?SpacingMark>} );
ok("\x[35E3]\c[DEVANAGARI SIGN VISARGA]" ~~ m/<?SpacingMark>/, q{Match unanchored <?SpacingMark>} );

# Me          EnclosingMark


ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<?Me>$/, q{Match <?Me> (EnclosingMark)} );
ok(not "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<!Me>.$/, q{Don't match negated <?Me> (EnclosingMark)} );
ok(not "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-Me>$/, q{Don't match inverted <?Me> (EnclosingMark)} );
ok(not "\x[9400]"  ~~ m/^<?Me>$/, q{Don't match unrelated <?Me> (EnclosingMark)} );
ok("\x[9400]"  ~~ m/^<!Me>.$/, q{Match unrelated negated <?Me> (EnclosingMark)} );
ok("\x[9400]"  ~~ m/^<-Me>$/, q{Match unrelated inverted <?Me> (EnclosingMark)} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Me>$/, q{Don't match related <?Me> (EnclosingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Me>.$/, q{Match related negated <?Me> (EnclosingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Me>$/, q{Match related inverted <?Me> (EnclosingMark)} );
ok("\x[9400]\c[COMBINING GRAVE ACCENT]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/<?Me>/, q{Match unanchored <?Me> (EnclosingMark)} );

ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<?EnclosingMark>$/, q{Match <?EnclosingMark>} );
ok(not "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<!EnclosingMark>.$/, q{Don't match negated <?EnclosingMark>} );
ok(not "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-EnclosingMark>$/, q{Don't match inverted <?EnclosingMark>} );
ok(not "\x[7C68]"  ~~ m/^<?EnclosingMark>$/, q{Don't match unrelated <?EnclosingMark>} );
ok("\x[7C68]"  ~~ m/^<!EnclosingMark>.$/, q{Match unrelated negated <?EnclosingMark>} );
ok("\x[7C68]"  ~~ m/^<-EnclosingMark>$/, q{Match unrelated inverted <?EnclosingMark>} );
ok("\x[7C68]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/<?EnclosingMark>/, q{Match unanchored <?EnclosingMark>} );

# N           Number


ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<N>$/, q{Match <N> (Number)} );
ok(not "\c[SUPERSCRIPT ZERO]" ~~ m/^<!N>.$/, q{Don't match negated <N> (Number)} );
ok(not "\c[SUPERSCRIPT ZERO]" ~~ m/^<-N>$/, q{Don't match inverted <N> (Number)} );
ok(not "\c[LATIN LETTER SMALL CAPITAL E]"  ~~ m/^<N>$/, q{Don't match unrelated <N> (Number)} );
ok("\c[LATIN LETTER SMALL CAPITAL E]"  ~~ m/^<!N>.$/, q{Match unrelated negated <N> (Number)} );
ok("\c[LATIN LETTER SMALL CAPITAL E]"  ~~ m/^<-N>$/, q{Match unrelated inverted <N> (Number)} );
ok("\c[LATIN LETTER SMALL CAPITAL E]\c[SUPERSCRIPT ZERO]" ~~ m/<N>/, q{Match unanchored <N> (Number)} );

ok("\c[DIGIT ZERO]" ~~ m/^<?Number>$/, q{Match <?Number>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<!Number>.$/, q{Don't match negated <?Number>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<-Number>$/, q{Don't match inverted <?Number>} );
ok(not "\x[A994]"  ~~ m/^<?Number>$/, q{Don't match unrelated <?Number>} );
ok("\x[A994]"  ~~ m/^<!Number>.$/, q{Match unrelated negated <?Number>} );
ok("\x[A994]"  ~~ m/^<-Number>$/, q{Match unrelated inverted <?Number>} );
ok("\x[A994]\c[DIGIT ZERO]" ~~ m/<?Number>/, q{Match unanchored <?Number>} );

# Nd          DecimalNumber


ok("\c[DIGIT ZERO]" ~~ m/^<?Nd>$/, q{Match <?Nd> (DecimalNumber)} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<!Nd>.$/, q{Don't match negated <?Nd> (DecimalNumber)} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<-Nd>$/, q{Don't match inverted <?Nd> (DecimalNumber)} );
ok(not "\x[4E2C]"  ~~ m/^<?Nd>$/, q{Don't match unrelated <?Nd> (DecimalNumber)} );
ok("\x[4E2C]"  ~~ m/^<!Nd>.$/, q{Match unrelated negated <?Nd> (DecimalNumber)} );
ok("\x[4E2C]"  ~~ m/^<-Nd>$/, q{Match unrelated inverted <?Nd> (DecimalNumber)} );
ok(not "\c[SUPERSCRIPT TWO]" ~~ m/^<?Nd>$/, q{Don't match related <?Nd> (DecimalNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<!Nd>.$/, q{Match related negated <?Nd> (DecimalNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<-Nd>$/, q{Match related inverted <?Nd> (DecimalNumber)} );
ok("\x[4E2C]\c[SUPERSCRIPT TWO]\c[DIGIT ZERO]" ~~ m/<?Nd>/, q{Match unanchored <?Nd> (DecimalNumber)} );

ok("\c[DIGIT ZERO]" ~~ m/^<?DecimalNumber>$/, q{Match <?DecimalNumber>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<!DecimalNumber>.$/, q{Don't match negated <?DecimalNumber>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<-DecimalNumber>$/, q{Don't match inverted <?DecimalNumber>} );
ok(not "\x[A652]"  ~~ m/^<?DecimalNumber>$/, q{Don't match unrelated <?DecimalNumber>} );
ok("\x[A652]"  ~~ m/^<!DecimalNumber>.$/, q{Match unrelated negated <?DecimalNumber>} );
ok("\x[A652]"  ~~ m/^<-DecimalNumber>$/, q{Match unrelated inverted <?DecimalNumber>} );
ok("\x[A652]\c[DIGIT ZERO]" ~~ m/<?DecimalNumber>/, q{Match unanchored <?DecimalNumber>} );

# Nl          LetterNumber


ok("\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<?Nl>$/, q{Match <?Nl> (LetterNumber)} );
ok(not "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<!Nl>.$/, q{Don't match negated <?Nl> (LetterNumber)} );
ok(not "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<-Nl>$/, q{Don't match inverted <?Nl> (LetterNumber)} );
ok(not "\x[6C2F]"  ~~ m/^<?Nl>$/, q{Don't match unrelated <?Nl> (LetterNumber)} );
ok("\x[6C2F]"  ~~ m/^<!Nl>.$/, q{Match unrelated negated <?Nl> (LetterNumber)} );
ok("\x[6C2F]"  ~~ m/^<-Nl>$/, q{Match unrelated inverted <?Nl> (LetterNumber)} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<?Nl>$/, q{Don't match related <?Nl> (LetterNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<!Nl>.$/, q{Match related negated <?Nl> (LetterNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<-Nl>$/, q{Match related inverted <?Nl> (LetterNumber)} );
ok("\x[6C2F]\c[DIGIT ZERO]\c[RUNIC ARLAUG SYMBOL]" ~~ m/<?Nl>/, q{Match unanchored <?Nl> (LetterNumber)} );

ok("\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<?LetterNumber>$/, q{Match <?LetterNumber>} );
ok(not "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<!LetterNumber>.$/, q{Don't match negated <?LetterNumber>} );
ok(not "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<-LetterNumber>$/, q{Don't match inverted <?LetterNumber>} );
ok(not "\x[80A5]"  ~~ m/^<?LetterNumber>$/, q{Don't match unrelated <?LetterNumber>} );
ok("\x[80A5]"  ~~ m/^<!LetterNumber>.$/, q{Match unrelated negated <?LetterNumber>} );
ok("\x[80A5]"  ~~ m/^<-LetterNumber>$/, q{Match unrelated inverted <?LetterNumber>} );
ok(not "\x[80A5]" ~~ m/^<?LetterNumber>$/, q{Don't match related <?LetterNumber>} );
ok("\x[80A5]" ~~ m/^<!LetterNumber>.$/, q{Match related negated <?LetterNumber>} );
ok("\x[80A5]" ~~ m/^<-LetterNumber>$/, q{Match related inverted <?LetterNumber>} );
ok("\x[80A5]\x[80A5]\c[RUNIC ARLAUG SYMBOL]" ~~ m/<?LetterNumber>/, q{Match unanchored <?LetterNumber>} );

# No          OtherNumber


ok("\c[SUPERSCRIPT TWO]" ~~ m/^<?No>$/, q{Match <?No> (OtherNumber)} );
ok(not "\c[SUPERSCRIPT TWO]" ~~ m/^<!No>.$/, q{Don't match negated <?No> (OtherNumber)} );
ok(not "\c[SUPERSCRIPT TWO]" ~~ m/^<-No>$/, q{Don't match inverted <?No> (OtherNumber)} );
ok(not "\x[92F3]"  ~~ m/^<?No>$/, q{Don't match unrelated <?No> (OtherNumber)} );
ok("\x[92F3]"  ~~ m/^<!No>.$/, q{Match unrelated negated <?No> (OtherNumber)} );
ok("\x[92F3]"  ~~ m/^<-No>$/, q{Match unrelated inverted <?No> (OtherNumber)} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<?No>$/, q{Don't match related <?No> (OtherNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<!No>.$/, q{Match related negated <?No> (OtherNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<-No>$/, q{Match related inverted <?No> (OtherNumber)} );
ok("\x[92F3]\c[DIGIT ZERO]\c[SUPERSCRIPT TWO]" ~~ m/<?No>/, q{Match unanchored <?No> (OtherNumber)} );

ok("\c[SUPERSCRIPT TWO]" ~~ m/^<?OtherNumber>$/, q{Match <?OtherNumber>} );
ok(not "\c[SUPERSCRIPT TWO]" ~~ m/^<!OtherNumber>.$/, q{Don't match negated <?OtherNumber>} );
ok(not "\c[SUPERSCRIPT TWO]" ~~ m/^<-OtherNumber>$/, q{Don't match inverted <?OtherNumber>} );
ok(not "\x[5363]"  ~~ m/^<?OtherNumber>$/, q{Don't match unrelated <?OtherNumber>} );
ok("\x[5363]"  ~~ m/^<!OtherNumber>.$/, q{Match unrelated negated <?OtherNumber>} );
ok("\x[5363]"  ~~ m/^<-OtherNumber>$/, q{Match unrelated inverted <?OtherNumber>} );
ok("\x[5363]\c[SUPERSCRIPT TWO]" ~~ m/<?OtherNumber>/, q{Match unanchored <?OtherNumber>} );

# P           Punctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<P>$/, q{Match <P> (Punctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<!P>.$/, q{Don't match negated <P> (Punctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<-P>$/, q{Don't match inverted <P> (Punctuation)} );
ok(not "\x[A918]"  ~~ m/^<P>$/, q{Don't match unrelated <P> (Punctuation)} );
ok("\x[A918]"  ~~ m/^<!P>.$/, q{Match unrelated negated <P> (Punctuation)} );
ok("\x[A918]"  ~~ m/^<-P>$/, q{Match unrelated inverted <P> (Punctuation)} );
ok("\x[A918]\c[EXCLAMATION MARK]" ~~ m/<P>/, q{Match unanchored <P> (Punctuation)} );

ok("\c[EXCLAMATION MARK]" ~~ m/^<?Punctuation>$/, q{Match <?Punctuation>} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<!Punctuation>.$/, q{Don't match negated <?Punctuation>} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<-Punctuation>$/, q{Don't match inverted <?Punctuation>} );
ok(not "\x[CE60]"  ~~ m/^<?Punctuation>$/, q{Don't match unrelated <?Punctuation>} );
ok("\x[CE60]"  ~~ m/^<!Punctuation>.$/, q{Match unrelated negated <?Punctuation>} );
ok("\x[CE60]"  ~~ m/^<-Punctuation>$/, q{Match unrelated inverted <?Punctuation>} );
ok("\x[CE60]\c[EXCLAMATION MARK]" ~~ m/<?Punctuation>/, q{Match unanchored <?Punctuation>} );

# Pc          ConnectorPunctuation


ok("\c[LOW LINE]" ~~ m/^<?Pc>$/, q{Match <?Pc> (ConnectorPunctuation)} );
ok(not "\c[LOW LINE]" ~~ m/^<!Pc>.$/, q{Don't match negated <?Pc> (ConnectorPunctuation)} );
ok(not "\c[LOW LINE]" ~~ m/^<-Pc>$/, q{Don't match inverted <?Pc> (ConnectorPunctuation)} );
ok(not "\x[5F19]"  ~~ m/^<?Pc>$/, q{Don't match unrelated <?Pc> (ConnectorPunctuation)} );
ok("\x[5F19]"  ~~ m/^<!Pc>.$/, q{Match unrelated negated <?Pc> (ConnectorPunctuation)} );
ok("\x[5F19]"  ~~ m/^<-Pc>$/, q{Match unrelated inverted <?Pc> (ConnectorPunctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<?Pc>$/, q{Don't match related <?Pc> (ConnectorPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<!Pc>.$/, q{Match related negated <?Pc> (ConnectorPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-Pc>$/, q{Match related inverted <?Pc> (ConnectorPunctuation)} );
ok("\x[5F19]\c[EXCLAMATION MARK]\c[LOW LINE]" ~~ m/<?Pc>/, q{Match unanchored <?Pc> (ConnectorPunctuation)} );

ok("\c[LOW LINE]" ~~ m/^<?ConnectorPunctuation>$/, q{Match <?ConnectorPunctuation>} );
ok(not "\c[LOW LINE]" ~~ m/^<!ConnectorPunctuation>.$/, q{Don't match negated <?ConnectorPunctuation>} );
ok(not "\c[LOW LINE]" ~~ m/^<-ConnectorPunctuation>$/, q{Don't match inverted <?ConnectorPunctuation>} );
ok(not "\c[YI SYLLABLE MGOX]"  ~~ m/^<?ConnectorPunctuation>$/, q{Don't match unrelated <?ConnectorPunctuation>} );
ok("\c[YI SYLLABLE MGOX]"  ~~ m/^<!ConnectorPunctuation>.$/, q{Match unrelated negated <?ConnectorPunctuation>} );
ok("\c[YI SYLLABLE MGOX]"  ~~ m/^<-ConnectorPunctuation>$/, q{Match unrelated inverted <?ConnectorPunctuation>} );
ok("\c[YI SYLLABLE MGOX]\c[LOW LINE]" ~~ m/<?ConnectorPunctuation>/, q{Match unanchored <?ConnectorPunctuation>} );

# Pd          DashPunctuation


ok("\c[HYPHEN-MINUS]" ~~ m/^<?Pd>$/, q{Match <?Pd> (DashPunctuation)} );
ok(not "\c[HYPHEN-MINUS]" ~~ m/^<!Pd>.$/, q{Don't match negated <?Pd> (DashPunctuation)} );
ok(not "\c[HYPHEN-MINUS]" ~~ m/^<-Pd>$/, q{Don't match inverted <?Pd> (DashPunctuation)} );
ok(not "\x[49A1]"  ~~ m/^<?Pd>$/, q{Don't match unrelated <?Pd> (DashPunctuation)} );
ok("\x[49A1]"  ~~ m/^<!Pd>.$/, q{Match unrelated negated <?Pd> (DashPunctuation)} );
ok("\x[49A1]"  ~~ m/^<-Pd>$/, q{Match unrelated inverted <?Pd> (DashPunctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<?Pd>$/, q{Don't match related <?Pd> (DashPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<!Pd>.$/, q{Match related negated <?Pd> (DashPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-Pd>$/, q{Match related inverted <?Pd> (DashPunctuation)} );
ok("\x[49A1]\c[EXCLAMATION MARK]\c[HYPHEN-MINUS]" ~~ m/<?Pd>/, q{Match unanchored <?Pd> (DashPunctuation)} );

ok("\c[HYPHEN-MINUS]" ~~ m/^<?DashPunctuation>$/, q{Match <?DashPunctuation>} );
ok(not "\c[HYPHEN-MINUS]" ~~ m/^<!DashPunctuation>.$/, q{Don't match negated <?DashPunctuation>} );
ok(not "\c[HYPHEN-MINUS]" ~~ m/^<-DashPunctuation>$/, q{Don't match inverted <?DashPunctuation>} );
ok(not "\x[3C6E]"  ~~ m/^<?DashPunctuation>$/, q{Don't match unrelated <?DashPunctuation>} );
ok("\x[3C6E]"  ~~ m/^<!DashPunctuation>.$/, q{Match unrelated negated <?DashPunctuation>} );
ok("\x[3C6E]"  ~~ m/^<-DashPunctuation>$/, q{Match unrelated inverted <?DashPunctuation>} );
ok("\x[3C6E]\c[HYPHEN-MINUS]" ~~ m/<?DashPunctuation>/, q{Match unanchored <?DashPunctuation>} );

# Ps          OpenPunctuation


ok("\c[LEFT PARENTHESIS]" ~~ m/^<?Ps>$/, q{Match <?Ps> (OpenPunctuation)} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<!Ps>.$/, q{Don't match negated <?Ps> (OpenPunctuation)} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<-Ps>$/, q{Don't match inverted <?Ps> (OpenPunctuation)} );
ok(not "\x[C8A5]"  ~~ m/^<?Ps>$/, q{Don't match unrelated <?Ps> (OpenPunctuation)} );
ok("\x[C8A5]"  ~~ m/^<!Ps>.$/, q{Match unrelated negated <?Ps> (OpenPunctuation)} );
ok("\x[C8A5]"  ~~ m/^<-Ps>$/, q{Match unrelated inverted <?Ps> (OpenPunctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<?Ps>$/, q{Don't match related <?Ps> (OpenPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<!Ps>.$/, q{Match related negated <?Ps> (OpenPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-Ps>$/, q{Match related inverted <?Ps> (OpenPunctuation)} );
ok("\x[C8A5]\c[EXCLAMATION MARK]\c[LEFT PARENTHESIS]" ~~ m/<?Ps>/, q{Match unanchored <?Ps> (OpenPunctuation)} );

ok("\c[LEFT PARENTHESIS]" ~~ m/^<?OpenPunctuation>$/, q{Match <?OpenPunctuation>} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<!OpenPunctuation>.$/, q{Don't match negated <?OpenPunctuation>} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<-OpenPunctuation>$/, q{Don't match inverted <?OpenPunctuation>} );
ok(not "\x[84B8]"  ~~ m/^<?OpenPunctuation>$/, q{Don't match unrelated <?OpenPunctuation>} );
ok("\x[84B8]"  ~~ m/^<!OpenPunctuation>.$/, q{Match unrelated negated <?OpenPunctuation>} );
ok("\x[84B8]"  ~~ m/^<-OpenPunctuation>$/, q{Match unrelated inverted <?OpenPunctuation>} );
ok("\x[84B8]\c[LEFT PARENTHESIS]" ~~ m/<?OpenPunctuation>/, q{Match unanchored <?OpenPunctuation>} );

# Pe          ClosePunctuation


ok("\c[RIGHT PARENTHESIS]" ~~ m/^<?Pe>$/, q{Match <?Pe> (ClosePunctuation)} );
ok(not "\c[RIGHT PARENTHESIS]" ~~ m/^<!Pe>.$/, q{Don't match negated <?Pe> (ClosePunctuation)} );
ok(not "\c[RIGHT PARENTHESIS]" ~~ m/^<-Pe>$/, q{Don't match inverted <?Pe> (ClosePunctuation)} );
ok(not "\x[BB92]"  ~~ m/^<?Pe>$/, q{Don't match unrelated <?Pe> (ClosePunctuation)} );
ok("\x[BB92]"  ~~ m/^<!Pe>.$/, q{Match unrelated negated <?Pe> (ClosePunctuation)} );
ok("\x[BB92]"  ~~ m/^<-Pe>$/, q{Match unrelated inverted <?Pe> (ClosePunctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<?Pe>$/, q{Don't match related <?Pe> (ClosePunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<!Pe>.$/, q{Match related negated <?Pe> (ClosePunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-Pe>$/, q{Match related inverted <?Pe> (ClosePunctuation)} );
ok("\x[BB92]\c[EXCLAMATION MARK]\c[RIGHT PARENTHESIS]" ~~ m/<?Pe>/, q{Match unanchored <?Pe> (ClosePunctuation)} );

ok("\c[RIGHT PARENTHESIS]" ~~ m/^<?ClosePunctuation>$/, q{Match <?ClosePunctuation>} );
ok(not "\c[RIGHT PARENTHESIS]" ~~ m/^<!ClosePunctuation>.$/, q{Don't match negated <?ClosePunctuation>} );
ok(not "\c[RIGHT PARENTHESIS]" ~~ m/^<-ClosePunctuation>$/, q{Don't match inverted <?ClosePunctuation>} );
ok(not "\x[D55D]"  ~~ m/^<?ClosePunctuation>$/, q{Don't match unrelated <?ClosePunctuation>} );
ok("\x[D55D]"  ~~ m/^<!ClosePunctuation>.$/, q{Match unrelated negated <?ClosePunctuation>} );
ok("\x[D55D]"  ~~ m/^<-ClosePunctuation>$/, q{Match unrelated inverted <?ClosePunctuation>} );
ok("\x[D55D]\c[RIGHT PARENTHESIS]" ~~ m/<?ClosePunctuation>/, q{Match unanchored <?ClosePunctuation>} );

# Pi          InitialPunctuation


ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<?Pi>$/, q{Match <?Pi> (InitialPunctuation)} );
ok(not "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<!Pi>.$/, q{Don't match negated <?Pi> (InitialPunctuation)} );
ok(not "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-Pi>$/, q{Don't match inverted <?Pi> (InitialPunctuation)} );
ok(not "\x[3A35]"  ~~ m/^<?Pi>$/, q{Don't match unrelated <?Pi> (InitialPunctuation)} );
ok("\x[3A35]"  ~~ m/^<!Pi>.$/, q{Match unrelated negated <?Pi> (InitialPunctuation)} );
ok("\x[3A35]"  ~~ m/^<-Pi>$/, q{Match unrelated inverted <?Pi> (InitialPunctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<?Pi>$/, q{Don't match related <?Pi> (InitialPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<!Pi>.$/, q{Match related negated <?Pi> (InitialPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-Pi>$/, q{Match related inverted <?Pi> (InitialPunctuation)} );
ok("\x[3A35]\c[EXCLAMATION MARK]\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<?Pi>/, q{Match unanchored <?Pi> (InitialPunctuation)} );

ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<?InitialPunctuation>$/, q{Match <?InitialPunctuation>} );
ok(not "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<!InitialPunctuation>.$/, q{Don't match negated <?InitialPunctuation>} );
ok(not "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-InitialPunctuation>$/, q{Don't match inverted <?InitialPunctuation>} );
ok(not "\x[B84F]"  ~~ m/^<?InitialPunctuation>$/, q{Don't match unrelated <?InitialPunctuation>} );
ok("\x[B84F]"  ~~ m/^<!InitialPunctuation>.$/, q{Match unrelated negated <?InitialPunctuation>} );
ok("\x[B84F]"  ~~ m/^<-InitialPunctuation>$/, q{Match unrelated inverted <?InitialPunctuation>} );
ok("\x[B84F]\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<?InitialPunctuation>/, q{Match unanchored <?InitialPunctuation>} );

# Pf          FinalPunctuation


ok("\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<?Pf>$/, q{Match <?Pf> (FinalPunctuation)} );
ok(not "\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<!Pf>.$/, q{Don't match negated <?Pf> (FinalPunctuation)} );
ok(not "\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-Pf>$/, q{Don't match inverted <?Pf> (FinalPunctuation)} );
ok(not "\x[27CF]"  ~~ m/^<?Pf>$/, q{Don't match unrelated <?Pf> (FinalPunctuation)} );
ok("\x[27CF]"  ~~ m/^<!Pf>.$/, q{Match unrelated negated <?Pf> (FinalPunctuation)} );
ok("\x[27CF]"  ~~ m/^<-Pf>$/, q{Match unrelated inverted <?Pf> (FinalPunctuation)} );
ok(not "\c[MATHEMATICAL LEFT WHITE SQUARE BRACKET]" ~~ m/^<?Pf>$/, q{Don't match related <?Pf> (FinalPunctuation)} );
ok("\c[MATHEMATICAL LEFT WHITE SQUARE BRACKET]" ~~ m/^<!Pf>.$/, q{Match related negated <?Pf> (FinalPunctuation)} );
ok("\c[MATHEMATICAL LEFT WHITE SQUARE BRACKET]" ~~ m/^<-Pf>$/, q{Match related inverted <?Pf> (FinalPunctuation)} );
ok("\x[27CF]\c[MATHEMATICAL LEFT WHITE SQUARE BRACKET]\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<?Pf>/, q{Match unanchored <?Pf> (FinalPunctuation)} );

ok("\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<?FinalPunctuation>$/, q{Match <?FinalPunctuation>} );
ok(not "\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<!FinalPunctuation>.$/, q{Don't match negated <?FinalPunctuation>} );
ok(not "\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-FinalPunctuation>$/, q{Don't match inverted <?FinalPunctuation>} );
ok(not "\x[4F65]"  ~~ m/^<?FinalPunctuation>$/, q{Don't match unrelated <?FinalPunctuation>} );
ok("\x[4F65]"  ~~ m/^<!FinalPunctuation>.$/, q{Match unrelated negated <?FinalPunctuation>} );
ok("\x[4F65]"  ~~ m/^<-FinalPunctuation>$/, q{Match unrelated inverted <?FinalPunctuation>} );
ok("\x[4F65]\c[RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<?FinalPunctuation>/, q{Match unanchored <?FinalPunctuation>} );

# Po          OtherPunctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<?Po>$/, q{Match <?Po> (OtherPunctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<!Po>.$/, q{Don't match negated <?Po> (OtherPunctuation)} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<-Po>$/, q{Don't match inverted <?Po> (OtherPunctuation)} );
ok(not "\x[AA74]"  ~~ m/^<?Po>$/, q{Don't match unrelated <?Po> (OtherPunctuation)} );
ok("\x[AA74]"  ~~ m/^<!Po>.$/, q{Match unrelated negated <?Po> (OtherPunctuation)} );
ok("\x[AA74]"  ~~ m/^<-Po>$/, q{Match unrelated inverted <?Po> (OtherPunctuation)} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<?Po>$/, q{Don't match related <?Po> (OtherPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<!Po>.$/, q{Match related negated <?Po> (OtherPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<-Po>$/, q{Match related inverted <?Po> (OtherPunctuation)} );
ok("\x[AA74]\c[LEFT PARENTHESIS]\c[EXCLAMATION MARK]" ~~ m/<?Po>/, q{Match unanchored <?Po> (OtherPunctuation)} );

ok("\c[EXCLAMATION MARK]" ~~ m/^<?OtherPunctuation>$/, q{Match <?OtherPunctuation>} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<!OtherPunctuation>.$/, q{Don't match negated <?OtherPunctuation>} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<-OtherPunctuation>$/, q{Don't match inverted <?OtherPunctuation>} );
ok(not "\x[7DD2]"  ~~ m/^<?OtherPunctuation>$/, q{Don't match unrelated <?OtherPunctuation>} );
ok("\x[7DD2]"  ~~ m/^<!OtherPunctuation>.$/, q{Match unrelated negated <?OtherPunctuation>} );
ok("\x[7DD2]"  ~~ m/^<-OtherPunctuation>$/, q{Match unrelated inverted <?OtherPunctuation>} );
ok("\x[7DD2]\c[EXCLAMATION MARK]" ~~ m/<?OtherPunctuation>/, q{Match unanchored <?OtherPunctuation>} );

# S           Symbol


ok("\c[YI RADICAL QOT]" ~~ m/^<S>$/, q{Match <S> (Symbol)} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<!S>.$/, q{Don't match negated <S> (Symbol)} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<-S>$/, q{Don't match inverted <S> (Symbol)} );
ok(not "\x[8839]"  ~~ m/^<S>$/, q{Don't match unrelated <S> (Symbol)} );
ok("\x[8839]"  ~~ m/^<!S>.$/, q{Match unrelated negated <S> (Symbol)} );
ok("\x[8839]"  ~~ m/^<-S>$/, q{Match unrelated inverted <S> (Symbol)} );
ok("\x[8839]\c[YI RADICAL QOT]" ~~ m/<S>/, q{Match unanchored <S> (Symbol)} );

ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<?Symbol>$/, q{Match <?Symbol>} );
ok(not "\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<!Symbol>.$/, q{Don't match negated <?Symbol>} );
ok(not "\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<-Symbol>$/, q{Don't match inverted <?Symbol>} );
ok(not "\x[4A1C]"  ~~ m/^<?Symbol>$/, q{Don't match unrelated <?Symbol>} );
ok("\x[4A1C]"  ~~ m/^<!Symbol>.$/, q{Match unrelated negated <?Symbol>} );
ok("\x[4A1C]"  ~~ m/^<-Symbol>$/, q{Match unrelated inverted <?Symbol>} );
ok("\x[4A1C]\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/<?Symbol>/, q{Match unanchored <?Symbol>} );

# Sm          MathSymbol


ok("\c[PLUS SIGN]" ~~ m/^<?Sm>$/, q{Match <?Sm> (MathSymbol)} );
ok(not "\c[PLUS SIGN]" ~~ m/^<!Sm>.$/, q{Don't match negated <?Sm> (MathSymbol)} );
ok(not "\c[PLUS SIGN]" ~~ m/^<-Sm>$/, q{Don't match inverted <?Sm> (MathSymbol)} );
ok(not "\x[B258]"  ~~ m/^<?Sm>$/, q{Don't match unrelated <?Sm> (MathSymbol)} );
ok("\x[B258]"  ~~ m/^<!Sm>.$/, q{Match unrelated negated <?Sm> (MathSymbol)} );
ok("\x[B258]"  ~~ m/^<-Sm>$/, q{Match unrelated inverted <?Sm> (MathSymbol)} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<?Sm>$/, q{Don't match related <?Sm> (MathSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<!Sm>.$/, q{Match related negated <?Sm> (MathSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-Sm>$/, q{Match related inverted <?Sm> (MathSymbol)} );
ok("\x[B258]\c[DOLLAR SIGN]\c[PLUS SIGN]" ~~ m/<?Sm>/, q{Match unanchored <?Sm> (MathSymbol)} );

ok("\c[PLUS SIGN]" ~~ m/^<?MathSymbol>$/, q{Match <?MathSymbol>} );
ok(not "\c[PLUS SIGN]" ~~ m/^<!MathSymbol>.$/, q{Don't match negated <?MathSymbol>} );
ok(not "\c[PLUS SIGN]" ~~ m/^<-MathSymbol>$/, q{Don't match inverted <?MathSymbol>} );
ok(not "\x[98FF]"  ~~ m/^<?MathSymbol>$/, q{Don't match unrelated <?MathSymbol>} );
ok("\x[98FF]"  ~~ m/^<!MathSymbol>.$/, q{Match unrelated negated <?MathSymbol>} );
ok("\x[98FF]"  ~~ m/^<-MathSymbol>$/, q{Match unrelated inverted <?MathSymbol>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?MathSymbol>$/, q{Don't match related <?MathSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!MathSymbol>.$/, q{Match related negated <?MathSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-MathSymbol>$/, q{Match related inverted <?MathSymbol>} );
ok("\x[98FF]\c[COMBINING GRAVE ACCENT]\c[PLUS SIGN]" ~~ m/<?MathSymbol>/, q{Match unanchored <?MathSymbol>} );

# Sc          CurrencySymbol


ok("\c[DOLLAR SIGN]" ~~ m/^<?Sc>$/, q{Match <?Sc> (CurrencySymbol)} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<!Sc>.$/, q{Don't match negated <?Sc> (CurrencySymbol)} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<-Sc>$/, q{Don't match inverted <?Sc> (CurrencySymbol)} );
ok(not "\x[994C]"  ~~ m/^<?Sc>$/, q{Don't match unrelated <?Sc> (CurrencySymbol)} );
ok("\x[994C]"  ~~ m/^<!Sc>.$/, q{Match unrelated negated <?Sc> (CurrencySymbol)} );
ok("\x[994C]"  ~~ m/^<-Sc>$/, q{Match unrelated inverted <?Sc> (CurrencySymbol)} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<?Sc>$/, q{Don't match related <?Sc> (CurrencySymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<!Sc>.$/, q{Match related negated <?Sc> (CurrencySymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-Sc>$/, q{Match related inverted <?Sc> (CurrencySymbol)} );
ok("\x[994C]\c[YI RADICAL QOT]\c[DOLLAR SIGN]" ~~ m/<?Sc>/, q{Match unanchored <?Sc> (CurrencySymbol)} );

ok("\c[DOLLAR SIGN]" ~~ m/^<?CurrencySymbol>$/, q{Match <?CurrencySymbol>} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<!CurrencySymbol>.$/, q{Don't match negated <?CurrencySymbol>} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<-CurrencySymbol>$/, q{Don't match inverted <?CurrencySymbol>} );
ok(not "\x[37C0]"  ~~ m/^<?CurrencySymbol>$/, q{Don't match unrelated <?CurrencySymbol>} );
ok("\x[37C0]"  ~~ m/^<!CurrencySymbol>.$/, q{Match unrelated negated <?CurrencySymbol>} );
ok("\x[37C0]"  ~~ m/^<-CurrencySymbol>$/, q{Match unrelated inverted <?CurrencySymbol>} );
ok("\x[37C0]\c[DOLLAR SIGN]" ~~ m/<?CurrencySymbol>/, q{Match unanchored <?CurrencySymbol>} );

# Sk          ModifierSymbol


ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<?Sk>$/, q{Match <?Sk> (ModifierSymbol)} );
ok(not "\c[CIRCUMFLEX ACCENT]" ~~ m/^<!Sk>.$/, q{Don't match negated <?Sk> (ModifierSymbol)} );
ok(not "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-Sk>$/, q{Don't match inverted <?Sk> (ModifierSymbol)} );
ok(not "\x[4578]"  ~~ m/^<?Sk>$/, q{Don't match unrelated <?Sk> (ModifierSymbol)} );
ok("\x[4578]"  ~~ m/^<!Sk>.$/, q{Match unrelated negated <?Sk> (ModifierSymbol)} );
ok("\x[4578]"  ~~ m/^<-Sk>$/, q{Match unrelated inverted <?Sk> (ModifierSymbol)} );
ok(not "\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<?Sk>$/, q{Don't match related <?Sk> (ModifierSymbol)} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<!Sk>.$/, q{Match related negated <?Sk> (ModifierSymbol)} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<-Sk>$/, q{Match related inverted <?Sk> (ModifierSymbol)} );
ok("\x[4578]\c[HEXAGRAM FOR THE CREATIVE HEAVEN]\c[CIRCUMFLEX ACCENT]" ~~ m/<?Sk>/, q{Match unanchored <?Sk> (ModifierSymbol)} );

ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<?ModifierSymbol>$/, q{Match <?ModifierSymbol>} );
ok(not "\c[CIRCUMFLEX ACCENT]" ~~ m/^<!ModifierSymbol>.$/, q{Don't match negated <?ModifierSymbol>} );
ok(not "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-ModifierSymbol>$/, q{Don't match inverted <?ModifierSymbol>} );
ok(not "\x[42F1]"  ~~ m/^<?ModifierSymbol>$/, q{Don't match unrelated <?ModifierSymbol>} );
ok("\x[42F1]"  ~~ m/^<!ModifierSymbol>.$/, q{Match unrelated negated <?ModifierSymbol>} );
ok("\x[42F1]"  ~~ m/^<-ModifierSymbol>$/, q{Match unrelated inverted <?ModifierSymbol>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?ModifierSymbol>$/, q{Don't match related <?ModifierSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!ModifierSymbol>.$/, q{Match related negated <?ModifierSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-ModifierSymbol>$/, q{Match related inverted <?ModifierSymbol>} );
ok("\x[42F1]\c[COMBINING GRAVE ACCENT]\c[CIRCUMFLEX ACCENT]" ~~ m/<?ModifierSymbol>/, q{Match unanchored <?ModifierSymbol>} );

# So          OtherSymbol


ok("\c[YI RADICAL QOT]" ~~ m/^<?So>$/, q{Match <?So> (OtherSymbol)} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<!So>.$/, q{Don't match negated <?So> (OtherSymbol)} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<-So>$/, q{Don't match inverted <?So> (OtherSymbol)} );
ok(not "\x[83DE]"  ~~ m/^<?So>$/, q{Don't match unrelated <?So> (OtherSymbol)} );
ok("\x[83DE]"  ~~ m/^<!So>.$/, q{Match unrelated negated <?So> (OtherSymbol)} );
ok("\x[83DE]"  ~~ m/^<-So>$/, q{Match unrelated inverted <?So> (OtherSymbol)} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<?So>$/, q{Don't match related <?So> (OtherSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<!So>.$/, q{Match related negated <?So> (OtherSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-So>$/, q{Match related inverted <?So> (OtherSymbol)} );
ok("\x[83DE]\c[DOLLAR SIGN]\c[YI RADICAL QOT]" ~~ m/<?So>/, q{Match unanchored <?So> (OtherSymbol)} );

ok("\c[YI RADICAL QOT]" ~~ m/^<?OtherSymbol>$/, q{Match <?OtherSymbol>} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<!OtherSymbol>.$/, q{Don't match negated <?OtherSymbol>} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<-OtherSymbol>$/, q{Don't match inverted <?OtherSymbol>} );
ok(not "\x[9B2C]"  ~~ m/^<?OtherSymbol>$/, q{Don't match unrelated <?OtherSymbol>} );
ok("\x[9B2C]"  ~~ m/^<!OtherSymbol>.$/, q{Match unrelated negated <?OtherSymbol>} );
ok("\x[9B2C]"  ~~ m/^<-OtherSymbol>$/, q{Match unrelated inverted <?OtherSymbol>} );
ok("\x[9B2C]\c[YI RADICAL QOT]" ~~ m/<?OtherSymbol>/, q{Match unanchored <?OtherSymbol>} );

# Z           Separator


ok("\c[IDEOGRAPHIC SPACE]" ~~ m/^<Z>$/, q{Match <Z> (Separator)} );
ok(not "\c[IDEOGRAPHIC SPACE]" ~~ m/^<!Z>.$/, q{Don't match negated <Z> (Separator)} );
ok(not "\c[IDEOGRAPHIC SPACE]" ~~ m/^<-Z>$/, q{Don't match inverted <Z> (Separator)} );
ok(not "\x[2C08]"  ~~ m/^<Z>$/, q{Don't match unrelated <Z> (Separator)} );
ok("\x[2C08]"  ~~ m/^<!Z>.$/, q{Match unrelated negated <Z> (Separator)} );
ok("\x[2C08]"  ~~ m/^<-Z>$/, q{Match unrelated inverted <Z> (Separator)} );
ok("\x[2C08]\c[IDEOGRAPHIC SPACE]" ~~ m/<Z>/, q{Match unanchored <Z> (Separator)} );

ok("\c[SPACE]" ~~ m/^<?Separator>$/, q{Match <?Separator>} );
ok(not "\c[SPACE]" ~~ m/^<!Separator>.$/, q{Don't match negated <?Separator>} );
ok(not "\c[SPACE]" ~~ m/^<-Separator>$/, q{Don't match inverted <?Separator>} );
ok(not "\c[YI SYLLABLE SOX]"  ~~ m/^<?Separator>$/, q{Don't match unrelated <?Separator>} );
ok("\c[YI SYLLABLE SOX]"  ~~ m/^<!Separator>.$/, q{Match unrelated negated <?Separator>} );
ok("\c[YI SYLLABLE SOX]"  ~~ m/^<-Separator>$/, q{Match unrelated inverted <?Separator>} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<?Separator>$/, q{Don't match related <?Separator>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<!Separator>.$/, q{Match related negated <?Separator>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-Separator>$/, q{Match related inverted <?Separator>} );
ok("\c[YI SYLLABLE SOX]\c[YI RADICAL QOT]\c[SPACE]" ~~ m/<?Separator>/, q{Match unanchored <?Separator>} );

# Zs          SpaceSeparator


ok("\c[SPACE]" ~~ m/^<?Zs>$/, q{Match <?Zs> (SpaceSeparator)} );
ok(not "\c[SPACE]" ~~ m/^<!Zs>.$/, q{Don't match negated <?Zs> (SpaceSeparator)} );
ok(not "\c[SPACE]" ~~ m/^<-Zs>$/, q{Don't match inverted <?Zs> (SpaceSeparator)} );
ok(not "\x[88DD]"  ~~ m/^<?Zs>$/, q{Don't match unrelated <?Zs> (SpaceSeparator)} );
ok("\x[88DD]"  ~~ m/^<!Zs>.$/, q{Match unrelated negated <?Zs> (SpaceSeparator)} );
ok("\x[88DD]"  ~~ m/^<-Zs>$/, q{Match unrelated inverted <?Zs> (SpaceSeparator)} );
ok(not "\c[LINE SEPARATOR]" ~~ m/^<?Zs>$/, q{Don't match related <?Zs> (SpaceSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<!Zs>.$/, q{Match related negated <?Zs> (SpaceSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<-Zs>$/, q{Match related inverted <?Zs> (SpaceSeparator)} );
ok("\x[88DD]\c[LINE SEPARATOR]\c[SPACE]" ~~ m/<?Zs>/, q{Match unanchored <?Zs> (SpaceSeparator)} );

ok("\c[SPACE]" ~~ m/^<?SpaceSeparator>$/, q{Match <?SpaceSeparator>} );
ok(not "\c[SPACE]" ~~ m/^<!SpaceSeparator>.$/, q{Don't match negated <?SpaceSeparator>} );
ok(not "\c[SPACE]" ~~ m/^<-SpaceSeparator>$/, q{Don't match inverted <?SpaceSeparator>} );
ok(not "\x[C808]"  ~~ m/^<?SpaceSeparator>$/, q{Don't match unrelated <?SpaceSeparator>} );
ok("\x[C808]"  ~~ m/^<!SpaceSeparator>.$/, q{Match unrelated negated <?SpaceSeparator>} );
ok("\x[C808]"  ~~ m/^<-SpaceSeparator>$/, q{Match unrelated inverted <?SpaceSeparator>} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<?SpaceSeparator>$/, q{Don't match related <?SpaceSeparator>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<!SpaceSeparator>.$/, q{Match related negated <?SpaceSeparator>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-SpaceSeparator>$/, q{Match related inverted <?SpaceSeparator>} );
ok("\x[C808]\c[DOLLAR SIGN]\c[SPACE]" ~~ m/<?SpaceSeparator>/, q{Match unanchored <?SpaceSeparator>} );

# Zl          LineSeparator


ok("\c[LINE SEPARATOR]" ~~ m/^<?Zl>$/, q{Match <?Zl> (LineSeparator)} );
ok(not "\c[LINE SEPARATOR]" ~~ m/^<!Zl>.$/, q{Don't match negated <?Zl> (LineSeparator)} );
ok(not "\c[LINE SEPARATOR]" ~~ m/^<-Zl>$/, q{Don't match inverted <?Zl> (LineSeparator)} );
ok(not "\x[B822]"  ~~ m/^<?Zl>$/, q{Don't match unrelated <?Zl> (LineSeparator)} );
ok("\x[B822]"  ~~ m/^<!Zl>.$/, q{Match unrelated negated <?Zl> (LineSeparator)} );
ok("\x[B822]"  ~~ m/^<-Zl>$/, q{Match unrelated inverted <?Zl> (LineSeparator)} );
ok(not "\c[SPACE]" ~~ m/^<?Zl>$/, q{Don't match related <?Zl> (LineSeparator)} );
ok("\c[SPACE]" ~~ m/^<!Zl>.$/, q{Match related negated <?Zl> (LineSeparator)} );
ok("\c[SPACE]" ~~ m/^<-Zl>$/, q{Match related inverted <?Zl> (LineSeparator)} );
ok("\x[B822]\c[SPACE]\c[LINE SEPARATOR]" ~~ m/<?Zl>/, q{Match unanchored <?Zl> (LineSeparator)} );

ok("\c[LINE SEPARATOR]" ~~ m/^<?LineSeparator>$/, q{Match <?LineSeparator>} );
ok(not "\c[LINE SEPARATOR]" ~~ m/^<!LineSeparator>.$/, q{Don't match negated <?LineSeparator>} );
ok(not "\c[LINE SEPARATOR]" ~~ m/^<-LineSeparator>$/, q{Don't match inverted <?LineSeparator>} );
ok(not "\x[1390]"  ~~ m/^<?LineSeparator>$/, q{Don't match unrelated <?LineSeparator>} );
ok("\x[1390]"  ~~ m/^<!LineSeparator>.$/, q{Match unrelated negated <?LineSeparator>} );
ok("\x[1390]"  ~~ m/^<-LineSeparator>$/, q{Match unrelated inverted <?LineSeparator>} );
ok(not "\c[CHEROKEE LETTER A]" ~~ m/^<?LineSeparator>$/, q{Don't match related <?LineSeparator>} );
ok("\c[CHEROKEE LETTER A]" ~~ m/^<!LineSeparator>.$/, q{Match related negated <?LineSeparator>} );
ok("\c[CHEROKEE LETTER A]" ~~ m/^<-LineSeparator>$/, q{Match related inverted <?LineSeparator>} );
ok("\x[1390]\c[CHEROKEE LETTER A]\c[LINE SEPARATOR]" ~~ m/<?LineSeparator>/, q{Match unanchored <?LineSeparator>} );

# Zp          ParagraphSeparator


ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<?Zp>$/, q{Match <?Zp> (ParagraphSeparator)} );
ok(not "\c[PARAGRAPH SEPARATOR]" ~~ m/^<!Zp>.$/, q{Don't match negated <?Zp> (ParagraphSeparator)} );
ok(not "\c[PARAGRAPH SEPARATOR]" ~~ m/^<-Zp>$/, q{Don't match inverted <?Zp> (ParagraphSeparator)} );
ok(not "\x[5FDE]"  ~~ m/^<?Zp>$/, q{Don't match unrelated <?Zp> (ParagraphSeparator)} );
ok("\x[5FDE]"  ~~ m/^<!Zp>.$/, q{Match unrelated negated <?Zp> (ParagraphSeparator)} );
ok("\x[5FDE]"  ~~ m/^<-Zp>$/, q{Match unrelated inverted <?Zp> (ParagraphSeparator)} );
ok(not "\c[SPACE]" ~~ m/^<?Zp>$/, q{Don't match related <?Zp> (ParagraphSeparator)} );
ok("\c[SPACE]" ~~ m/^<!Zp>.$/, q{Match related negated <?Zp> (ParagraphSeparator)} );
ok("\c[SPACE]" ~~ m/^<-Zp>$/, q{Match related inverted <?Zp> (ParagraphSeparator)} );
ok("\x[5FDE]\c[SPACE]\c[PARAGRAPH SEPARATOR]" ~~ m/<?Zp>/, q{Match unanchored <?Zp> (ParagraphSeparator)} );

ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<?ParagraphSeparator>$/, q{Match <?ParagraphSeparator>} );
ok(not "\c[PARAGRAPH SEPARATOR]" ~~ m/^<!ParagraphSeparator>.$/, q{Don't match negated <?ParagraphSeparator>} );
ok(not "\c[PARAGRAPH SEPARATOR]" ~~ m/^<-ParagraphSeparator>$/, q{Don't match inverted <?ParagraphSeparator>} );
ok(not "\x[345B]"  ~~ m/^<?ParagraphSeparator>$/, q{Don't match unrelated <?ParagraphSeparator>} );
ok("\x[345B]"  ~~ m/^<!ParagraphSeparator>.$/, q{Match unrelated negated <?ParagraphSeparator>} );
ok("\x[345B]"  ~~ m/^<-ParagraphSeparator>$/, q{Match unrelated inverted <?ParagraphSeparator>} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<?ParagraphSeparator>$/, q{Don't match related <?ParagraphSeparator>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<!ParagraphSeparator>.$/, q{Match related negated <?ParagraphSeparator>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-ParagraphSeparator>$/, q{Match related inverted <?ParagraphSeparator>} );
ok("\x[345B]\c[EXCLAMATION MARK]\c[PARAGRAPH SEPARATOR]" ~~ m/<?ParagraphSeparator>/, q{Match unanchored <?ParagraphSeparator>} );

# C           Other


ok("\x[9FA6]" ~~ m/^<C>$/, q{Match <C> (Other)} );
ok(not "\x[9FA6]" ~~ m/^<!C>.$/, q{Don't match negated <C> (Other)} );
ok(not "\x[9FA6]" ~~ m/^<-C>$/, q{Don't match inverted <C> (Other)} );
ok(not "\x[6A3F]"  ~~ m/^<C>$/, q{Don't match unrelated <C> (Other)} );
ok("\x[6A3F]"  ~~ m/^<!C>.$/, q{Match unrelated negated <C> (Other)} );
ok("\x[6A3F]"  ~~ m/^<-C>$/, q{Match unrelated inverted <C> (Other)} );
ok("\x[6A3F]\x[9FA6]" ~~ m/<C>/, q{Match unanchored <C> (Other)} );

ok("\x[A679]" ~~ m/^<?Other>$/, q{Match <?Other>} );
ok(not "\x[A679]" ~~ m/^<!Other>.$/, q{Don't match negated <?Other>} );
ok(not "\x[A679]" ~~ m/^<-Other>$/, q{Don't match inverted <?Other>} );
ok(not "\x[AC00]"  ~~ m/^<?Other>$/, q{Don't match unrelated <?Other>} );
ok("\x[AC00]"  ~~ m/^<!Other>.$/, q{Match unrelated negated <?Other>} );
ok("\x[AC00]"  ~~ m/^<-Other>$/, q{Match unrelated inverted <?Other>} );
ok("\x[AC00]\x[A679]" ~~ m/<?Other>/, q{Match unanchored <?Other>} );

# Cc          Control


ok("\c[NULL]" ~~ m/^<?Cc>$/, q{Match <?Cc> (Control)} );
ok(not "\c[NULL]" ~~ m/^<!Cc>.$/, q{Don't match negated <?Cc> (Control)} );
ok(not "\c[NULL]" ~~ m/^<-Cc>$/, q{Don't match inverted <?Cc> (Control)} );
ok(not "\x[0A7A]"  ~~ m/^<?Cc>$/, q{Don't match unrelated <?Cc> (Control)} );
ok("\x[0A7A]"  ~~ m/^<!Cc>.$/, q{Match unrelated negated <?Cc> (Control)} );
ok("\x[0A7A]"  ~~ m/^<-Cc>$/, q{Match unrelated inverted <?Cc> (Control)} );
ok(not "\x[0A7A]" ~~ m/^<?Cc>$/, q{Don't match related <?Cc> (Control)} );
ok("\x[0A7A]" ~~ m/^<!Cc>.$/, q{Match related negated <?Cc> (Control)} );
ok("\x[0A7A]" ~~ m/^<-Cc>$/, q{Match related inverted <?Cc> (Control)} );
ok("\x[0A7A]\x[0A7A]\c[NULL]" ~~ m/<?Cc>/, q{Match unanchored <?Cc> (Control)} );

ok("\c[NULL]" ~~ m/^<?Control>$/, q{Match <?Control>} );
ok(not "\c[NULL]" ~~ m/^<!Control>.$/, q{Don't match negated <?Control>} );
ok(not "\c[NULL]" ~~ m/^<-Control>$/, q{Don't match inverted <?Control>} );
ok(not "\x[4886]"  ~~ m/^<?Control>$/, q{Don't match unrelated <?Control>} );
ok("\x[4886]"  ~~ m/^<!Control>.$/, q{Match unrelated negated <?Control>} );
ok("\x[4886]"  ~~ m/^<-Control>$/, q{Match unrelated inverted <?Control>} );
ok(not "\x[4DB6]" ~~ m/^<?Control>$/, q{Don't match related <?Control>} );
ok("\x[4DB6]" ~~ m/^<!Control>.$/, q{Match related negated <?Control>} );
ok("\x[4DB6]" ~~ m/^<-Control>$/, q{Match related inverted <?Control>} );
ok("\x[4886]\x[4DB6]\c[NULL]" ~~ m/<?Control>/, q{Match unanchored <?Control>} );

# Cf          Format


ok("\c[SOFT HYPHEN]" ~~ m/^<?Cf>$/, q{Match <?Cf> (Format)} );
ok(not "\c[SOFT HYPHEN]" ~~ m/^<!Cf>.$/, q{Don't match negated <?Cf> (Format)} );
ok(not "\c[SOFT HYPHEN]" ~~ m/^<-Cf>$/, q{Don't match inverted <?Cf> (Format)} );
ok(not "\x[77B8]"  ~~ m/^<?Cf>$/, q{Don't match unrelated <?Cf> (Format)} );
ok("\x[77B8]"  ~~ m/^<!Cf>.$/, q{Match unrelated negated <?Cf> (Format)} );
ok("\x[77B8]"  ~~ m/^<-Cf>$/, q{Match unrelated inverted <?Cf> (Format)} );
ok(not "\x[9FA6]" ~~ m/^<?Cf>$/, q{Don't match related <?Cf> (Format)} );
ok("\x[9FA6]" ~~ m/^<!Cf>.$/, q{Match related negated <?Cf> (Format)} );
ok("\x[9FA6]" ~~ m/^<-Cf>$/, q{Match related inverted <?Cf> (Format)} );
ok("\x[77B8]\x[9FA6]\c[SOFT HYPHEN]" ~~ m/<?Cf>/, q{Match unanchored <?Cf> (Format)} );

ok("\c[KHMER VOWEL INHERENT AQ]" ~~ m/^<?Format>$/, q{Match <?Format>} );
ok(not "\c[KHMER VOWEL INHERENT AQ]" ~~ m/^<!Format>.$/, q{Don't match negated <?Format>} );
ok(not "\c[KHMER VOWEL INHERENT AQ]" ~~ m/^<-Format>$/, q{Don't match inverted <?Format>} );
ok(not "\c[DEVANAGARI VOWEL SIGN AU]"  ~~ m/^<?Format>$/, q{Don't match unrelated <?Format>} );
ok("\c[DEVANAGARI VOWEL SIGN AU]"  ~~ m/^<!Format>.$/, q{Match unrelated negated <?Format>} );
ok("\c[DEVANAGARI VOWEL SIGN AU]"  ~~ m/^<-Format>$/, q{Match unrelated inverted <?Format>} );
ok("\c[DEVANAGARI VOWEL SIGN AU]\c[KHMER VOWEL INHERENT AQ]" ~~ m/<?Format>/, q{Match unanchored <?Format>} );

# BidiL       # Left-to-Right


ok("\c[YI SYLLABLE IT]" ~~ m/^<?BidiL>$/, q{Match (Left-to-Right)} );
ok(not "\c[YI SYLLABLE IT]" ~~ m/^<!BidiL>.$/, q{Don't match negated (Left-to-Right)} );
ok(not "\c[YI SYLLABLE IT]" ~~ m/^<-BidiL>$/, q{Don't match inverted (Left-to-Right)} );
ok(not "\x[5A87]"  ~~ m/^<?BidiL>$/, q{Don't match unrelated (Left-to-Right)} );
ok("\x[5A87]"  ~~ m/^<!BidiL>.$/, q{Match unrelated negated (Left-to-Right)} );
ok("\x[5A87]"  ~~ m/^<-BidiL>$/, q{Match unrelated inverted (Left-to-Right)} );
ok("\x[5A87]\c[YI SYLLABLE IT]" ~~ m/<?BidiL>/, q{Match unanchored (Left-to-Right)} );

# BidiEN      # European Number


ok("\c[DIGIT ZERO]" ~~ m/^<?BidiEN>$/, q{Match (European Number)} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<!BidiEN>.$/, q{Don't match negated (European Number)} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<-BidiEN>$/, q{Don't match inverted (European Number)} );
ok(not "\x[AFFB]"  ~~ m/^<?BidiEN>$/, q{Don't match unrelated (European Number)} );
ok("\x[AFFB]"  ~~ m/^<!BidiEN>.$/, q{Match unrelated negated (European Number)} );
ok("\x[AFFB]"  ~~ m/^<-BidiEN>$/, q{Match unrelated inverted (European Number)} );
ok("\x[AFFB]\c[DIGIT ZERO]" ~~ m/<?BidiEN>/, q{Match unanchored (European Number)} );

# BidiES      # European Number Separator


ok("\c[SOLIDUS]" ~~ m/^<?BidiES>$/, q{Match (European Number Separator)} );
ok(not "\c[SOLIDUS]" ~~ m/^<!BidiES>.$/, q{Don't match negated (European Number Separator)} );
ok(not "\c[SOLIDUS]" ~~ m/^<-BidiES>$/, q{Don't match inverted (European Number Separator)} );
ok(not "\x[7B89]"  ~~ m/^<?BidiES>$/, q{Don't match unrelated (European Number Separator)} );
ok("\x[7B89]"  ~~ m/^<!BidiES>.$/, q{Match unrelated negated (European Number Separator)} );
ok("\x[7B89]"  ~~ m/^<-BidiES>$/, q{Match unrelated inverted (European Number Separator)} );
ok("\x[7B89]\c[SOLIDUS]" ~~ m/<?BidiES>/, q{Match unanchored (European Number Separator)} );

# BidiET      # European Number Terminator


ok("\c[NUMBER SIGN]" ~~ m/^<?BidiET>$/, q{Match (European Number Terminator)} );
ok(not "\c[NUMBER SIGN]" ~~ m/^<!BidiET>.$/, q{Don't match negated (European Number Terminator)} );
ok(not "\c[NUMBER SIGN]" ~~ m/^<-BidiET>$/, q{Don't match inverted (European Number Terminator)} );
ok(not "\x[6780]"  ~~ m/^<?BidiET>$/, q{Don't match unrelated (European Number Terminator)} );
ok("\x[6780]"  ~~ m/^<!BidiET>.$/, q{Match unrelated negated (European Number Terminator)} );
ok("\x[6780]"  ~~ m/^<-BidiET>$/, q{Match unrelated inverted (European Number Terminator)} );
ok("\x[6780]\c[NUMBER SIGN]" ~~ m/<?BidiET>/, q{Match unanchored (European Number Terminator)} );

# BidiWS      # Whitespace


ok("\c[FORM FEED (FF)]" ~~ m/^<?BidiWS>$/, q{Match (Whitespace)} );
ok(not "\c[FORM FEED (FF)]" ~~ m/^<!BidiWS>.$/, q{Don't match negated (Whitespace)} );
ok(not "\c[FORM FEED (FF)]" ~~ m/^<-BidiWS>$/, q{Don't match inverted (Whitespace)} );
ok(not "\x[6CF9]"  ~~ m/^<?BidiWS>$/, q{Don't match unrelated (Whitespace)} );
ok("\x[6CF9]"  ~~ m/^<!BidiWS>.$/, q{Match unrelated negated (Whitespace)} );
ok("\x[6CF9]"  ~~ m/^<-BidiWS>$/, q{Match unrelated inverted (Whitespace)} );
ok("\x[6CF9]\c[FORM FEED (FF)]" ~~ m/<?BidiWS>/, q{Match unanchored (Whitespace)} );

# Arabic


ok("\c[ARABIC LETTER HAMZA]" ~~ m/^<?Arabic>$/, q{Match <?Arabic>} );
ok(not "\c[ARABIC LETTER HAMZA]" ~~ m/^<!Arabic>.$/, q{Don't match negated <?Arabic>} );
ok(not "\c[ARABIC LETTER HAMZA]" ~~ m/^<-Arabic>$/, q{Don't match inverted <?Arabic>} );
ok(not "\x[A649]"  ~~ m/^<?Arabic>$/, q{Don't match unrelated <?Arabic>} );
ok("\x[A649]"  ~~ m/^<!Arabic>.$/, q{Match unrelated negated <?Arabic>} );
ok("\x[A649]"  ~~ m/^<-Arabic>$/, q{Match unrelated inverted <?Arabic>} );
ok("\x[A649]\c[ARABIC LETTER HAMZA]" ~~ m/<?Arabic>/, q{Match unanchored <?Arabic>} );

# Armenian


ok("\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<?Armenian>$/, q{Match <?Armenian>} );
ok(not "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<!Armenian>.$/, q{Don't match negated <?Armenian>} );
ok(not "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<-Armenian>$/, q{Don't match inverted <?Armenian>} );
ok(not "\x[CBFF]"  ~~ m/^<?Armenian>$/, q{Don't match unrelated <?Armenian>} );
ok("\x[CBFF]"  ~~ m/^<!Armenian>.$/, q{Match unrelated negated <?Armenian>} );
ok("\x[CBFF]"  ~~ m/^<-Armenian>$/, q{Match unrelated inverted <?Armenian>} );
ok("\x[CBFF]\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/<?Armenian>/, q{Match unanchored <?Armenian>} );

# Bengali


ok("\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<?Bengali>$/, q{Match <?Bengali>} );
ok(not "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<!Bengali>.$/, q{Don't match negated <?Bengali>} );
ok(not "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<-Bengali>$/, q{Don't match inverted <?Bengali>} );
ok(not "\x[D1E8]"  ~~ m/^<?Bengali>$/, q{Don't match unrelated <?Bengali>} );
ok("\x[D1E8]"  ~~ m/^<!Bengali>.$/, q{Match unrelated negated <?Bengali>} );
ok("\x[D1E8]"  ~~ m/^<-Bengali>$/, q{Match unrelated inverted <?Bengali>} );
ok("\x[D1E8]\c[BENGALI SIGN CANDRABINDU]" ~~ m/<?Bengali>/, q{Match unanchored <?Bengali>} );

# Bopomofo


ok("\c[BOPOMOFO LETTER B]" ~~ m/^<?Bopomofo>$/, q{Match <?Bopomofo>} );
ok(not "\c[BOPOMOFO LETTER B]" ~~ m/^<!Bopomofo>.$/, q{Don't match negated <?Bopomofo>} );
ok(not "\c[BOPOMOFO LETTER B]" ~~ m/^<-Bopomofo>$/, q{Don't match inverted <?Bopomofo>} );
ok(not "\x[B093]"  ~~ m/^<?Bopomofo>$/, q{Don't match unrelated <?Bopomofo>} );
ok("\x[B093]"  ~~ m/^<!Bopomofo>.$/, q{Match unrelated negated <?Bopomofo>} );
ok("\x[B093]"  ~~ m/^<-Bopomofo>$/, q{Match unrelated inverted <?Bopomofo>} );
ok("\x[B093]\c[BOPOMOFO LETTER B]" ~~ m/<?Bopomofo>/, q{Match unanchored <?Bopomofo>} );

# Buhid


ok("\c[BUHID LETTER A]" ~~ m/^<?Buhid>$/, q{Match <?Buhid>} );
ok(not "\c[BUHID LETTER A]" ~~ m/^<!Buhid>.$/, q{Don't match negated <?Buhid>} );
ok(not "\c[BUHID LETTER A]" ~~ m/^<-Buhid>$/, q{Don't match inverted <?Buhid>} );
ok(not "\x[C682]"  ~~ m/^<?Buhid>$/, q{Don't match unrelated <?Buhid>} );
ok("\x[C682]"  ~~ m/^<!Buhid>.$/, q{Match unrelated negated <?Buhid>} );
ok("\x[C682]"  ~~ m/^<-Buhid>$/, q{Match unrelated inverted <?Buhid>} );
ok("\x[C682]\c[BUHID LETTER A]" ~~ m/<?Buhid>/, q{Match unanchored <?Buhid>} );

# CanadianAboriginal


ok("\c[CANADIAN SYLLABICS E]" ~~ m/^<?CanadianAboriginal>$/, q{Match <?CanadianAboriginal>} );
ok(not "\c[CANADIAN SYLLABICS E]" ~~ m/^<!CanadianAboriginal>.$/, q{Don't match negated <?CanadianAboriginal>} );
ok(not "\c[CANADIAN SYLLABICS E]" ~~ m/^<-CanadianAboriginal>$/, q{Don't match inverted <?CanadianAboriginal>} );
ok(not "\x[888B]"  ~~ m/^<?CanadianAboriginal>$/, q{Don't match unrelated <?CanadianAboriginal>} );
ok("\x[888B]"  ~~ m/^<!CanadianAboriginal>.$/, q{Match unrelated negated <?CanadianAboriginal>} );
ok("\x[888B]"  ~~ m/^<-CanadianAboriginal>$/, q{Match unrelated inverted <?CanadianAboriginal>} );
ok(not "\x[9FA6]" ~~ m/^<?CanadianAboriginal>$/, q{Don't match related <?CanadianAboriginal>} );
ok("\x[9FA6]" ~~ m/^<!CanadianAboriginal>.$/, q{Match related negated <?CanadianAboriginal>} );
ok("\x[9FA6]" ~~ m/^<-CanadianAboriginal>$/, q{Match related inverted <?CanadianAboriginal>} );
ok("\x[888B]\x[9FA6]\c[CANADIAN SYLLABICS E]" ~~ m/<?CanadianAboriginal>/, q{Match unanchored <?CanadianAboriginal>} );

# Cherokee


ok("\c[CHEROKEE LETTER A]" ~~ m/^<?Cherokee>$/, q{Match <?Cherokee>} );
ok(not "\c[CHEROKEE LETTER A]" ~~ m/^<!Cherokee>.$/, q{Don't match negated <?Cherokee>} );
ok(not "\c[CHEROKEE LETTER A]" ~~ m/^<-Cherokee>$/, q{Don't match inverted <?Cherokee>} );
ok(not "\x[8260]"  ~~ m/^<?Cherokee>$/, q{Don't match unrelated <?Cherokee>} );
ok("\x[8260]"  ~~ m/^<!Cherokee>.$/, q{Match unrelated negated <?Cherokee>} );
ok("\x[8260]"  ~~ m/^<-Cherokee>$/, q{Match unrelated inverted <?Cherokee>} );
ok(not "\x[9FA6]" ~~ m/^<?Cherokee>$/, q{Don't match related <?Cherokee>} );
ok("\x[9FA6]" ~~ m/^<!Cherokee>.$/, q{Match related negated <?Cherokee>} );
ok("\x[9FA6]" ~~ m/^<-Cherokee>$/, q{Match related inverted <?Cherokee>} );
ok("\x[8260]\x[9FA6]\c[CHEROKEE LETTER A]" ~~ m/<?Cherokee>/, q{Match unanchored <?Cherokee>} );

# Cyrillic


ok("\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<?Cyrillic>$/, q{Match <?Cyrillic>} );
ok(not "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<!Cyrillic>.$/, q{Don't match negated <?Cyrillic>} );
ok(not "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<-Cyrillic>$/, q{Don't match inverted <?Cyrillic>} );
ok(not "\x[B7DF]"  ~~ m/^<?Cyrillic>$/, q{Don't match unrelated <?Cyrillic>} );
ok("\x[B7DF]"  ~~ m/^<!Cyrillic>.$/, q{Match unrelated negated <?Cyrillic>} );
ok("\x[B7DF]"  ~~ m/^<-Cyrillic>$/, q{Match unrelated inverted <?Cyrillic>} );
ok(not "\x[D7A4]" ~~ m/^<?Cyrillic>$/, q{Don't match related <?Cyrillic>} );
ok("\x[D7A4]" ~~ m/^<!Cyrillic>.$/, q{Match related negated <?Cyrillic>} );
ok("\x[D7A4]" ~~ m/^<-Cyrillic>$/, q{Match related inverted <?Cyrillic>} );
ok("\x[B7DF]\x[D7A4]\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/<?Cyrillic>/, q{Match unanchored <?Cyrillic>} );

# Deseret


ok(not "\x[A8A0]"  ~~ m/^<?Deseret>$/, q{Don't match unrelated <?Deseret>} );
ok("\x[A8A0]"  ~~ m/^<!Deseret>.$/, q{Match unrelated negated <?Deseret>} );
ok("\x[A8A0]"  ~~ m/^<-Deseret>$/, q{Match unrelated inverted <?Deseret>} );

# Devanagari


ok("\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<?Devanagari>$/, q{Match <?Devanagari>} );
ok(not "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<!Devanagari>.$/, q{Don't match negated <?Devanagari>} );
ok(not "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<-Devanagari>$/, q{Don't match inverted <?Devanagari>} );
ok(not "\x[D291]"  ~~ m/^<?Devanagari>$/, q{Don't match unrelated <?Devanagari>} );
ok("\x[D291]"  ~~ m/^<!Devanagari>.$/, q{Match unrelated negated <?Devanagari>} );
ok("\x[D291]"  ~~ m/^<-Devanagari>$/, q{Match unrelated inverted <?Devanagari>} );
ok("\x[D291]\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/<?Devanagari>/, q{Match unanchored <?Devanagari>} );

# Ethiopic


ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<?Ethiopic>$/, q{Match <?Ethiopic>} );
ok(not "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<!Ethiopic>.$/, q{Don't match negated <?Ethiopic>} );
ok(not "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<-Ethiopic>$/, q{Don't match inverted <?Ethiopic>} );
ok(not "\x[A9FA]"  ~~ m/^<?Ethiopic>$/, q{Don't match unrelated <?Ethiopic>} );
ok("\x[A9FA]"  ~~ m/^<!Ethiopic>.$/, q{Match unrelated negated <?Ethiopic>} );
ok("\x[A9FA]"  ~~ m/^<-Ethiopic>$/, q{Match unrelated inverted <?Ethiopic>} );
ok("\x[A9FA]\c[ETHIOPIC SYLLABLE HA]" ~~ m/<?Ethiopic>/, q{Match unanchored <?Ethiopic>} );

# Georgian


ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<?Georgian>$/, q{Match <?Georgian>} );
ok(not "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<!Georgian>.$/, q{Don't match negated <?Georgian>} );
ok(not "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<-Georgian>$/, q{Don't match inverted <?Georgian>} );
ok(not "\x[BBC9]"  ~~ m/^<?Georgian>$/, q{Don't match unrelated <?Georgian>} );
ok("\x[BBC9]"  ~~ m/^<!Georgian>.$/, q{Match unrelated negated <?Georgian>} );
ok("\x[BBC9]"  ~~ m/^<-Georgian>$/, q{Match unrelated inverted <?Georgian>} );
ok("\x[BBC9]\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/<?Georgian>/, q{Match unanchored <?Georgian>} );

# Gothic


ok(not "\x[5888]"  ~~ m/^<?Gothic>$/, q{Don't match unrelated <?Gothic>} );
ok("\x[5888]"  ~~ m/^<!Gothic>.$/, q{Match unrelated negated <?Gothic>} );
ok("\x[5888]"  ~~ m/^<-Gothic>$/, q{Match unrelated inverted <?Gothic>} );

# Greek


ok("\c[GREEK LETTER SMALL CAPITAL GAMMA]" ~~ m/^<?Greek>$/, q{Match <?Greek>} );
ok(not "\c[GREEK LETTER SMALL CAPITAL GAMMA]" ~~ m/^<!Greek>.$/, q{Don't match negated <?Greek>} );
ok(not "\c[GREEK LETTER SMALL CAPITAL GAMMA]" ~~ m/^<-Greek>$/, q{Don't match inverted <?Greek>} );
ok(not "\c[ETHIOPIC SYLLABLE KEE]"  ~~ m/^<?Greek>$/, q{Don't match unrelated <?Greek>} );
ok("\c[ETHIOPIC SYLLABLE KEE]"  ~~ m/^<!Greek>.$/, q{Match unrelated negated <?Greek>} );
ok("\c[ETHIOPIC SYLLABLE KEE]"  ~~ m/^<-Greek>$/, q{Match unrelated inverted <?Greek>} );
ok("\c[ETHIOPIC SYLLABLE KEE]\c[GREEK LETTER SMALL CAPITAL GAMMA]" ~~ m/<?Greek>/, q{Match unanchored <?Greek>} );

# Gujarati


ok("\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<?Gujarati>$/, q{Match <?Gujarati>} );
ok(not "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<!Gujarati>.$/, q{Don't match negated <?Gujarati>} );
ok(not "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<-Gujarati>$/, q{Don't match inverted <?Gujarati>} );
ok(not "\x[D108]"  ~~ m/^<?Gujarati>$/, q{Don't match unrelated <?Gujarati>} );
ok("\x[D108]"  ~~ m/^<!Gujarati>.$/, q{Match unrelated negated <?Gujarati>} );
ok("\x[D108]"  ~~ m/^<-Gujarati>$/, q{Match unrelated inverted <?Gujarati>} );
ok("\x[D108]\c[GUJARATI SIGN CANDRABINDU]" ~~ m/<?Gujarati>/, q{Match unanchored <?Gujarati>} );

# Gurmukhi


ok("\c[GURMUKHI SIGN BINDI]" ~~ m/^<?Gurmukhi>$/, q{Match <?Gurmukhi>} );
ok(not "\c[GURMUKHI SIGN BINDI]" ~~ m/^<!Gurmukhi>.$/, q{Don't match negated <?Gurmukhi>} );
ok(not "\c[GURMUKHI SIGN BINDI]" ~~ m/^<-Gurmukhi>$/, q{Don't match inverted <?Gurmukhi>} );
ok(not "\x[5E05]"  ~~ m/^<?Gurmukhi>$/, q{Don't match unrelated <?Gurmukhi>} );
ok("\x[5E05]"  ~~ m/^<!Gurmukhi>.$/, q{Match unrelated negated <?Gurmukhi>} );
ok("\x[5E05]"  ~~ m/^<-Gurmukhi>$/, q{Match unrelated inverted <?Gurmukhi>} );
ok("\x[5E05]\c[GURMUKHI SIGN BINDI]" ~~ m/<?Gurmukhi>/, q{Match unanchored <?Gurmukhi>} );

# Han


ok("\c[CJK RADICAL REPEAT]" ~~ m/^<?Han>$/, q{Match <?Han>} );
ok(not "\c[CJK RADICAL REPEAT]" ~~ m/^<!Han>.$/, q{Don't match negated <?Han>} );
ok(not "\c[CJK RADICAL REPEAT]" ~~ m/^<-Han>$/, q{Don't match inverted <?Han>} );
ok(not "\c[CANADIAN SYLLABICS KAA]"  ~~ m/^<?Han>$/, q{Don't match unrelated <?Han>} );
ok("\c[CANADIAN SYLLABICS KAA]"  ~~ m/^<!Han>.$/, q{Match unrelated negated <?Han>} );
ok("\c[CANADIAN SYLLABICS KAA]"  ~~ m/^<-Han>$/, q{Match unrelated inverted <?Han>} );
ok("\c[CANADIAN SYLLABICS KAA]\c[CJK RADICAL REPEAT]" ~~ m/<?Han>/, q{Match unanchored <?Han>} );

# Hangul


ok("\x[AC00]" ~~ m/^<?Hangul>$/, q{Match <?Hangul>} );
ok(not "\x[AC00]" ~~ m/^<!Hangul>.$/, q{Don't match negated <?Hangul>} );
ok(not "\x[AC00]" ~~ m/^<-Hangul>$/, q{Don't match inverted <?Hangul>} );
ok(not "\x[9583]"  ~~ m/^<?Hangul>$/, q{Don't match unrelated <?Hangul>} );
ok("\x[9583]"  ~~ m/^<!Hangul>.$/, q{Match unrelated negated <?Hangul>} );
ok("\x[9583]"  ~~ m/^<-Hangul>$/, q{Match unrelated inverted <?Hangul>} );
ok("\x[9583]\x[AC00]" ~~ m/<?Hangul>/, q{Match unanchored <?Hangul>} );

# Hanunoo


ok("\c[HANUNOO LETTER A]" ~~ m/^<?Hanunoo>$/, q{Match <?Hanunoo>} );
ok(not "\c[HANUNOO LETTER A]" ~~ m/^<!Hanunoo>.$/, q{Don't match negated <?Hanunoo>} );
ok(not "\c[HANUNOO LETTER A]" ~~ m/^<-Hanunoo>$/, q{Don't match inverted <?Hanunoo>} );
ok(not "\x[7625]"  ~~ m/^<?Hanunoo>$/, q{Don't match unrelated <?Hanunoo>} );
ok("\x[7625]"  ~~ m/^<!Hanunoo>.$/, q{Match unrelated negated <?Hanunoo>} );
ok("\x[7625]"  ~~ m/^<-Hanunoo>$/, q{Match unrelated inverted <?Hanunoo>} );
ok("\x[7625]\c[HANUNOO LETTER A]" ~~ m/<?Hanunoo>/, q{Match unanchored <?Hanunoo>} );

# Hebrew


ok("\c[HEBREW LETTER ALEF]" ~~ m/^<?Hebrew>$/, q{Match <?Hebrew>} );
ok(not "\c[HEBREW LETTER ALEF]" ~~ m/^<!Hebrew>.$/, q{Don't match negated <?Hebrew>} );
ok(not "\c[HEBREW LETTER ALEF]" ~~ m/^<-Hebrew>$/, q{Don't match inverted <?Hebrew>} );
ok(not "\c[YI SYLLABLE SSIT]"  ~~ m/^<?Hebrew>$/, q{Don't match unrelated <?Hebrew>} );
ok("\c[YI SYLLABLE SSIT]"  ~~ m/^<!Hebrew>.$/, q{Match unrelated negated <?Hebrew>} );
ok("\c[YI SYLLABLE SSIT]"  ~~ m/^<-Hebrew>$/, q{Match unrelated inverted <?Hebrew>} );
ok("\c[YI SYLLABLE SSIT]\c[HEBREW LETTER ALEF]" ~~ m/<?Hebrew>/, q{Match unanchored <?Hebrew>} );

# Hiragana


ok("\c[HIRAGANA LETTER SMALL A]" ~~ m/^<?Hiragana>$/, q{Match <?Hiragana>} );
ok(not "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<!Hiragana>.$/, q{Don't match negated <?Hiragana>} );
ok(not "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<-Hiragana>$/, q{Don't match inverted <?Hiragana>} );
ok(not "\c[CANADIAN SYLLABICS Y]"  ~~ m/^<?Hiragana>$/, q{Don't match unrelated <?Hiragana>} );
ok("\c[CANADIAN SYLLABICS Y]"  ~~ m/^<!Hiragana>.$/, q{Match unrelated negated <?Hiragana>} );
ok("\c[CANADIAN SYLLABICS Y]"  ~~ m/^<-Hiragana>$/, q{Match unrelated inverted <?Hiragana>} );
ok("\c[CANADIAN SYLLABICS Y]\c[HIRAGANA LETTER SMALL A]" ~~ m/<?Hiragana>/, q{Match unanchored <?Hiragana>} );

# Inherited


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Inherited>$/, q{Match <?Inherited>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Inherited>.$/, q{Don't match negated <?Inherited>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Inherited>$/, q{Don't match inverted <?Inherited>} );
ok(not "\x[75FA]"  ~~ m/^<?Inherited>$/, q{Don't match unrelated <?Inherited>} );
ok("\x[75FA]"  ~~ m/^<!Inherited>.$/, q{Match unrelated negated <?Inherited>} );
ok("\x[75FA]"  ~~ m/^<-Inherited>$/, q{Match unrelated inverted <?Inherited>} );
ok("\x[75FA]\c[COMBINING GRAVE ACCENT]" ~~ m/<?Inherited>/, q{Match unanchored <?Inherited>} );

# Kannada


ok("\c[KANNADA SIGN ANUSVARA]" ~~ m/^<?Kannada>$/, q{Match <?Kannada>} );
ok(not "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<!Kannada>.$/, q{Don't match negated <?Kannada>} );
ok(not "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<-Kannada>$/, q{Don't match inverted <?Kannada>} );
ok(not "\x[C1DF]"  ~~ m/^<?Kannada>$/, q{Don't match unrelated <?Kannada>} );
ok("\x[C1DF]"  ~~ m/^<!Kannada>.$/, q{Match unrelated negated <?Kannada>} );
ok("\x[C1DF]"  ~~ m/^<-Kannada>$/, q{Match unrelated inverted <?Kannada>} );
ok("\x[C1DF]\c[KANNADA SIGN ANUSVARA]" ~~ m/<?Kannada>/, q{Match unanchored <?Kannada>} );

# Katakana


ok("\c[KATAKANA LETTER SMALL A]" ~~ m/^<?Katakana>$/, q{Match <?Katakana>} );
ok(not "\c[KATAKANA LETTER SMALL A]" ~~ m/^<!Katakana>.$/, q{Don't match negated <?Katakana>} );
ok(not "\c[KATAKANA LETTER SMALL A]" ~~ m/^<-Katakana>$/, q{Don't match inverted <?Katakana>} );
ok(not "\x[177A]"  ~~ m/^<?Katakana>$/, q{Don't match unrelated <?Katakana>} );
ok("\x[177A]"  ~~ m/^<!Katakana>.$/, q{Match unrelated negated <?Katakana>} );
ok("\x[177A]"  ~~ m/^<-Katakana>$/, q{Match unrelated inverted <?Katakana>} );
ok("\x[177A]\c[KATAKANA LETTER SMALL A]" ~~ m/<?Katakana>/, q{Match unanchored <?Katakana>} );

# Khmer


ok("\c[KHMER LETTER KA]" ~~ m/^<?Khmer>$/, q{Match <?Khmer>} );
ok(not "\c[KHMER LETTER KA]" ~~ m/^<!Khmer>.$/, q{Don't match negated <?Khmer>} );
ok(not "\c[KHMER LETTER KA]" ~~ m/^<-Khmer>$/, q{Don't match inverted <?Khmer>} );
ok(not "\c[GEORGIAN LETTER QAR]"  ~~ m/^<?Khmer>$/, q{Don't match unrelated <?Khmer>} );
ok("\c[GEORGIAN LETTER QAR]"  ~~ m/^<!Khmer>.$/, q{Match unrelated negated <?Khmer>} );
ok("\c[GEORGIAN LETTER QAR]"  ~~ m/^<-Khmer>$/, q{Match unrelated inverted <?Khmer>} );
ok("\c[GEORGIAN LETTER QAR]\c[KHMER LETTER KA]" ~~ m/<?Khmer>/, q{Match unanchored <?Khmer>} );

# Lao


ok("\c[LAO LETTER KO]" ~~ m/^<?Lao>$/, q{Match <?Lao>} );
ok(not "\c[LAO LETTER KO]" ~~ m/^<!Lao>.$/, q{Don't match negated <?Lao>} );
ok(not "\c[LAO LETTER KO]" ~~ m/^<-Lao>$/, q{Don't match inverted <?Lao>} );
ok(not "\x[3DA9]"  ~~ m/^<?Lao>$/, q{Don't match unrelated <?Lao>} );
ok("\x[3DA9]"  ~~ m/^<!Lao>.$/, q{Match unrelated negated <?Lao>} );
ok("\x[3DA9]"  ~~ m/^<-Lao>$/, q{Match unrelated inverted <?Lao>} );
ok(not "\x[3DA9]" ~~ m/^<?Lao>$/, q{Don't match related <?Lao>} );
ok("\x[3DA9]" ~~ m/^<!Lao>.$/, q{Match related negated <?Lao>} );
ok("\x[3DA9]" ~~ m/^<-Lao>$/, q{Match related inverted <?Lao>} );
ok("\x[3DA9]\x[3DA9]\c[LAO LETTER KO]" ~~ m/<?Lao>/, q{Match unanchored <?Lao>} );

# Latin


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<?Latin>$/, q{Match <?Latin>} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<!Latin>.$/, q{Don't match negated <?Latin>} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-Latin>$/, q{Don't match inverted <?Latin>} );
ok(not "\x[C549]"  ~~ m/^<?Latin>$/, q{Don't match unrelated <?Latin>} );
ok("\x[C549]"  ~~ m/^<!Latin>.$/, q{Match unrelated negated <?Latin>} );
ok("\x[C549]"  ~~ m/^<-Latin>$/, q{Match unrelated inverted <?Latin>} );
ok(not "\x[C549]" ~~ m/^<?Latin>$/, q{Don't match related <?Latin>} );
ok("\x[C549]" ~~ m/^<!Latin>.$/, q{Match related negated <?Latin>} );
ok("\x[C549]" ~~ m/^<-Latin>$/, q{Match related inverted <?Latin>} );
ok("\x[C549]\x[C549]\c[LATIN CAPITAL LETTER A]" ~~ m/<?Latin>/, q{Match unanchored <?Latin>} );

# Malayalam


ok("\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<?Malayalam>$/, q{Match <?Malayalam>} );
ok(not "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<!Malayalam>.$/, q{Don't match negated <?Malayalam>} );
ok(not "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<-Malayalam>$/, q{Don't match inverted <?Malayalam>} );
ok(not "\x[625C]"  ~~ m/^<?Malayalam>$/, q{Don't match unrelated <?Malayalam>} );
ok("\x[625C]"  ~~ m/^<!Malayalam>.$/, q{Match unrelated negated <?Malayalam>} );
ok("\x[625C]"  ~~ m/^<-Malayalam>$/, q{Match unrelated inverted <?Malayalam>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Malayalam>$/, q{Don't match related <?Malayalam>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Malayalam>.$/, q{Match related negated <?Malayalam>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Malayalam>$/, q{Match related inverted <?Malayalam>} );
ok("\x[625C]\c[COMBINING GRAVE ACCENT]\c[MALAYALAM SIGN ANUSVARA]" ~~ m/<?Malayalam>/, q{Match unanchored <?Malayalam>} );

# Mongolian


ok("\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<?Mongolian>$/, q{Match <?Mongolian>} );
ok(not "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<!Mongolian>.$/, q{Don't match negated <?Mongolian>} );
ok(not "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<-Mongolian>$/, q{Don't match inverted <?Mongolian>} );
ok(not "\x[5F93]"  ~~ m/^<?Mongolian>$/, q{Don't match unrelated <?Mongolian>} );
ok("\x[5F93]"  ~~ m/^<!Mongolian>.$/, q{Match unrelated negated <?Mongolian>} );
ok("\x[5F93]"  ~~ m/^<-Mongolian>$/, q{Match unrelated inverted <?Mongolian>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Mongolian>$/, q{Don't match related <?Mongolian>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Mongolian>.$/, q{Match related negated <?Mongolian>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Mongolian>$/, q{Match related inverted <?Mongolian>} );
ok("\x[5F93]\c[COMBINING GRAVE ACCENT]\c[MONGOLIAN DIGIT ZERO]" ~~ m/<?Mongolian>/, q{Match unanchored <?Mongolian>} );

# Myanmar


ok("\c[MYANMAR LETTER KA]" ~~ m/^<?Myanmar>$/, q{Match <?Myanmar>} );
ok(not "\c[MYANMAR LETTER KA]" ~~ m/^<!Myanmar>.$/, q{Don't match negated <?Myanmar>} );
ok(not "\c[MYANMAR LETTER KA]" ~~ m/^<-Myanmar>$/, q{Don't match inverted <?Myanmar>} );
ok(not "\x[649A]"  ~~ m/^<?Myanmar>$/, q{Don't match unrelated <?Myanmar>} );
ok("\x[649A]"  ~~ m/^<!Myanmar>.$/, q{Match unrelated negated <?Myanmar>} );
ok("\x[649A]"  ~~ m/^<-Myanmar>$/, q{Match unrelated inverted <?Myanmar>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Myanmar>$/, q{Don't match related <?Myanmar>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Myanmar>.$/, q{Match related negated <?Myanmar>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Myanmar>$/, q{Match related inverted <?Myanmar>} );
ok("\x[649A]\c[COMBINING GRAVE ACCENT]\c[MYANMAR LETTER KA]" ~~ m/<?Myanmar>/, q{Match unanchored <?Myanmar>} );

# Ogham


ok("\c[OGHAM LETTER BEITH]" ~~ m/^<?Ogham>$/, q{Match <?Ogham>} );
ok(not "\c[OGHAM LETTER BEITH]" ~~ m/^<!Ogham>.$/, q{Don't match negated <?Ogham>} );
ok(not "\c[OGHAM LETTER BEITH]" ~~ m/^<-Ogham>$/, q{Don't match inverted <?Ogham>} );
ok(not "\c[KATAKANA LETTER KA]"  ~~ m/^<?Ogham>$/, q{Don't match unrelated <?Ogham>} );
ok("\c[KATAKANA LETTER KA]"  ~~ m/^<!Ogham>.$/, q{Match unrelated negated <?Ogham>} );
ok("\c[KATAKANA LETTER KA]"  ~~ m/^<-Ogham>$/, q{Match unrelated inverted <?Ogham>} );
ok("\c[KATAKANA LETTER KA]\c[OGHAM LETTER BEITH]" ~~ m/<?Ogham>/, q{Match unanchored <?Ogham>} );

# OldItalic


ok(not "\x[8BB7]"  ~~ m/^<?OldItalic>$/, q{Don't match unrelated <?OldItalic>} );
ok("\x[8BB7]"  ~~ m/^<!OldItalic>.$/, q{Match unrelated negated <?OldItalic>} );
ok("\x[8BB7]"  ~~ m/^<-OldItalic>$/, q{Match unrelated inverted <?OldItalic>} );

# Oriya


ok("\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<?Oriya>$/, q{Match <?Oriya>} );
ok(not "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<!Oriya>.$/, q{Don't match negated <?Oriya>} );
ok(not "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<-Oriya>$/, q{Don't match inverted <?Oriya>} );
ok(not "\x[4292]"  ~~ m/^<?Oriya>$/, q{Don't match unrelated <?Oriya>} );
ok("\x[4292]"  ~~ m/^<!Oriya>.$/, q{Match unrelated negated <?Oriya>} );
ok("\x[4292]"  ~~ m/^<-Oriya>$/, q{Match unrelated inverted <?Oriya>} );
ok("\x[4292]\c[ORIYA SIGN CANDRABINDU]" ~~ m/<?Oriya>/, q{Match unanchored <?Oriya>} );

# Runic


ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<?Runic>$/, q{Match <?Runic>} );
ok(not "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<!Runic>.$/, q{Don't match negated <?Runic>} );
ok(not "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<-Runic>$/, q{Don't match inverted <?Runic>} );
ok(not "\x[9857]"  ~~ m/^<?Runic>$/, q{Don't match unrelated <?Runic>} );
ok("\x[9857]"  ~~ m/^<!Runic>.$/, q{Match unrelated negated <?Runic>} );
ok("\x[9857]"  ~~ m/^<-Runic>$/, q{Match unrelated inverted <?Runic>} );
ok("\x[9857]\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/<?Runic>/, q{Match unanchored <?Runic>} );

# Sinhala


ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<?Sinhala>$/, q{Match <?Sinhala>} );
ok(not "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<!Sinhala>.$/, q{Don't match negated <?Sinhala>} );
ok(not "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<-Sinhala>$/, q{Don't match inverted <?Sinhala>} );
ok(not "\x[5DF5]"  ~~ m/^<?Sinhala>$/, q{Don't match unrelated <?Sinhala>} );
ok("\x[5DF5]"  ~~ m/^<!Sinhala>.$/, q{Match unrelated negated <?Sinhala>} );
ok("\x[5DF5]"  ~~ m/^<-Sinhala>$/, q{Match unrelated inverted <?Sinhala>} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<?Sinhala>$/, q{Don't match related <?Sinhala>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<!Sinhala>.$/, q{Match related negated <?Sinhala>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-Sinhala>$/, q{Match related inverted <?Sinhala>} );
ok("\x[5DF5]\c[YI RADICAL QOT]\c[SINHALA SIGN ANUSVARAYA]" ~~ m/<?Sinhala>/, q{Match unanchored <?Sinhala>} );

# Syriac


ok("\c[SYRIAC LETTER ALAPH]" ~~ m/^<?Syriac>$/, q{Match <?Syriac>} );
ok(not "\c[SYRIAC LETTER ALAPH]" ~~ m/^<!Syriac>.$/, q{Don't match negated <?Syriac>} );
ok(not "\c[SYRIAC LETTER ALAPH]" ~~ m/^<-Syriac>$/, q{Don't match inverted <?Syriac>} );
ok(not "\x[57F0]"  ~~ m/^<?Syriac>$/, q{Don't match unrelated <?Syriac>} );
ok("\x[57F0]"  ~~ m/^<!Syriac>.$/, q{Match unrelated negated <?Syriac>} );
ok("\x[57F0]"  ~~ m/^<-Syriac>$/, q{Match unrelated inverted <?Syriac>} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<?Syriac>$/, q{Don't match related <?Syriac>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<!Syriac>.$/, q{Match related negated <?Syriac>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-Syriac>$/, q{Match related inverted <?Syriac>} );
ok("\x[57F0]\c[YI RADICAL QOT]\c[SYRIAC LETTER ALAPH]" ~~ m/<?Syriac>/, q{Match unanchored <?Syriac>} );

# Tagalog


ok("\c[TAGALOG LETTER A]" ~~ m/^<?Tagalog>$/, q{Match <?Tagalog>} );
ok(not "\c[TAGALOG LETTER A]" ~~ m/^<!Tagalog>.$/, q{Don't match negated <?Tagalog>} );
ok(not "\c[TAGALOG LETTER A]" ~~ m/^<-Tagalog>$/, q{Don't match inverted <?Tagalog>} );
ok(not "\x[3DE8]"  ~~ m/^<?Tagalog>$/, q{Don't match unrelated <?Tagalog>} );
ok("\x[3DE8]"  ~~ m/^<!Tagalog>.$/, q{Match unrelated negated <?Tagalog>} );
ok("\x[3DE8]"  ~~ m/^<-Tagalog>$/, q{Match unrelated inverted <?Tagalog>} );
ok("\x[3DE8]\c[TAGALOG LETTER A]" ~~ m/<?Tagalog>/, q{Match unanchored <?Tagalog>} );

# Tagbanwa


ok("\c[TAGBANWA LETTER A]" ~~ m/^<?Tagbanwa>$/, q{Match <?Tagbanwa>} );
ok(not "\c[TAGBANWA LETTER A]" ~~ m/^<!Tagbanwa>.$/, q{Don't match negated <?Tagbanwa>} );
ok(not "\c[TAGBANWA LETTER A]" ~~ m/^<-Tagbanwa>$/, q{Don't match inverted <?Tagbanwa>} );
ok(not "\c[CHEROKEE LETTER TLV]"  ~~ m/^<?Tagbanwa>$/, q{Don't match unrelated <?Tagbanwa>} );
ok("\c[CHEROKEE LETTER TLV]"  ~~ m/^<!Tagbanwa>.$/, q{Match unrelated negated <?Tagbanwa>} );
ok("\c[CHEROKEE LETTER TLV]"  ~~ m/^<-Tagbanwa>$/, q{Match unrelated inverted <?Tagbanwa>} );
ok("\c[CHEROKEE LETTER TLV]\c[TAGBANWA LETTER A]" ~~ m/<?Tagbanwa>/, q{Match unanchored <?Tagbanwa>} );

# Tamil


ok("\c[TAMIL SIGN ANUSVARA]" ~~ m/^<?Tamil>$/, q{Match <?Tamil>} );
ok(not "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<!Tamil>.$/, q{Don't match negated <?Tamil>} );
ok(not "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<-Tamil>$/, q{Don't match inverted <?Tamil>} );
ok(not "\x[8DF2]"  ~~ m/^<?Tamil>$/, q{Don't match unrelated <?Tamil>} );
ok("\x[8DF2]"  ~~ m/^<!Tamil>.$/, q{Match unrelated negated <?Tamil>} );
ok("\x[8DF2]"  ~~ m/^<-Tamil>$/, q{Match unrelated inverted <?Tamil>} );
ok("\x[8DF2]\c[TAMIL SIGN ANUSVARA]" ~~ m/<?Tamil>/, q{Match unanchored <?Tamil>} );

# Telugu


ok("\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<?Telugu>$/, q{Match <?Telugu>} );
ok(not "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<!Telugu>.$/, q{Don't match negated <?Telugu>} );
ok(not "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<-Telugu>$/, q{Don't match inverted <?Telugu>} );
ok(not "\x[8088]"  ~~ m/^<?Telugu>$/, q{Don't match unrelated <?Telugu>} );
ok("\x[8088]"  ~~ m/^<!Telugu>.$/, q{Match unrelated negated <?Telugu>} );
ok("\x[8088]"  ~~ m/^<-Telugu>$/, q{Match unrelated inverted <?Telugu>} );
ok("\x[8088]\c[TELUGU SIGN CANDRABINDU]" ~~ m/<?Telugu>/, q{Match unanchored <?Telugu>} );

# Thaana


ok("\c[THAANA LETTER HAA]" ~~ m/^<?Thaana>$/, q{Match <?Thaana>} );
ok(not "\c[THAANA LETTER HAA]" ~~ m/^<!Thaana>.$/, q{Don't match negated <?Thaana>} );
ok(not "\c[THAANA LETTER HAA]" ~~ m/^<-Thaana>$/, q{Don't match inverted <?Thaana>} );
ok(not "\x[5240]"  ~~ m/^<?Thaana>$/, q{Don't match unrelated <?Thaana>} );
ok("\x[5240]"  ~~ m/^<!Thaana>.$/, q{Match unrelated negated <?Thaana>} );
ok("\x[5240]"  ~~ m/^<-Thaana>$/, q{Match unrelated inverted <?Thaana>} );
ok("\x[5240]\c[THAANA LETTER HAA]" ~~ m/<?Thaana>/, q{Match unanchored <?Thaana>} );

# Thai


ok("\c[THAI CHARACTER KO KAI]" ~~ m/^<?Thai>$/, q{Match <?Thai>} );
ok(not "\c[THAI CHARACTER KO KAI]" ~~ m/^<!Thai>.$/, q{Don't match negated <?Thai>} );
ok(not "\c[THAI CHARACTER KO KAI]" ~~ m/^<-Thai>$/, q{Don't match inverted <?Thai>} );
ok(not "\x[CAD3]"  ~~ m/^<?Thai>$/, q{Don't match unrelated <?Thai>} );
ok("\x[CAD3]"  ~~ m/^<!Thai>.$/, q{Match unrelated negated <?Thai>} );
ok("\x[CAD3]"  ~~ m/^<-Thai>$/, q{Match unrelated inverted <?Thai>} );
ok("\x[CAD3]\c[THAI CHARACTER KO KAI]" ~~ m/<?Thai>/, q{Match unanchored <?Thai>} );

# Tibetan


ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<?Tibetan>$/, q{Match <?Tibetan>} );
ok(not "\c[TIBETAN SYLLABLE OM]" ~~ m/^<!Tibetan>.$/, q{Don't match negated <?Tibetan>} );
ok(not "\c[TIBETAN SYLLABLE OM]" ~~ m/^<-Tibetan>$/, q{Don't match inverted <?Tibetan>} );
ok(not "\x[8557]"  ~~ m/^<?Tibetan>$/, q{Don't match unrelated <?Tibetan>} );
ok("\x[8557]"  ~~ m/^<!Tibetan>.$/, q{Match unrelated negated <?Tibetan>} );
ok("\x[8557]"  ~~ m/^<-Tibetan>$/, q{Match unrelated inverted <?Tibetan>} );
ok("\x[8557]\c[TIBETAN SYLLABLE OM]" ~~ m/<?Tibetan>/, q{Match unanchored <?Tibetan>} );

# Yi


ok("\c[YI SYLLABLE IT]" ~~ m/^<?Yi>$/, q{Match <?Yi>} );
ok(not "\c[YI SYLLABLE IT]" ~~ m/^<!Yi>.$/, q{Don't match negated <?Yi>} );
ok(not "\c[YI SYLLABLE IT]" ~~ m/^<-Yi>$/, q{Don't match inverted <?Yi>} );
ok(not "\x[BCD0]"  ~~ m/^<?Yi>$/, q{Don't match unrelated <?Yi>} );
ok("\x[BCD0]"  ~~ m/^<!Yi>.$/, q{Match unrelated negated <?Yi>} );
ok("\x[BCD0]"  ~~ m/^<-Yi>$/, q{Match unrelated inverted <?Yi>} );
ok("\x[BCD0]\c[YI SYLLABLE IT]" ~~ m/<?Yi>/, q{Match unanchored <?Yi>} );

# ASCIIHexDigit


ok("\c[DIGIT ZERO]" ~~ m/^<?ASCIIHexDigit>$/, q{Match <?ASCIIHexDigit>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<!ASCIIHexDigit>.$/, q{Don't match negated <?ASCIIHexDigit>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<-ASCIIHexDigit>$/, q{Don't match inverted <?ASCIIHexDigit>} );
ok(not "\x[53BA]"  ~~ m/^<?ASCIIHexDigit>$/, q{Don't match unrelated <?ASCIIHexDigit>} );
ok("\x[53BA]"  ~~ m/^<!ASCIIHexDigit>.$/, q{Match unrelated negated <?ASCIIHexDigit>} );
ok("\x[53BA]"  ~~ m/^<-ASCIIHexDigit>$/, q{Match unrelated inverted <?ASCIIHexDigit>} );
ok("\x[53BA]\c[DIGIT ZERO]" ~~ m/<?ASCIIHexDigit>/, q{Match unanchored <?ASCIIHexDigit>} );

# Dash


ok("\c[HYPHEN-MINUS]" ~~ m/^<?Dash>$/, q{Match <?Dash>} );
ok(not "\c[HYPHEN-MINUS]" ~~ m/^<!Dash>.$/, q{Don't match negated <?Dash>} );
ok(not "\c[HYPHEN-MINUS]" ~~ m/^<-Dash>$/, q{Don't match inverted <?Dash>} );
ok(not "\x[53F7]"  ~~ m/^<?Dash>$/, q{Don't match unrelated <?Dash>} );
ok("\x[53F7]"  ~~ m/^<!Dash>.$/, q{Match unrelated negated <?Dash>} );
ok("\x[53F7]"  ~~ m/^<-Dash>$/, q{Match unrelated inverted <?Dash>} );
ok("\x[53F7]\c[HYPHEN-MINUS]" ~~ m/<?Dash>/, q{Match unanchored <?Dash>} );

# Diacritic


ok("\c[MODIFIER LETTER CAPITAL A]" ~~ m/^<?Diacritic>$/, q{Match <?Diacritic>} );
ok(not "\c[MODIFIER LETTER CAPITAL A]" ~~ m/^<!Diacritic>.$/, q{Don't match negated <?Diacritic>} );
ok(not "\c[MODIFIER LETTER CAPITAL A]" ~~ m/^<-Diacritic>$/, q{Don't match inverted <?Diacritic>} );
ok(not "\x[1BCD]"  ~~ m/^<?Diacritic>$/, q{Don't match unrelated <?Diacritic>} );
ok("\x[1BCD]"  ~~ m/^<!Diacritic>.$/, q{Match unrelated negated <?Diacritic>} );
ok("\x[1BCD]"  ~~ m/^<-Diacritic>$/, q{Match unrelated inverted <?Diacritic>} );
ok("\x[1BCD]\c[MODIFIER LETTER CAPITAL A]" ~~ m/<?Diacritic>/, q{Match unanchored <?Diacritic>} );

# Extender


ok("\c[MIDDLE DOT]" ~~ m/^<?Extender>$/, q{Match <?Extender>} );
ok(not "\c[MIDDLE DOT]" ~~ m/^<!Extender>.$/, q{Don't match negated <?Extender>} );
ok(not "\c[MIDDLE DOT]" ~~ m/^<-Extender>$/, q{Don't match inverted <?Extender>} );
ok(not "\x[3A18]"  ~~ m/^<?Extender>$/, q{Don't match unrelated <?Extender>} );
ok("\x[3A18]"  ~~ m/^<!Extender>.$/, q{Match unrelated negated <?Extender>} );
ok("\x[3A18]"  ~~ m/^<-Extender>$/, q{Match unrelated inverted <?Extender>} );
ok("\x[3A18]\c[MIDDLE DOT]" ~~ m/<?Extender>/, q{Match unanchored <?Extender>} );

# GraphemeLink


ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<?GraphemeLink>$/, q{Match <?GraphemeLink>} );
ok(not "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<!GraphemeLink>.$/, q{Don't match negated <?GraphemeLink>} );
ok(not "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<-GraphemeLink>$/, q{Don't match inverted <?GraphemeLink>} );
ok(not "\x[4989]"  ~~ m/^<?GraphemeLink>$/, q{Don't match unrelated <?GraphemeLink>} );
ok("\x[4989]"  ~~ m/^<!GraphemeLink>.$/, q{Match unrelated negated <?GraphemeLink>} );
ok("\x[4989]"  ~~ m/^<-GraphemeLink>$/, q{Match unrelated inverted <?GraphemeLink>} );
ok("\x[4989]\c[COMBINING GRAPHEME JOINER]" ~~ m/<?GraphemeLink>/, q{Match unanchored <?GraphemeLink>} );

# HexDigit


ok("\c[DIGIT ZERO]" ~~ m/^<?HexDigit>$/, q{Match <?HexDigit>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<!HexDigit>.$/, q{Don't match negated <?HexDigit>} );
ok(not "\c[DIGIT ZERO]" ~~ m/^<-HexDigit>$/, q{Don't match inverted <?HexDigit>} );
ok(not "\x[6292]"  ~~ m/^<?HexDigit>$/, q{Don't match unrelated <?HexDigit>} );
ok("\x[6292]"  ~~ m/^<!HexDigit>.$/, q{Match unrelated negated <?HexDigit>} );
ok("\x[6292]"  ~~ m/^<-HexDigit>$/, q{Match unrelated inverted <?HexDigit>} );
ok("\x[6292]\c[DIGIT ZERO]" ~~ m/<?HexDigit>/, q{Match unanchored <?HexDigit>} );

# Hyphen

ok("\c[KATAKANA MIDDLE DOT]" ~~ m/^<?Hyphen>$/, q{Match <?Hyphen>} );
ok(not "\c[KATAKANA MIDDLE DOT]" ~~ m/^<!Hyphen>.$/, q{Don't match negated <?Hyphen>} );
ok(not "\c[KATAKANA MIDDLE DOT]" ~~ m/^<-Hyphen>$/, q{Don't match inverted <?Hyphen>} );
ok(not "\c[BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE]"  ~~ m/^<?Hyphen>$/, q{Don't match unrelated <?Hyphen>} );
ok("\c[BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE]"  ~~ m/^<!Hyphen>.$/, q{Match unrelated negated <?Hyphen>} );
ok("\c[BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE]"  ~~ m/^<-Hyphen>$/, q{Match unrelated inverted <?Hyphen>} );
ok("\c[BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE]\c[KATAKANA MIDDLE DOT]" ~~ m/<?Hyphen>/, q{Match unanchored <?Hyphen>} );

# Ideographic


ok("\x[8AB0]" ~~ m/^<?Ideographic>$/, q{Match <?Ideographic>} );
ok(not "\x[8AB0]" ~~ m/^<!Ideographic>.$/, q{Don't match negated <?Ideographic>} );
ok(not "\x[8AB0]" ~~ m/^<-Ideographic>$/, q{Don't match inverted <?Ideographic>} );
ok(not "\x[9FA6]"  ~~ m/^<?Ideographic>$/, q{Don't match unrelated <?Ideographic>} );
ok("\x[9FA6]"  ~~ m/^<!Ideographic>.$/, q{Match unrelated negated <?Ideographic>} );
ok("\x[9FA6]"  ~~ m/^<-Ideographic>$/, q{Match unrelated inverted <?Ideographic>} );
ok("\x[9FA6]\x[8AB0]" ~~ m/<?Ideographic>/, q{Match unanchored <?Ideographic>} );

# IDSBinaryOperator


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<?IDSBinaryOperator>$/, q{Match <?IDSBinaryOperator>} );
ok(not "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<!IDSBinaryOperator>.$/, q{Don't match negated <?IDSBinaryOperator>} );
ok(not "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<-IDSBinaryOperator>$/, q{Don't match inverted <?IDSBinaryOperator>} );
ok(not "\x[59E9]"  ~~ m/^<?IDSBinaryOperator>$/, q{Don't match unrelated <?IDSBinaryOperator>} );
ok("\x[59E9]"  ~~ m/^<!IDSBinaryOperator>.$/, q{Match unrelated negated <?IDSBinaryOperator>} );
ok("\x[59E9]"  ~~ m/^<-IDSBinaryOperator>$/, q{Match unrelated inverted <?IDSBinaryOperator>} );
ok("\x[59E9]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/<?IDSBinaryOperator>/, q{Match unanchored <?IDSBinaryOperator>} );

# IDSTrinaryOperator


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<?IDSTrinaryOperator>$/, q{Match <?IDSTrinaryOperator>} );
ok(not "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<!IDSTrinaryOperator>.$/, q{Don't match negated <?IDSTrinaryOperator>} );
ok(not "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<-IDSTrinaryOperator>$/, q{Don't match inverted <?IDSTrinaryOperator>} );
ok(not "\x[9224]"  ~~ m/^<?IDSTrinaryOperator>$/, q{Don't match unrelated <?IDSTrinaryOperator>} );
ok("\x[9224]"  ~~ m/^<!IDSTrinaryOperator>.$/, q{Match unrelated negated <?IDSTrinaryOperator>} );
ok("\x[9224]"  ~~ m/^<-IDSTrinaryOperator>$/, q{Match unrelated inverted <?IDSTrinaryOperator>} );
ok("\x[9224]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/<?IDSTrinaryOperator>/, q{Match unanchored <?IDSTrinaryOperator>} );

# JoinControl


ok("\c[ZERO WIDTH NON-JOINER]" ~~ m/^<?JoinControl>$/, q{Match <?JoinControl>} );
ok(not "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<!JoinControl>.$/, q{Don't match negated <?JoinControl>} );
ok(not "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<-JoinControl>$/, q{Don't match inverted <?JoinControl>} );
ok(not "\c[BENGALI LETTER DDHA]"  ~~ m/^<?JoinControl>$/, q{Don't match unrelated <?JoinControl>} );
ok("\c[BENGALI LETTER DDHA]"  ~~ m/^<!JoinControl>.$/, q{Match unrelated negated <?JoinControl>} );
ok("\c[BENGALI LETTER DDHA]"  ~~ m/^<-JoinControl>$/, q{Match unrelated inverted <?JoinControl>} );
ok("\c[BENGALI LETTER DDHA]\c[ZERO WIDTH NON-JOINER]" ~~ m/<?JoinControl>/, q{Match unanchored <?JoinControl>} );

# LogicalOrderException


ok("\c[THAI CHARACTER SARA E]" ~~ m/^<?LogicalOrderException>$/, q{Match <?LogicalOrderException>} );
ok(not "\c[THAI CHARACTER SARA E]" ~~ m/^<!LogicalOrderException>.$/, q{Don't match negated <?LogicalOrderException>} );
ok(not "\c[THAI CHARACTER SARA E]" ~~ m/^<-LogicalOrderException>$/, q{Don't match inverted <?LogicalOrderException>} );
ok(not "\x[857B]"  ~~ m/^<?LogicalOrderException>$/, q{Don't match unrelated <?LogicalOrderException>} );
ok("\x[857B]"  ~~ m/^<!LogicalOrderException>.$/, q{Match unrelated negated <?LogicalOrderException>} );
ok("\x[857B]"  ~~ m/^<-LogicalOrderException>$/, q{Match unrelated inverted <?LogicalOrderException>} );
ok(not "\x[857B]" ~~ m/^<?LogicalOrderException>$/, q{Don't match related <?LogicalOrderException>} );
ok("\x[857B]" ~~ m/^<!LogicalOrderException>.$/, q{Match related negated <?LogicalOrderException>} );
ok("\x[857B]" ~~ m/^<-LogicalOrderException>$/, q{Match related inverted <?LogicalOrderException>} );
ok("\x[857B]\x[857B]\c[THAI CHARACTER SARA E]" ~~ m/<?LogicalOrderException>/, q{Match unanchored <?LogicalOrderException>} );

# NoncharacterCodePoint


ok(not "\c[LATIN LETTER REVERSED GLOTTAL STOP WITH STROKE]"  ~~ m/^<?NoncharacterCodePoint>$/, q{Don't match unrelated <?NoncharacterCodePoint>} );
ok("\c[LATIN LETTER REVERSED GLOTTAL STOP WITH STROKE]"  ~~ m/^<!NoncharacterCodePoint>.$/, q{Match unrelated negated <?NoncharacterCodePoint>} );
ok("\c[LATIN LETTER REVERSED GLOTTAL STOP WITH STROKE]"  ~~ m/^<-NoncharacterCodePoint>$/, q{Match unrelated inverted <?NoncharacterCodePoint>} );
ok(not "\c[ARABIC-INDIC DIGIT ZERO]" ~~ m/^<?NoncharacterCodePoint>$/, q{Don't match related <?NoncharacterCodePoint>} );
ok("\c[ARABIC-INDIC DIGIT ZERO]" ~~ m/^<!NoncharacterCodePoint>.$/, q{Match related negated <?NoncharacterCodePoint>} );
ok("\c[ARABIC-INDIC DIGIT ZERO]" ~~ m/^<-NoncharacterCodePoint>$/, q{Match related inverted <?NoncharacterCodePoint>} );

# OtherAlphabetic


ok("\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<?OtherAlphabetic>$/, q{Match <?OtherAlphabetic>} );
ok(not "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<!OtherAlphabetic>.$/, q{Don't match negated <?OtherAlphabetic>} );
ok(not "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<-OtherAlphabetic>$/, q{Don't match inverted <?OtherAlphabetic>} );
ok(not "\x[413C]"  ~~ m/^<?OtherAlphabetic>$/, q{Don't match unrelated <?OtherAlphabetic>} );
ok("\x[413C]"  ~~ m/^<!OtherAlphabetic>.$/, q{Match unrelated negated <?OtherAlphabetic>} );
ok("\x[413C]"  ~~ m/^<-OtherAlphabetic>$/, q{Match unrelated inverted <?OtherAlphabetic>} );
ok("\x[413C]\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/<?OtherAlphabetic>/, q{Match unanchored <?OtherAlphabetic>} );

# OtherDefaultIgnorableCodePoint


ok("\c[HANGUL FILLER]" ~~ m/^<?OtherDefaultIgnorableCodePoint>$/, q{Match <?OtherDefaultIgnorableCodePoint>} );
ok(not "\c[HANGUL FILLER]" ~~ m/^<!OtherDefaultIgnorableCodePoint>.$/, q{Don't match negated <?OtherDefaultIgnorableCodePoint>} );
ok(not "\c[HANGUL FILLER]" ~~ m/^<-OtherDefaultIgnorableCodePoint>$/, q{Don't match inverted <?OtherDefaultIgnorableCodePoint>} );
ok(not "\c[VERTICAL BAR DOUBLE LEFT TURNSTILE]"  ~~ m/^<?OtherDefaultIgnorableCodePoint>$/, q{Don't match unrelated <?OtherDefaultIgnorableCodePoint>} );
ok("\c[VERTICAL BAR DOUBLE LEFT TURNSTILE]"  ~~ m/^<!OtherDefaultIgnorableCodePoint>.$/, q{Match unrelated negated <?OtherDefaultIgnorableCodePoint>} );
ok("\c[VERTICAL BAR DOUBLE LEFT TURNSTILE]"  ~~ m/^<-OtherDefaultIgnorableCodePoint>$/, q{Match unrelated inverted <?OtherDefaultIgnorableCodePoint>} );
ok("\c[VERTICAL BAR DOUBLE LEFT TURNSTILE]\c[HANGUL FILLER]" ~~ m/<?OtherDefaultIgnorableCodePoint>/, q{Match unanchored <?OtherDefaultIgnorableCodePoint>} );

# OtherGraphemeExtend


ok("\c[BENGALI VOWEL SIGN AA]" ~~ m/^<?OtherGraphemeExtend>$/, q{Match <?OtherGraphemeExtend>} );
ok(not "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<!OtherGraphemeExtend>.$/, q{Don't match negated <?OtherGraphemeExtend>} );
ok(not "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<-OtherGraphemeExtend>$/, q{Don't match inverted <?OtherGraphemeExtend>} );
ok(not "\c[APL FUNCTIONAL SYMBOL EPSILON UNDERBAR]"  ~~ m/^<?OtherGraphemeExtend>$/, q{Don't match unrelated <?OtherGraphemeExtend>} );
ok("\c[APL FUNCTIONAL SYMBOL EPSILON UNDERBAR]"  ~~ m/^<!OtherGraphemeExtend>.$/, q{Match unrelated negated <?OtherGraphemeExtend>} );
ok("\c[APL FUNCTIONAL SYMBOL EPSILON UNDERBAR]"  ~~ m/^<-OtherGraphemeExtend>$/, q{Match unrelated inverted <?OtherGraphemeExtend>} );
ok("\c[APL FUNCTIONAL SYMBOL EPSILON UNDERBAR]\c[BENGALI VOWEL SIGN AA]" ~~ m/<?OtherGraphemeExtend>/, q{Match unanchored <?OtherGraphemeExtend>} );

# OtherLowercase


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<?OtherLowercase>$/, q{Match <?OtherLowercase>} );
ok(not "\c[MODIFIER LETTER SMALL H]" ~~ m/^<!OtherLowercase>.$/, q{Don't match negated <?OtherLowercase>} );
ok(not "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-OtherLowercase>$/, q{Don't match inverted <?OtherLowercase>} );
ok(not "\c[HANGUL LETTER NIEUN-CIEUC]"  ~~ m/^<?OtherLowercase>$/, q{Don't match unrelated <?OtherLowercase>} );
ok("\c[HANGUL LETTER NIEUN-CIEUC]"  ~~ m/^<!OtherLowercase>.$/, q{Match unrelated negated <?OtherLowercase>} );
ok("\c[HANGUL LETTER NIEUN-CIEUC]"  ~~ m/^<-OtherLowercase>$/, q{Match unrelated inverted <?OtherLowercase>} );
ok("\c[HANGUL LETTER NIEUN-CIEUC]\c[MODIFIER LETTER SMALL H]" ~~ m/<?OtherLowercase>/, q{Match unanchored <?OtherLowercase>} );

# OtherMath


ok("\c[LEFT PARENTHESIS]" ~~ m/^<?OtherMath>$/, q{Match <?OtherMath>} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<!OtherMath>.$/, q{Don't match negated <?OtherMath>} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<-OtherMath>$/, q{Don't match inverted <?OtherMath>} );
ok(not "\x[B43A]"  ~~ m/^<?OtherMath>$/, q{Don't match unrelated <?OtherMath>} );
ok("\x[B43A]"  ~~ m/^<!OtherMath>.$/, q{Match unrelated negated <?OtherMath>} );
ok("\x[B43A]"  ~~ m/^<-OtherMath>$/, q{Match unrelated inverted <?OtherMath>} );
ok("\x[B43A]\c[LEFT PARENTHESIS]" ~~ m/<?OtherMath>/, q{Match unanchored <?OtherMath>} );

# OtherUppercase


ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<?OtherUppercase>$/, q{Match <?OtherUppercase>} );
ok(not "\c[ROMAN NUMERAL ONE]" ~~ m/^<!OtherUppercase>.$/, q{Don't match negated <?OtherUppercase>} );
ok(not "\c[ROMAN NUMERAL ONE]" ~~ m/^<-OtherUppercase>$/, q{Don't match inverted <?OtherUppercase>} );
ok(not "\x[D246]"  ~~ m/^<?OtherUppercase>$/, q{Don't match unrelated <?OtherUppercase>} );
ok("\x[D246]"  ~~ m/^<!OtherUppercase>.$/, q{Match unrelated negated <?OtherUppercase>} );
ok("\x[D246]"  ~~ m/^<-OtherUppercase>$/, q{Match unrelated inverted <?OtherUppercase>} );
ok("\x[D246]\c[ROMAN NUMERAL ONE]" ~~ m/<?OtherUppercase>/, q{Match unanchored <?OtherUppercase>} );

# QuotationMark


ok("\c[QUOTATION MARK]" ~~ m/^<?QuotationMark>$/, q{Match <?QuotationMark>} );
ok(not "\c[QUOTATION MARK]" ~~ m/^<!QuotationMark>.$/, q{Don't match negated <?QuotationMark>} );
ok(not "\c[QUOTATION MARK]" ~~ m/^<-QuotationMark>$/, q{Don't match inverted <?QuotationMark>} );
ok(not "\x[C890]"  ~~ m/^<?QuotationMark>$/, q{Don't match unrelated <?QuotationMark>} );
ok("\x[C890]"  ~~ m/^<!QuotationMark>.$/, q{Match unrelated negated <?QuotationMark>} );
ok("\x[C890]"  ~~ m/^<-QuotationMark>$/, q{Match unrelated inverted <?QuotationMark>} );
ok("\x[C890]\c[QUOTATION MARK]" ~~ m/<?QuotationMark>/, q{Match unanchored <?QuotationMark>} );

# Radical


ok("\c[CJK RADICAL REPEAT]" ~~ m/^<?Radical>$/, q{Match <?Radical>} );
ok(not "\c[CJK RADICAL REPEAT]" ~~ m/^<!Radical>.$/, q{Don't match negated <?Radical>} );
ok(not "\c[CJK RADICAL REPEAT]" ~~ m/^<-Radical>$/, q{Don't match inverted <?Radical>} );
ok(not "\c[HANGUL JONGSEONG CHIEUCH]"  ~~ m/^<?Radical>$/, q{Don't match unrelated <?Radical>} );
ok("\c[HANGUL JONGSEONG CHIEUCH]"  ~~ m/^<!Radical>.$/, q{Match unrelated negated <?Radical>} );
ok("\c[HANGUL JONGSEONG CHIEUCH]"  ~~ m/^<-Radical>$/, q{Match unrelated inverted <?Radical>} );
ok("\c[HANGUL JONGSEONG CHIEUCH]\c[CJK RADICAL REPEAT]" ~~ m/<?Radical>/, q{Match unanchored <?Radical>} );

# SoftDotted


ok("\c[LATIN SMALL LETTER I]" ~~ m/^<?SoftDotted>$/, q{Match <?SoftDotted>} );
ok(not "\c[LATIN SMALL LETTER I]" ~~ m/^<!SoftDotted>.$/, q{Don't match negated <?SoftDotted>} );
ok(not "\c[LATIN SMALL LETTER I]" ~~ m/^<-SoftDotted>$/, q{Don't match inverted <?SoftDotted>} );
ok(not "\x[ADEF]"  ~~ m/^<?SoftDotted>$/, q{Don't match unrelated <?SoftDotted>} );
ok("\x[ADEF]"  ~~ m/^<!SoftDotted>.$/, q{Match unrelated negated <?SoftDotted>} );
ok("\x[ADEF]"  ~~ m/^<-SoftDotted>$/, q{Match unrelated inverted <?SoftDotted>} );
ok(not "\c[DOLLAR SIGN]" ~~ m/^<?SoftDotted>$/, q{Don't match related <?SoftDotted>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<!SoftDotted>.$/, q{Match related negated <?SoftDotted>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-SoftDotted>$/, q{Match related inverted <?SoftDotted>} );
ok("\x[ADEF]\c[DOLLAR SIGN]\c[LATIN SMALL LETTER I]" ~~ m/<?SoftDotted>/, q{Match unanchored <?SoftDotted>} );

# TerminalPunctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<?TerminalPunctuation>$/, q{Match <?TerminalPunctuation>} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<!TerminalPunctuation>.$/, q{Don't match negated <?TerminalPunctuation>} );
ok(not "\c[EXCLAMATION MARK]" ~~ m/^<-TerminalPunctuation>$/, q{Don't match inverted <?TerminalPunctuation>} );
ok(not "\x[3C9D]"  ~~ m/^<?TerminalPunctuation>$/, q{Don't match unrelated <?TerminalPunctuation>} );
ok("\x[3C9D]"  ~~ m/^<!TerminalPunctuation>.$/, q{Match unrelated negated <?TerminalPunctuation>} );
ok("\x[3C9D]"  ~~ m/^<-TerminalPunctuation>$/, q{Match unrelated inverted <?TerminalPunctuation>} );
ok("\x[3C9D]\c[EXCLAMATION MARK]" ~~ m/<?TerminalPunctuation>/, q{Match unanchored <?TerminalPunctuation>} );

# UnifiedIdeograph


ok("\x[7896]" ~~ m/^<?UnifiedIdeograph>$/, q{Match <?UnifiedIdeograph>} );
ok(not "\x[7896]" ~~ m/^<!UnifiedIdeograph>.$/, q{Don't match negated <?UnifiedIdeograph>} );
ok(not "\x[7896]" ~~ m/^<-UnifiedIdeograph>$/, q{Don't match inverted <?UnifiedIdeograph>} );
ok(not "\x[9FA6]"  ~~ m/^<?UnifiedIdeograph>$/, q{Don't match unrelated <?UnifiedIdeograph>} );
ok("\x[9FA6]"  ~~ m/^<!UnifiedIdeograph>.$/, q{Match unrelated negated <?UnifiedIdeograph>} );
ok("\x[9FA6]"  ~~ m/^<-UnifiedIdeograph>$/, q{Match unrelated inverted <?UnifiedIdeograph>} );
ok("\x[9FA6]\x[7896]" ~~ m/<?UnifiedIdeograph>/, q{Match unanchored <?UnifiedIdeograph>} );

# WhiteSpace


ok("\c[CHARACTER TABULATION]" ~~ m/^<?WhiteSpace>$/, q{Match <?WhiteSpace>} );
ok(not "\c[CHARACTER TABULATION]" ~~ m/^<!WhiteSpace>.$/, q{Don't match negated <?WhiteSpace>} );
ok(not "\c[CHARACTER TABULATION]" ~~ m/^<-WhiteSpace>$/, q{Don't match inverted <?WhiteSpace>} );
ok(not "\x[6358]"  ~~ m/^<?WhiteSpace>$/, q{Don't match unrelated <?WhiteSpace>} );
ok("\x[6358]"  ~~ m/^<!WhiteSpace>.$/, q{Match unrelated negated <?WhiteSpace>} );
ok("\x[6358]"  ~~ m/^<-WhiteSpace>$/, q{Match unrelated inverted <?WhiteSpace>} );
ok("\x[6358]\c[CHARACTER TABULATION]" ~~ m/<?WhiteSpace>/, q{Match unanchored <?WhiteSpace>} );

# Alphabetic      # Lu + Ll + Lt + Lm + Lo + OtherAlphabetic


ok("\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<?Alphabetic>$/, q{Match (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(not "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<!Alphabetic>.$/, q{Don't match negated (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(not "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<-Alphabetic>$/, q{Don't match inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(not "\x[0855]"  ~~ m/^<?Alphabetic>$/, q{Don't match unrelated (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[0855]"  ~~ m/^<!Alphabetic>.$/, q{Match unrelated negated (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[0855]"  ~~ m/^<-Alphabetic>$/, q{Match unrelated inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[0855]\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/<?Alphabetic>/, q{Match unanchored (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );

# Lowercase       # Ll + OtherLowercase


ok("\c[LATIN SMALL LETTER A]" ~~ m/^<?Lowercase>$/, q{Match (Ll + OtherLowercase)} );
ok(not "\c[LATIN SMALL LETTER A]" ~~ m/^<!Lowercase>.$/, q{Don't match negated (Ll + OtherLowercase)} );
ok(not "\c[LATIN SMALL LETTER A]" ~~ m/^<-Lowercase>$/, q{Don't match inverted (Ll + OtherLowercase)} );
ok(not "\x[6220]"  ~~ m/^<?Lowercase>$/, q{Don't match unrelated (Ll + OtherLowercase)} );
ok("\x[6220]"  ~~ m/^<!Lowercase>.$/, q{Match unrelated negated (Ll + OtherLowercase)} );
ok("\x[6220]"  ~~ m/^<-Lowercase>$/, q{Match unrelated inverted (Ll + OtherLowercase)} );
ok(not "\x[6220]" ~~ m/^<?Lowercase>$/, q{Don't match related (Ll + OtherLowercase)} );
ok("\x[6220]" ~~ m/^<!Lowercase>.$/, q{Match related negated (Ll + OtherLowercase)} );
ok("\x[6220]" ~~ m/^<-Lowercase>$/, q{Match related inverted (Ll + OtherLowercase)} );
ok("\x[6220]\x[6220]\c[LATIN SMALL LETTER A]" ~~ m/<?Lowercase>/, q{Match unanchored (Ll + OtherLowercase)} );

# Uppercase       # Lu + OtherUppercase


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<?Uppercase>$/, q{Match (Lu + OtherUppercase)} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<!Uppercase>.$/, q{Don't match negated (Lu + OtherUppercase)} );
ok(not "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-Uppercase>$/, q{Don't match inverted (Lu + OtherUppercase)} );
ok(not "\x[C080]"  ~~ m/^<?Uppercase>$/, q{Don't match unrelated (Lu + OtherUppercase)} );
ok("\x[C080]"  ~~ m/^<!Uppercase>.$/, q{Match unrelated negated (Lu + OtherUppercase)} );
ok("\x[C080]"  ~~ m/^<-Uppercase>$/, q{Match unrelated inverted (Lu + OtherUppercase)} );
ok("\x[C080]\c[LATIN CAPITAL LETTER A]" ~~ m/<?Uppercase>/, q{Match unanchored (Lu + OtherUppercase)} );

# Math            # Sm + OtherMath


ok("\c[LEFT PARENTHESIS]" ~~ m/^<?Math>$/, q{Match (Sm + OtherMath)} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<!Math>.$/, q{Don't match negated (Sm + OtherMath)} );
ok(not "\c[LEFT PARENTHESIS]" ~~ m/^<-Math>$/, q{Don't match inverted (Sm + OtherMath)} );
ok(not "\x[D4D2]"  ~~ m/^<?Math>$/, q{Don't match unrelated (Sm + OtherMath)} );
ok("\x[D4D2]"  ~~ m/^<!Math>.$/, q{Match unrelated negated (Sm + OtherMath)} );
ok("\x[D4D2]"  ~~ m/^<-Math>$/, q{Match unrelated inverted (Sm + OtherMath)} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<?Math>$/, q{Don't match related (Sm + OtherMath)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<!Math>.$/, q{Match related negated (Sm + OtherMath)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-Math>$/, q{Match related inverted (Sm + OtherMath)} );
ok("\x[D4D2]\c[COMBINING GRAVE ACCENT]\c[LEFT PARENTHESIS]" ~~ m/<?Math>/, q{Match unanchored (Sm + OtherMath)} );

# ID_Start        # Lu + Ll + Lt + Lm + Lo + Nl


ok("\x[C276]" ~~ m/^<?ID_Start>$/, q{Match (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(not "\x[C276]" ~~ m/^<!ID_Start>.$/, q{Don't match negated (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(not "\x[C276]" ~~ m/^<-ID_Start>$/, q{Don't match inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(not "\x[D7A4]"  ~~ m/^<?ID_Start>$/, q{Don't match unrelated (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[D7A4]"  ~~ m/^<!ID_Start>.$/, q{Match unrelated negated (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[D7A4]"  ~~ m/^<-ID_Start>$/, q{Match unrelated inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[D7A4]\x[C276]" ~~ m/<?ID_Start>/, q{Match unanchored (Lu + Ll + Lt + Lm + Lo + Nl)} );

# ID_Continue     # ID_Start + Mn + Mc + Nd + Pc


ok("\x[949B]" ~~ m/^<?ID_Continue>$/, q{Match (ID_Start + Mn + Mc + Nd + Pc)} );
ok(not "\x[949B]" ~~ m/^<!ID_Continue>.$/, q{Don't match negated (ID_Start + Mn + Mc + Nd + Pc)} );
ok(not "\x[949B]" ~~ m/^<-ID_Continue>$/, q{Don't match inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok(not "\x[9FA6]"  ~~ m/^<?ID_Continue>$/, q{Don't match unrelated (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\x[9FA6]"  ~~ m/^<!ID_Continue>.$/, q{Match unrelated negated (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\x[9FA6]"  ~~ m/^<-ID_Continue>$/, q{Match unrelated inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\x[9FA6]\x[949B]" ~~ m/<?ID_Continue>/, q{Match unanchored (ID_Start + Mn + Mc + Nd + Pc)} );

# Any             # Any character


ok("\x[C709]" ~~ m/^<?Any>$/, q{Match (Any character)} );
ok(not "\x[C709]" ~~ m/^<!Any>.$/, q{Don't match negated (Any character)} );
ok(not "\x[C709]" ~~ m/^<-Any>$/, q{Don't match inverted (Any character)} );
ok("\x[C709]" ~~ m/<?Any>/, q{Match unanchored (Any character)} );

# Assigned        # Any non-Cn character (i.e. synonym for \P{Cn})


ok("\x[C99D]" ~~ m/^<?Assigned>$/, q{Match (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(not "\x[C99D]" ~~ m/^<!Assigned>.$/, q{Don't match negated (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(not "\x[C99D]" ~~ m/^<-Assigned>$/, q{Don't match inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(not "\x[D7A4]"  ~~ m/^<?Assigned>$/, q{Don't match unrelated (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[D7A4]"  ~~ m/^<!Assigned>.$/, q{Match unrelated negated (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[D7A4]"  ~~ m/^<-Assigned>$/, q{Match unrelated inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[D7A4]\x[C99D]" ~~ m/<?Assigned>/, q{Match unanchored (Any non-Cn character (i.e. synonym for \P{Cn}))} );

# Unassigned      # Synonym for \p{Cn}


ok("\x[27EC]" ~~ m/^<?Unassigned>$/, q{Match (Synonym for \p{Cn})} );
ok(not "\x[27EC]" ~~ m/^<!Unassigned>.$/, q{Don't match negated (Synonym for \p{Cn})} );
ok(not "\x[27EC]" ~~ m/^<-Unassigned>$/, q{Don't match inverted (Synonym for \p{Cn})} );
ok(not "\c[RIGHT OUTER JOIN]"  ~~ m/^<?Unassigned>$/, q{Don't match unrelated (Synonym for \p{Cn})} );
ok("\c[RIGHT OUTER JOIN]"  ~~ m/^<!Unassigned>.$/, q{Match unrelated negated (Synonym for \p{Cn})} );
ok("\c[RIGHT OUTER JOIN]"  ~~ m/^<-Unassigned>$/, q{Match unrelated inverted (Synonym for \p{Cn})} );
ok("\c[RIGHT OUTER JOIN]\x[27EC]" ~~ m/<?Unassigned>/, q{Match unanchored (Synonym for \p{Cn})} );

# Common          # Codepoint not explicitly assigned to a script


ok("\x[0C7E]" ~~ m/^<?Common>$/, q{Match (Codepoint not explicitly assigned to a script)} );
ok(not "\x[0C7E]" ~~ m/^<!Common>.$/, q{Don't match negated (Codepoint not explicitly assigned to a script)} );
ok(not "\x[0C7E]" ~~ m/^<-Common>$/, q{Don't match inverted (Codepoint not explicitly assigned to a script)} );
ok(not "\c[KANNADA SIGN ANUSVARA]"  ~~ m/^<?Common>$/, q{Don't match unrelated (Codepoint not explicitly assigned to a script)} );
ok("\c[KANNADA SIGN ANUSVARA]"  ~~ m/^<!Common>.$/, q{Match unrelated negated (Codepoint not explicitly assigned to a script)} );
ok("\c[KANNADA SIGN ANUSVARA]"  ~~ m/^<-Common>$/, q{Match unrelated inverted (Codepoint not explicitly assigned to a script)} );
ok(not "\c[KHMER VOWEL INHERENT AQ]" ~~ m/^<?Common>$/, q{Don't match related (Codepoint not explicitly assigned to a script)} );
ok("\c[KHMER VOWEL INHERENT AQ]" ~~ m/^<!Common>.$/, q{Match related negated (Codepoint not explicitly assigned to a script)} );
ok("\c[KHMER VOWEL INHERENT AQ]" ~~ m/^<-Common>$/, q{Match related inverted (Codepoint not explicitly assigned to a script)} );
ok("\c[KANNADA SIGN ANUSVARA]\c[KHMER VOWEL INHERENT AQ]\x[0C7E]" ~~ m/<?Common>/, q{Match unanchored (Codepoint not explicitly assigned to a script)} );

# InAlphabeticPresentationForms


ok(not "\x[531A]"  ~~ m/^<?InAlphabeticPresentationForms>$/, q{Don't match unrelated <?InAlphabeticPresentationForms>} );
ok("\x[531A]"  ~~ m/^<!InAlphabeticPresentationForms>.$/, q{Match unrelated negated <?InAlphabeticPresentationForms>} );
ok("\x[531A]"  ~~ m/^<-InAlphabeticPresentationForms>$/, q{Match unrelated inverted <?InAlphabeticPresentationForms>} );

# InArabic


ok("\c[ARABIC NUMBER SIGN]" ~~ m/^<?InArabic>$/, q{Match <?InArabic>} );
ok(not "\c[ARABIC NUMBER SIGN]" ~~ m/^<!InArabic>.$/, q{Don't match negated <?InArabic>} );
ok(not "\c[ARABIC NUMBER SIGN]" ~~ m/^<-InArabic>$/, q{Don't match inverted <?InArabic>} );
ok(not "\x[7315]"  ~~ m/^<?InArabic>$/, q{Don't match unrelated <?InArabic>} );
ok("\x[7315]"  ~~ m/^<!InArabic>.$/, q{Match unrelated negated <?InArabic>} );
ok("\x[7315]"  ~~ m/^<-InArabic>$/, q{Match unrelated inverted <?InArabic>} );
ok("\x[7315]\c[ARABIC NUMBER SIGN]" ~~ m/<?InArabic>/, q{Match unanchored <?InArabic>} );

# InArabicPresentationFormsA


ok(not "\x[8340]"  ~~ m/^<?InArabicPresentationFormsA>$/, q{Don't match unrelated <?InArabicPresentationFormsA>} );
ok("\x[8340]"  ~~ m/^<!InArabicPresentationFormsA>.$/, q{Match unrelated negated <?InArabicPresentationFormsA>} );
ok("\x[8340]"  ~~ m/^<-InArabicPresentationFormsA>$/, q{Match unrelated inverted <?InArabicPresentationFormsA>} );

# InArabicPresentationFormsB


ok(not "\x[BEEC]"  ~~ m/^<?InArabicPresentationFormsB>$/, q{Don't match unrelated <?InArabicPresentationFormsB>} );
ok("\x[BEEC]"  ~~ m/^<!InArabicPresentationFormsB>.$/, q{Match unrelated negated <?InArabicPresentationFormsB>} );
ok("\x[BEEC]"  ~~ m/^<-InArabicPresentationFormsB>$/, q{Match unrelated inverted <?InArabicPresentationFormsB>} );

# InArmenian


ok("\x[0530]" ~~ m/^<?InArmenian>$/, q{Match <?InArmenian>} );
ok(not "\x[0530]" ~~ m/^<!InArmenian>.$/, q{Don't match negated <?InArmenian>} );
ok(not "\x[0530]" ~~ m/^<-InArmenian>$/, q{Don't match inverted <?InArmenian>} );
ok(not "\x[3B0D]"  ~~ m/^<?InArmenian>$/, q{Don't match unrelated <?InArmenian>} );
ok("\x[3B0D]"  ~~ m/^<!InArmenian>.$/, q{Match unrelated negated <?InArmenian>} );
ok("\x[3B0D]"  ~~ m/^<-InArmenian>$/, q{Match unrelated inverted <?InArmenian>} );
ok("\x[3B0D]\x[0530]" ~~ m/<?InArmenian>/, q{Match unanchored <?InArmenian>} );

# InArrows


ok("\c[LEFTWARDS ARROW]" ~~ m/^<?InArrows>$/, q{Match <?InArrows>} );
ok(not "\c[LEFTWARDS ARROW]" ~~ m/^<!InArrows>.$/, q{Don't match negated <?InArrows>} );
ok(not "\c[LEFTWARDS ARROW]" ~~ m/^<-InArrows>$/, q{Don't match inverted <?InArrows>} );
ok(not "\x[C401]"  ~~ m/^<?InArrows>$/, q{Don't match unrelated <?InArrows>} );
ok("\x[C401]"  ~~ m/^<!InArrows>.$/, q{Match unrelated negated <?InArrows>} );
ok("\x[C401]"  ~~ m/^<-InArrows>$/, q{Match unrelated inverted <?InArrows>} );
ok("\x[C401]\c[LEFTWARDS ARROW]" ~~ m/<?InArrows>/, q{Match unanchored <?InArrows>} );

# InBasicLatin


ok("\c[NULL]" ~~ m/^<?InBasicLatin>$/, q{Match <?InBasicLatin>} );
ok(not "\c[NULL]" ~~ m/^<!InBasicLatin>.$/, q{Don't match negated <?InBasicLatin>} );
ok(not "\c[NULL]" ~~ m/^<-InBasicLatin>$/, q{Don't match inverted <?InBasicLatin>} );
ok(not "\x[46EA]"  ~~ m/^<?InBasicLatin>$/, q{Don't match unrelated <?InBasicLatin>} );
ok("\x[46EA]"  ~~ m/^<!InBasicLatin>.$/, q{Match unrelated negated <?InBasicLatin>} );
ok("\x[46EA]"  ~~ m/^<-InBasicLatin>$/, q{Match unrelated inverted <?InBasicLatin>} );
ok("\x[46EA]\c[NULL]" ~~ m/<?InBasicLatin>/, q{Match unanchored <?InBasicLatin>} );

# InBengali


ok("\x[0980]" ~~ m/^<?InBengali>$/, q{Match <?InBengali>} );
ok(not "\x[0980]" ~~ m/^<!InBengali>.$/, q{Don't match negated <?InBengali>} );
ok(not "\x[0980]" ~~ m/^<-InBengali>$/, q{Don't match inverted <?InBengali>} );
ok(not "\c[YI SYLLABLE HMY]"  ~~ m/^<?InBengali>$/, q{Don't match unrelated <?InBengali>} );
ok("\c[YI SYLLABLE HMY]"  ~~ m/^<!InBengali>.$/, q{Match unrelated negated <?InBengali>} );
ok("\c[YI SYLLABLE HMY]"  ~~ m/^<-InBengali>$/, q{Match unrelated inverted <?InBengali>} );
ok("\c[YI SYLLABLE HMY]\x[0980]" ~~ m/<?InBengali>/, q{Match unanchored <?InBengali>} );

# InBlockElements


ok("\c[UPPER HALF BLOCK]" ~~ m/^<?InBlockElements>$/, q{Match <?InBlockElements>} );
ok(not "\c[UPPER HALF BLOCK]" ~~ m/^<!InBlockElements>.$/, q{Don't match negated <?InBlockElements>} );
ok(not "\c[UPPER HALF BLOCK]" ~~ m/^<-InBlockElements>$/, q{Don't match inverted <?InBlockElements>} );
ok(not "\x[5F41]"  ~~ m/^<?InBlockElements>$/, q{Don't match unrelated <?InBlockElements>} );
ok("\x[5F41]"  ~~ m/^<!InBlockElements>.$/, q{Match unrelated negated <?InBlockElements>} );
ok("\x[5F41]"  ~~ m/^<-InBlockElements>$/, q{Match unrelated inverted <?InBlockElements>} );
ok("\x[5F41]\c[UPPER HALF BLOCK]" ~~ m/<?InBlockElements>/, q{Match unanchored <?InBlockElements>} );

# InBopomofo


ok("\x[3100]" ~~ m/^<?InBopomofo>$/, q{Match <?InBopomofo>} );
ok(not "\x[3100]" ~~ m/^<!InBopomofo>.$/, q{Don't match negated <?InBopomofo>} );
ok(not "\x[3100]" ~~ m/^<-InBopomofo>$/, q{Don't match inverted <?InBopomofo>} );
ok(not "\x[9F8E]"  ~~ m/^<?InBopomofo>$/, q{Don't match unrelated <?InBopomofo>} );
ok("\x[9F8E]"  ~~ m/^<!InBopomofo>.$/, q{Match unrelated negated <?InBopomofo>} );
ok("\x[9F8E]"  ~~ m/^<-InBopomofo>$/, q{Match unrelated inverted <?InBopomofo>} );
ok("\x[9F8E]\x[3100]" ~~ m/<?InBopomofo>/, q{Match unanchored <?InBopomofo>} );

# InBopomofoExtended


ok("\c[BOPOMOFO LETTER BU]" ~~ m/^<?InBopomofoExtended>$/, q{Match <?InBopomofoExtended>} );
ok(not "\c[BOPOMOFO LETTER BU]" ~~ m/^<!InBopomofoExtended>.$/, q{Don't match negated <?InBopomofoExtended>} );
ok(not "\c[BOPOMOFO LETTER BU]" ~~ m/^<-InBopomofoExtended>$/, q{Don't match inverted <?InBopomofoExtended>} );
ok(not "\x[43A6]"  ~~ m/^<?InBopomofoExtended>$/, q{Don't match unrelated <?InBopomofoExtended>} );
ok("\x[43A6]"  ~~ m/^<!InBopomofoExtended>.$/, q{Match unrelated negated <?InBopomofoExtended>} );
ok("\x[43A6]"  ~~ m/^<-InBopomofoExtended>$/, q{Match unrelated inverted <?InBopomofoExtended>} );
ok("\x[43A6]\c[BOPOMOFO LETTER BU]" ~~ m/<?InBopomofoExtended>/, q{Match unanchored <?InBopomofoExtended>} );

# InBoxDrawing


ok("\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<?InBoxDrawing>$/, q{Match <?InBoxDrawing>} );
ok(not "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<!InBoxDrawing>.$/, q{Don't match negated <?InBoxDrawing>} );
ok(not "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<-InBoxDrawing>$/, q{Don't match inverted <?InBoxDrawing>} );
ok(not "\x[7865]"  ~~ m/^<?InBoxDrawing>$/, q{Don't match unrelated <?InBoxDrawing>} );
ok("\x[7865]"  ~~ m/^<!InBoxDrawing>.$/, q{Match unrelated negated <?InBoxDrawing>} );
ok("\x[7865]"  ~~ m/^<-InBoxDrawing>$/, q{Match unrelated inverted <?InBoxDrawing>} );
ok("\x[7865]\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/<?InBoxDrawing>/, q{Match unanchored <?InBoxDrawing>} );

# InBraillePatterns


ok("\c[BRAILLE PATTERN BLANK]" ~~ m/^<?InBraillePatterns>$/, q{Match <?InBraillePatterns>} );
ok(not "\c[BRAILLE PATTERN BLANK]" ~~ m/^<!InBraillePatterns>.$/, q{Don't match negated <?InBraillePatterns>} );
ok(not "\c[BRAILLE PATTERN BLANK]" ~~ m/^<-InBraillePatterns>$/, q{Don't match inverted <?InBraillePatterns>} );
ok(not "\c[THAI CHARACTER KHO KHAI]"  ~~ m/^<?InBraillePatterns>$/, q{Don't match unrelated <?InBraillePatterns>} );
ok("\c[THAI CHARACTER KHO KHAI]"  ~~ m/^<!InBraillePatterns>.$/, q{Match unrelated negated <?InBraillePatterns>} );
ok("\c[THAI CHARACTER KHO KHAI]"  ~~ m/^<-InBraillePatterns>$/, q{Match unrelated inverted <?InBraillePatterns>} );
ok("\c[THAI CHARACTER KHO KHAI]\c[BRAILLE PATTERN BLANK]" ~~ m/<?InBraillePatterns>/, q{Match unanchored <?InBraillePatterns>} );

# InBuhid


ok("\c[BUHID LETTER A]" ~~ m/^<?InBuhid>$/, q{Match <?InBuhid>} );
ok(not "\c[BUHID LETTER A]" ~~ m/^<!InBuhid>.$/, q{Don't match negated <?InBuhid>} );
ok(not "\c[BUHID LETTER A]" ~~ m/^<-InBuhid>$/, q{Don't match inverted <?InBuhid>} );
ok(not "\x[D208]"  ~~ m/^<?InBuhid>$/, q{Don't match unrelated <?InBuhid>} );
ok("\x[D208]"  ~~ m/^<!InBuhid>.$/, q{Match unrelated negated <?InBuhid>} );
ok("\x[D208]"  ~~ m/^<-InBuhid>$/, q{Match unrelated inverted <?InBuhid>} );
ok("\x[D208]\c[BUHID LETTER A]" ~~ m/<?InBuhid>/, q{Match unanchored <?InBuhid>} );

# InByzantineMusicalSymbols


ok(not "\x[9B1D]"  ~~ m/^<?InByzantineMusicalSymbols>$/, q{Don't match unrelated <?InByzantineMusicalSymbols>} );
ok("\x[9B1D]"  ~~ m/^<!InByzantineMusicalSymbols>.$/, q{Match unrelated negated <?InByzantineMusicalSymbols>} );
ok("\x[9B1D]"  ~~ m/^<-InByzantineMusicalSymbols>$/, q{Match unrelated inverted <?InByzantineMusicalSymbols>} );

# InCJKCompatibility


ok("\c[SQUARE APAATO]" ~~ m/^<?InCJKCompatibility>$/, q{Match <?InCJKCompatibility>} );
ok(not "\c[SQUARE APAATO]" ~~ m/^<!InCJKCompatibility>.$/, q{Don't match negated <?InCJKCompatibility>} );
ok(not "\c[SQUARE APAATO]" ~~ m/^<-InCJKCompatibility>$/, q{Don't match inverted <?InCJKCompatibility>} );
ok(not "\x[B8A5]"  ~~ m/^<?InCJKCompatibility>$/, q{Don't match unrelated <?InCJKCompatibility>} );
ok("\x[B8A5]"  ~~ m/^<!InCJKCompatibility>.$/, q{Match unrelated negated <?InCJKCompatibility>} );
ok("\x[B8A5]"  ~~ m/^<-InCJKCompatibility>$/, q{Match unrelated inverted <?InCJKCompatibility>} );
ok("\x[B8A5]\c[SQUARE APAATO]" ~~ m/<?InCJKCompatibility>/, q{Match unanchored <?InCJKCompatibility>} );

# InCJKCompatibilityForms


ok(not "\x[3528]"  ~~ m/^<?InCJKCompatibilityForms>$/, q{Don't match unrelated <?InCJKCompatibilityForms>} );
ok("\x[3528]"  ~~ m/^<!InCJKCompatibilityForms>.$/, q{Match unrelated negated <?InCJKCompatibilityForms>} );
ok("\x[3528]"  ~~ m/^<-InCJKCompatibilityForms>$/, q{Match unrelated inverted <?InCJKCompatibilityForms>} );

# InCJKCompatibilityIdeographs


ok(not "\x[69F7]"  ~~ m/^<?InCJKCompatibilityIdeographs>$/, q{Don't match unrelated <?InCJKCompatibilityIdeographs>} );
ok("\x[69F7]"  ~~ m/^<!InCJKCompatibilityIdeographs>.$/, q{Match unrelated negated <?InCJKCompatibilityIdeographs>} );
ok("\x[69F7]"  ~~ m/^<-InCJKCompatibilityIdeographs>$/, q{Match unrelated inverted <?InCJKCompatibilityIdeographs>} );

# InCJKCompatibilityIdeographsSupplement


ok(not "\c[CANADIAN SYLLABICS NUNAVIK HO]"  ~~ m/^<?InCJKCompatibilityIdeographsSupplement>$/, q{Don't match unrelated <?InCJKCompatibilityIdeographsSupplement>} );
ok("\c[CANADIAN SYLLABICS NUNAVIK HO]"  ~~ m/^<!InCJKCompatibilityIdeographsSupplement>.$/, q{Match unrelated negated <?InCJKCompatibilityIdeographsSupplement>} );
ok("\c[CANADIAN SYLLABICS NUNAVIK HO]"  ~~ m/^<-InCJKCompatibilityIdeographsSupplement>$/, q{Match unrelated inverted <?InCJKCompatibilityIdeographsSupplement>} );

# InCJKRadicalsSupplement


ok("\c[CJK RADICAL REPEAT]" ~~ m/^<?InCJKRadicalsSupplement>$/, q{Match <?InCJKRadicalsSupplement>} );
ok(not "\c[CJK RADICAL REPEAT]" ~~ m/^<!InCJKRadicalsSupplement>.$/, q{Don't match negated <?InCJKRadicalsSupplement>} );
ok(not "\c[CJK RADICAL REPEAT]" ~~ m/^<-InCJKRadicalsSupplement>$/, q{Don't match inverted <?InCJKRadicalsSupplement>} );
ok(not "\x[37B4]"  ~~ m/^<?InCJKRadicalsSupplement>$/, q{Don't match unrelated <?InCJKRadicalsSupplement>} );
ok("\x[37B4]"  ~~ m/^<!InCJKRadicalsSupplement>.$/, q{Match unrelated negated <?InCJKRadicalsSupplement>} );
ok("\x[37B4]"  ~~ m/^<-InCJKRadicalsSupplement>$/, q{Match unrelated inverted <?InCJKRadicalsSupplement>} );
ok("\x[37B4]\c[CJK RADICAL REPEAT]" ~~ m/<?InCJKRadicalsSupplement>/, q{Match unanchored <?InCJKRadicalsSupplement>} );

# InCJKSymbolsAndPunctuation


ok("\c[IDEOGRAPHIC SPACE]" ~~ m/^<?InCJKSymbolsAndPunctuation>$/, q{Match <?InCJKSymbolsAndPunctuation>} );
ok(not "\c[IDEOGRAPHIC SPACE]" ~~ m/^<!InCJKSymbolsAndPunctuation>.$/, q{Don't match negated <?InCJKSymbolsAndPunctuation>} );
ok(not "\c[IDEOGRAPHIC SPACE]" ~~ m/^<-InCJKSymbolsAndPunctuation>$/, q{Don't match inverted <?InCJKSymbolsAndPunctuation>} );
ok(not "\x[80AA]"  ~~ m/^<?InCJKSymbolsAndPunctuation>$/, q{Don't match unrelated <?InCJKSymbolsAndPunctuation>} );
ok("\x[80AA]"  ~~ m/^<!InCJKSymbolsAndPunctuation>.$/, q{Match unrelated negated <?InCJKSymbolsAndPunctuation>} );
ok("\x[80AA]"  ~~ m/^<-InCJKSymbolsAndPunctuation>$/, q{Match unrelated inverted <?InCJKSymbolsAndPunctuation>} );
ok("\x[80AA]\c[IDEOGRAPHIC SPACE]" ~~ m/<?InCJKSymbolsAndPunctuation>/, q{Match unanchored <?InCJKSymbolsAndPunctuation>} );

# InCJKUnifiedIdeographs


ok("\x[4E00]" ~~ m/^<?InCJKUnifiedIdeographs>$/, q{Match <?InCJKUnifiedIdeographs>} );
ok(not "\x[4E00]" ~~ m/^<!InCJKUnifiedIdeographs>.$/, q{Don't match negated <?InCJKUnifiedIdeographs>} );
ok(not "\x[4E00]" ~~ m/^<-InCJKUnifiedIdeographs>$/, q{Don't match inverted <?InCJKUnifiedIdeographs>} );
ok(not "\x[3613]"  ~~ m/^<?InCJKUnifiedIdeographs>$/, q{Don't match unrelated <?InCJKUnifiedIdeographs>} );
ok("\x[3613]"  ~~ m/^<!InCJKUnifiedIdeographs>.$/, q{Match unrelated negated <?InCJKUnifiedIdeographs>} );
ok("\x[3613]"  ~~ m/^<-InCJKUnifiedIdeographs>$/, q{Match unrelated inverted <?InCJKUnifiedIdeographs>} );
ok("\x[3613]\x[4E00]" ~~ m/<?InCJKUnifiedIdeographs>/, q{Match unanchored <?InCJKUnifiedIdeographs>} );

# InCJKUnifiedIdeographsExtensionA


ok("\x[3400]" ~~ m/^<?InCJKUnifiedIdeographsExtensionA>$/, q{Match <?InCJKUnifiedIdeographsExtensionA>} );
ok(not "\x[3400]" ~~ m/^<!InCJKUnifiedIdeographsExtensionA>.$/, q{Don't match negated <?InCJKUnifiedIdeographsExtensionA>} );
ok(not "\x[3400]" ~~ m/^<-InCJKUnifiedIdeographsExtensionA>$/, q{Don't match inverted <?InCJKUnifiedIdeographsExtensionA>} );
ok(not "\c[SQUARE HOORU]"  ~~ m/^<?InCJKUnifiedIdeographsExtensionA>$/, q{Don't match unrelated <?InCJKUnifiedIdeographsExtensionA>} );
ok("\c[SQUARE HOORU]"  ~~ m/^<!InCJKUnifiedIdeographsExtensionA>.$/, q{Match unrelated negated <?InCJKUnifiedIdeographsExtensionA>} );
ok("\c[SQUARE HOORU]"  ~~ m/^<-InCJKUnifiedIdeographsExtensionA>$/, q{Match unrelated inverted <?InCJKUnifiedIdeographsExtensionA>} );
ok("\c[SQUARE HOORU]\x[3400]" ~~ m/<?InCJKUnifiedIdeographsExtensionA>/, q{Match unanchored <?InCJKUnifiedIdeographsExtensionA>} );

# InCJKUnifiedIdeographsExtensionB


ok(not "\x[AC3B]"  ~~ m/^<?InCJKUnifiedIdeographsExtensionB>$/, q{Don't match unrelated <?InCJKUnifiedIdeographsExtensionB>} );
ok("\x[AC3B]"  ~~ m/^<!InCJKUnifiedIdeographsExtensionB>.$/, q{Match unrelated negated <?InCJKUnifiedIdeographsExtensionB>} );
ok("\x[AC3B]"  ~~ m/^<-InCJKUnifiedIdeographsExtensionB>$/, q{Match unrelated inverted <?InCJKUnifiedIdeographsExtensionB>} );

# InCherokee


ok("\c[CHEROKEE LETTER A]" ~~ m/^<?InCherokee>$/, q{Match <?InCherokee>} );
ok(not "\c[CHEROKEE LETTER A]" ~~ m/^<!InCherokee>.$/, q{Don't match negated <?InCherokee>} );
ok(not "\c[CHEROKEE LETTER A]" ~~ m/^<-InCherokee>$/, q{Don't match inverted <?InCherokee>} );
ok(not "\x[985F]"  ~~ m/^<?InCherokee>$/, q{Don't match unrelated <?InCherokee>} );
ok("\x[985F]"  ~~ m/^<!InCherokee>.$/, q{Match unrelated negated <?InCherokee>} );
ok("\x[985F]"  ~~ m/^<-InCherokee>$/, q{Match unrelated inverted <?InCherokee>} );
ok("\x[985F]\c[CHEROKEE LETTER A]" ~~ m/<?InCherokee>/, q{Match unanchored <?InCherokee>} );

# InCombiningDiacriticalMarks


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<?InCombiningDiacriticalMarks>$/, q{Match <?InCombiningDiacriticalMarks>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<!InCombiningDiacriticalMarks>.$/, q{Don't match negated <?InCombiningDiacriticalMarks>} );
ok(not "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-InCombiningDiacriticalMarks>$/, q{Don't match inverted <?InCombiningDiacriticalMarks>} );
ok(not "\x[76DA]"  ~~ m/^<?InCombiningDiacriticalMarks>$/, q{Don't match unrelated <?InCombiningDiacriticalMarks>} );
ok("\x[76DA]"  ~~ m/^<!InCombiningDiacriticalMarks>.$/, q{Match unrelated negated <?InCombiningDiacriticalMarks>} );
ok("\x[76DA]"  ~~ m/^<-InCombiningDiacriticalMarks>$/, q{Match unrelated inverted <?InCombiningDiacriticalMarks>} );
ok("\x[76DA]\c[COMBINING GRAVE ACCENT]" ~~ m/<?InCombiningDiacriticalMarks>/, q{Match unanchored <?InCombiningDiacriticalMarks>} );

# InCombiningDiacriticalMarksforSymbols


ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<?InCombiningDiacriticalMarksforSymbols>$/, q{Match <?InCombiningDiacriticalMarksforSymbols>} );
ok(not "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<!InCombiningDiacriticalMarksforSymbols>.$/, q{Don't match negated <?InCombiningDiacriticalMarksforSymbols>} );
ok(not "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<-InCombiningDiacriticalMarksforSymbols>$/, q{Don't match inverted <?InCombiningDiacriticalMarksforSymbols>} );
ok(not "\x[7345]"  ~~ m/^<?InCombiningDiacriticalMarksforSymbols>$/, q{Don't match unrelated <?InCombiningDiacriticalMarksforSymbols>} );
ok("\x[7345]"  ~~ m/^<!InCombiningDiacriticalMarksforSymbols>.$/, q{Match unrelated negated <?InCombiningDiacriticalMarksforSymbols>} );
ok("\x[7345]"  ~~ m/^<-InCombiningDiacriticalMarksforSymbols>$/, q{Match unrelated inverted <?InCombiningDiacriticalMarksforSymbols>} );
ok("\x[7345]\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/<?InCombiningDiacriticalMarksforSymbols>/, q{Match unanchored <?InCombiningDiacriticalMarksforSymbols>} );

# InCombiningHalfMarks


ok(not "\x[6C2E]"  ~~ m/^<?InCombiningHalfMarks>$/, q{Don't match unrelated <?InCombiningHalfMarks>} );
ok("\x[6C2E]"  ~~ m/^<!InCombiningHalfMarks>.$/, q{Match unrelated negated <?InCombiningHalfMarks>} );
ok("\x[6C2E]"  ~~ m/^<-InCombiningHalfMarks>$/, q{Match unrelated inverted <?InCombiningHalfMarks>} );

# InControlPictures


ok("\c[SYMBOL FOR NULL]" ~~ m/^<?InControlPictures>$/, q{Match <?InControlPictures>} );
ok(not "\c[SYMBOL FOR NULL]" ~~ m/^<!InControlPictures>.$/, q{Don't match negated <?InControlPictures>} );
ok(not "\c[SYMBOL FOR NULL]" ~~ m/^<-InControlPictures>$/, q{Don't match inverted <?InControlPictures>} );
ok(not "\x[BCE2]"  ~~ m/^<?InControlPictures>$/, q{Don't match unrelated <?InControlPictures>} );
ok("\x[BCE2]"  ~~ m/^<!InControlPictures>.$/, q{Match unrelated negated <?InControlPictures>} );
ok("\x[BCE2]"  ~~ m/^<-InControlPictures>$/, q{Match unrelated inverted <?InControlPictures>} );
ok("\x[BCE2]\c[SYMBOL FOR NULL]" ~~ m/<?InControlPictures>/, q{Match unanchored <?InControlPictures>} );

# InCurrencySymbols


ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<?InCurrencySymbols>$/, q{Match <?InCurrencySymbols>} );
ok(not "\c[EURO-CURRENCY SIGN]" ~~ m/^<!InCurrencySymbols>.$/, q{Don't match negated <?InCurrencySymbols>} );
ok(not "\c[EURO-CURRENCY SIGN]" ~~ m/^<-InCurrencySymbols>$/, q{Don't match inverted <?InCurrencySymbols>} );
ok(not "\x[8596]"  ~~ m/^<?InCurrencySymbols>$/, q{Don't match unrelated <?InCurrencySymbols>} );
ok("\x[8596]"  ~~ m/^<!InCurrencySymbols>.$/, q{Match unrelated negated <?InCurrencySymbols>} );
ok("\x[8596]"  ~~ m/^<-InCurrencySymbols>$/, q{Match unrelated inverted <?InCurrencySymbols>} );
ok("\x[8596]\c[EURO-CURRENCY SIGN]" ~~ m/<?InCurrencySymbols>/, q{Match unanchored <?InCurrencySymbols>} );

# InCyrillic


ok("\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<?InCyrillic>$/, q{Match <?InCyrillic>} );
ok(not "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<!InCyrillic>.$/, q{Don't match negated <?InCyrillic>} );
ok(not "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<-InCyrillic>$/, q{Don't match inverted <?InCyrillic>} );
ok(not "\x[51B2]"  ~~ m/^<?InCyrillic>$/, q{Don't match unrelated <?InCyrillic>} );
ok("\x[51B2]"  ~~ m/^<!InCyrillic>.$/, q{Match unrelated negated <?InCyrillic>} );
ok("\x[51B2]"  ~~ m/^<-InCyrillic>$/, q{Match unrelated inverted <?InCyrillic>} );
ok("\x[51B2]\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/<?InCyrillic>/, q{Match unanchored <?InCyrillic>} );

# InCyrillicSupplementary


ok("\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<?InCyrillicSupplementary>$/, q{Match <?InCyrillicSupplementary>} );
ok(not "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<!InCyrillicSupplementary>.$/, q{Don't match negated <?InCyrillicSupplementary>} );
ok(not "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<-InCyrillicSupplementary>$/, q{Don't match inverted <?InCyrillicSupplementary>} );
ok(not "\x[7BD9]"  ~~ m/^<?InCyrillicSupplementary>$/, q{Don't match unrelated <?InCyrillicSupplementary>} );
ok("\x[7BD9]"  ~~ m/^<!InCyrillicSupplementary>.$/, q{Match unrelated negated <?InCyrillicSupplementary>} );
ok("\x[7BD9]"  ~~ m/^<-InCyrillicSupplementary>$/, q{Match unrelated inverted <?InCyrillicSupplementary>} );
ok("\x[7BD9]\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/<?InCyrillicSupplementary>/, q{Match unanchored <?InCyrillicSupplementary>} );

# InDeseret


ok(not "\c[TAMIL DIGIT FOUR]"  ~~ m/^<?InDeseret>$/, q{Don't match unrelated <?InDeseret>} );
ok("\c[TAMIL DIGIT FOUR]"  ~~ m/^<!InDeseret>.$/, q{Match unrelated negated <?InDeseret>} );
ok("\c[TAMIL DIGIT FOUR]"  ~~ m/^<-InDeseret>$/, q{Match unrelated inverted <?InDeseret>} );

# InDevanagari


ok("\x[0900]" ~~ m/^<?InDevanagari>$/, q{Match <?InDevanagari>} );
ok(not "\x[0900]" ~~ m/^<!InDevanagari>.$/, q{Don't match negated <?InDevanagari>} );
ok(not "\x[0900]" ~~ m/^<-InDevanagari>$/, q{Don't match inverted <?InDevanagari>} );
ok(not "\x[BB12]"  ~~ m/^<?InDevanagari>$/, q{Don't match unrelated <?InDevanagari>} );
ok("\x[BB12]"  ~~ m/^<!InDevanagari>.$/, q{Match unrelated negated <?InDevanagari>} );
ok("\x[BB12]"  ~~ m/^<-InDevanagari>$/, q{Match unrelated inverted <?InDevanagari>} );
ok("\x[BB12]\x[0900]" ~~ m/<?InDevanagari>/, q{Match unanchored <?InDevanagari>} );

# InDingbats


ok("\x[2700]" ~~ m/^<?InDingbats>$/, q{Match <?InDingbats>} );
ok(not "\x[2700]" ~~ m/^<!InDingbats>.$/, q{Don't match negated <?InDingbats>} );
ok(not "\x[2700]" ~~ m/^<-InDingbats>$/, q{Don't match inverted <?InDingbats>} );
ok(not "\x[D7A8]"  ~~ m/^<?InDingbats>$/, q{Don't match unrelated <?InDingbats>} );
ok("\x[D7A8]"  ~~ m/^<!InDingbats>.$/, q{Match unrelated negated <?InDingbats>} );
ok("\x[D7A8]"  ~~ m/^<-InDingbats>$/, q{Match unrelated inverted <?InDingbats>} );
ok("\x[D7A8]\x[2700]" ~~ m/<?InDingbats>/, q{Match unanchored <?InDingbats>} );

# InEnclosedAlphanumerics


ok("\c[CIRCLED DIGIT ONE]" ~~ m/^<?InEnclosedAlphanumerics>$/, q{Match <?InEnclosedAlphanumerics>} );
ok(not "\c[CIRCLED DIGIT ONE]" ~~ m/^<!InEnclosedAlphanumerics>.$/, q{Don't match negated <?InEnclosedAlphanumerics>} );
ok(not "\c[CIRCLED DIGIT ONE]" ~~ m/^<-InEnclosedAlphanumerics>$/, q{Don't match inverted <?InEnclosedAlphanumerics>} );
ok(not "\x[C3A2]"  ~~ m/^<?InEnclosedAlphanumerics>$/, q{Don't match unrelated <?InEnclosedAlphanumerics>} );
ok("\x[C3A2]"  ~~ m/^<!InEnclosedAlphanumerics>.$/, q{Match unrelated negated <?InEnclosedAlphanumerics>} );
ok("\x[C3A2]"  ~~ m/^<-InEnclosedAlphanumerics>$/, q{Match unrelated inverted <?InEnclosedAlphanumerics>} );
ok("\x[C3A2]\c[CIRCLED DIGIT ONE]" ~~ m/<?InEnclosedAlphanumerics>/, q{Match unanchored <?InEnclosedAlphanumerics>} );

# InEnclosedCJKLettersAndMonths


ok("\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<?InEnclosedCJKLettersAndMonths>$/, q{Match <?InEnclosedCJKLettersAndMonths>} );
ok(not "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<!InEnclosedCJKLettersAndMonths>.$/, q{Don't match negated <?InEnclosedCJKLettersAndMonths>} );
ok(not "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<-InEnclosedCJKLettersAndMonths>$/, q{Don't match inverted <?InEnclosedCJKLettersAndMonths>} );
ok(not "\x[5B44]"  ~~ m/^<?InEnclosedCJKLettersAndMonths>$/, q{Don't match unrelated <?InEnclosedCJKLettersAndMonths>} );
ok("\x[5B44]"  ~~ m/^<!InEnclosedCJKLettersAndMonths>.$/, q{Match unrelated negated <?InEnclosedCJKLettersAndMonths>} );
ok("\x[5B44]"  ~~ m/^<-InEnclosedCJKLettersAndMonths>$/, q{Match unrelated inverted <?InEnclosedCJKLettersAndMonths>} );
ok("\x[5B44]\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/<?InEnclosedCJKLettersAndMonths>/, q{Match unanchored <?InEnclosedCJKLettersAndMonths>} );

# InEthiopic


ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<?InEthiopic>$/, q{Match <?InEthiopic>} );
ok(not "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<!InEthiopic>.$/, q{Don't match negated <?InEthiopic>} );
ok(not "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<-InEthiopic>$/, q{Don't match inverted <?InEthiopic>} );
ok(not "\x[BBAE]"  ~~ m/^<?InEthiopic>$/, q{Don't match unrelated <?InEthiopic>} );
ok("\x[BBAE]"  ~~ m/^<!InEthiopic>.$/, q{Match unrelated negated <?InEthiopic>} );
ok("\x[BBAE]"  ~~ m/^<-InEthiopic>$/, q{Match unrelated inverted <?InEthiopic>} );
ok("\x[BBAE]\c[ETHIOPIC SYLLABLE HA]" ~~ m/<?InEthiopic>/, q{Match unanchored <?InEthiopic>} );

# InGeneralPunctuation


ok("\c[EN QUAD]" ~~ m/^<?InGeneralPunctuation>$/, q{Match <?InGeneralPunctuation>} );
ok(not "\c[EN QUAD]" ~~ m/^<!InGeneralPunctuation>.$/, q{Don't match negated <?InGeneralPunctuation>} );
ok(not "\c[EN QUAD]" ~~ m/^<-InGeneralPunctuation>$/, q{Don't match inverted <?InGeneralPunctuation>} );
ok(not "\c[MEDIUM RIGHT PARENTHESIS ORNAMENT]"  ~~ m/^<?InGeneralPunctuation>$/, q{Don't match unrelated <?InGeneralPunctuation>} );
ok("\c[MEDIUM RIGHT PARENTHESIS ORNAMENT]"  ~~ m/^<!InGeneralPunctuation>.$/, q{Match unrelated negated <?InGeneralPunctuation>} );
ok("\c[MEDIUM RIGHT PARENTHESIS ORNAMENT]"  ~~ m/^<-InGeneralPunctuation>$/, q{Match unrelated inverted <?InGeneralPunctuation>} );
ok("\c[MEDIUM RIGHT PARENTHESIS ORNAMENT]\c[EN QUAD]" ~~ m/<?InGeneralPunctuation>/, q{Match unanchored <?InGeneralPunctuation>} );

# InGeometricShapes


ok("\c[BLACK SQUARE]" ~~ m/^<?InGeometricShapes>$/, q{Match <?InGeometricShapes>} );
ok(not "\c[BLACK SQUARE]" ~~ m/^<!InGeometricShapes>.$/, q{Don't match negated <?InGeometricShapes>} );
ok(not "\c[BLACK SQUARE]" ~~ m/^<-InGeometricShapes>$/, q{Don't match inverted <?InGeometricShapes>} );
ok(not "\x[B700]"  ~~ m/^<?InGeometricShapes>$/, q{Don't match unrelated <?InGeometricShapes>} );
ok("\x[B700]"  ~~ m/^<!InGeometricShapes>.$/, q{Match unrelated negated <?InGeometricShapes>} );
ok("\x[B700]"  ~~ m/^<-InGeometricShapes>$/, q{Match unrelated inverted <?InGeometricShapes>} );
ok("\x[B700]\c[BLACK SQUARE]" ~~ m/<?InGeometricShapes>/, q{Match unanchored <?InGeometricShapes>} );

# InGeorgian


ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<?InGeorgian>$/, q{Match <?InGeorgian>} );
ok(not "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<!InGeorgian>.$/, q{Don't match negated <?InGeorgian>} );
ok(not "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<-InGeorgian>$/, q{Don't match inverted <?InGeorgian>} );
ok(not "\c[IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ONE]"  ~~ m/^<?InGeorgian>$/, q{Don't match unrelated <?InGeorgian>} );
ok("\c[IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ONE]"  ~~ m/^<!InGeorgian>.$/, q{Match unrelated negated <?InGeorgian>} );
ok("\c[IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ONE]"  ~~ m/^<-InGeorgian>$/, q{Match unrelated inverted <?InGeorgian>} );
ok("\c[IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ONE]\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/<?InGeorgian>/, q{Match unanchored <?InGeorgian>} );

# InGothic


ok(not "\x[4825]"  ~~ m/^<?InGothic>$/, q{Don't match unrelated <?InGothic>} );
ok("\x[4825]"  ~~ m/^<!InGothic>.$/, q{Match unrelated negated <?InGothic>} );
ok("\x[4825]"  ~~ m/^<-InGothic>$/, q{Match unrelated inverted <?InGothic>} );

# InGreekExtended


ok("\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<?InGreekExtended>$/, q{Match <?InGreekExtended>} );
ok(not "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<!InGreekExtended>.$/, q{Don't match negated <?InGreekExtended>} );
ok(not "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<-InGreekExtended>$/, q{Don't match inverted <?InGreekExtended>} );
ok(not "\x[B9B7]"  ~~ m/^<?InGreekExtended>$/, q{Don't match unrelated <?InGreekExtended>} );
ok("\x[B9B7]"  ~~ m/^<!InGreekExtended>.$/, q{Match unrelated negated <?InGreekExtended>} );
ok("\x[B9B7]"  ~~ m/^<-InGreekExtended>$/, q{Match unrelated inverted <?InGreekExtended>} );
ok("\x[B9B7]\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/<?InGreekExtended>/, q{Match unanchored <?InGreekExtended>} );

# InGreekAndCoptic


ok("\x[0370]" ~~ m/^<?InGreekAndCoptic>$/, q{Match <?InGreekAndCoptic>} );
ok(not "\x[0370]" ~~ m/^<!InGreekAndCoptic>.$/, q{Don't match negated <?InGreekAndCoptic>} );
ok(not "\x[0370]" ~~ m/^<-InGreekAndCoptic>$/, q{Don't match inverted <?InGreekAndCoptic>} );
ok(not "\x[7197]"  ~~ m/^<?InGreekAndCoptic>$/, q{Don't match unrelated <?InGreekAndCoptic>} );
ok("\x[7197]"  ~~ m/^<!InGreekAndCoptic>.$/, q{Match unrelated negated <?InGreekAndCoptic>} );
ok("\x[7197]"  ~~ m/^<-InGreekAndCoptic>$/, q{Match unrelated inverted <?InGreekAndCoptic>} );
ok("\x[7197]\x[0370]" ~~ m/<?InGreekAndCoptic>/, q{Match unanchored <?InGreekAndCoptic>} );

# InGujarati


ok("\x[0A80]" ~~ m/^<?InGujarati>$/, q{Match <?InGujarati>} );
ok(not "\x[0A80]" ~~ m/^<!InGujarati>.$/, q{Don't match negated <?InGujarati>} );
ok(not "\x[0A80]" ~~ m/^<-InGujarati>$/, q{Don't match inverted <?InGujarati>} );
ok(not "\x[3B63]"  ~~ m/^<?InGujarati>$/, q{Don't match unrelated <?InGujarati>} );
ok("\x[3B63]"  ~~ m/^<!InGujarati>.$/, q{Match unrelated negated <?InGujarati>} );
ok("\x[3B63]"  ~~ m/^<-InGujarati>$/, q{Match unrelated inverted <?InGujarati>} );
ok("\x[3B63]\x[0A80]" ~~ m/<?InGujarati>/, q{Match unanchored <?InGujarati>} );

# InGurmukhi


ok("\x[0A00]" ~~ m/^<?InGurmukhi>$/, q{Match <?InGurmukhi>} );
ok(not "\x[0A00]" ~~ m/^<!InGurmukhi>.$/, q{Don't match negated <?InGurmukhi>} );
ok(not "\x[0A00]" ~~ m/^<-InGurmukhi>$/, q{Don't match inverted <?InGurmukhi>} );
ok(not "\x[10C8]"  ~~ m/^<?InGurmukhi>$/, q{Don't match unrelated <?InGurmukhi>} );
ok("\x[10C8]"  ~~ m/^<!InGurmukhi>.$/, q{Match unrelated negated <?InGurmukhi>} );
ok("\x[10C8]"  ~~ m/^<-InGurmukhi>$/, q{Match unrelated inverted <?InGurmukhi>} );
ok("\x[10C8]\x[0A00]" ~~ m/<?InGurmukhi>/, q{Match unanchored <?InGurmukhi>} );

# InHalfwidthAndFullwidthForms


ok(not "\x[CA55]"  ~~ m/^<?InHalfwidthAndFullwidthForms>$/, q{Don't match unrelated <?InHalfwidthAndFullwidthForms>} );
ok("\x[CA55]"  ~~ m/^<!InHalfwidthAndFullwidthForms>.$/, q{Match unrelated negated <?InHalfwidthAndFullwidthForms>} );
ok("\x[CA55]"  ~~ m/^<-InHalfwidthAndFullwidthForms>$/, q{Match unrelated inverted <?InHalfwidthAndFullwidthForms>} );

# InHangulCompatibilityJamo


ok("\x[3130]" ~~ m/^<?InHangulCompatibilityJamo>$/, q{Match <?InHangulCompatibilityJamo>} );
ok(not "\x[3130]" ~~ m/^<!InHangulCompatibilityJamo>.$/, q{Don't match negated <?InHangulCompatibilityJamo>} );
ok(not "\x[3130]" ~~ m/^<-InHangulCompatibilityJamo>$/, q{Don't match inverted <?InHangulCompatibilityJamo>} );
ok(not "\c[MEASURED BY]"  ~~ m/^<?InHangulCompatibilityJamo>$/, q{Don't match unrelated <?InHangulCompatibilityJamo>} );
ok("\c[MEASURED BY]"  ~~ m/^<!InHangulCompatibilityJamo>.$/, q{Match unrelated negated <?InHangulCompatibilityJamo>} );
ok("\c[MEASURED BY]"  ~~ m/^<-InHangulCompatibilityJamo>$/, q{Match unrelated inverted <?InHangulCompatibilityJamo>} );
ok("\c[MEASURED BY]\x[3130]" ~~ m/<?InHangulCompatibilityJamo>/, q{Match unanchored <?InHangulCompatibilityJamo>} );

# InHangulJamo


ok("\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<?InHangulJamo>$/, q{Match <?InHangulJamo>} );
ok(not "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<!InHangulJamo>.$/, q{Don't match negated <?InHangulJamo>} );
ok(not "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<-InHangulJamo>$/, q{Don't match inverted <?InHangulJamo>} );
ok(not "\x[3B72]"  ~~ m/^<?InHangulJamo>$/, q{Don't match unrelated <?InHangulJamo>} );
ok("\x[3B72]"  ~~ m/^<!InHangulJamo>.$/, q{Match unrelated negated <?InHangulJamo>} );
ok("\x[3B72]"  ~~ m/^<-InHangulJamo>$/, q{Match unrelated inverted <?InHangulJamo>} );
ok("\x[3B72]\c[HANGUL CHOSEONG KIYEOK]" ~~ m/<?InHangulJamo>/, q{Match unanchored <?InHangulJamo>} );

# InHangulSyllables


ok("\x[CD95]" ~~ m/^<?InHangulSyllables>$/, q{Match <?InHangulSyllables>} );
ok(not "\x[CD95]" ~~ m/^<!InHangulSyllables>.$/, q{Don't match negated <?InHangulSyllables>} );
ok(not "\x[CD95]" ~~ m/^<-InHangulSyllables>$/, q{Don't match inverted <?InHangulSyllables>} );
ok(not "\x[D7B0]"  ~~ m/^<?InHangulSyllables>$/, q{Don't match unrelated <?InHangulSyllables>} );
ok("\x[D7B0]"  ~~ m/^<!InHangulSyllables>.$/, q{Match unrelated negated <?InHangulSyllables>} );
ok("\x[D7B0]"  ~~ m/^<-InHangulSyllables>$/, q{Match unrelated inverted <?InHangulSyllables>} );
ok("\x[D7B0]\x[CD95]" ~~ m/<?InHangulSyllables>/, q{Match unanchored <?InHangulSyllables>} );

# InHanunoo


ok("\c[HANUNOO LETTER A]" ~~ m/^<?InHanunoo>$/, q{Match <?InHanunoo>} );
ok(not "\c[HANUNOO LETTER A]" ~~ m/^<!InHanunoo>.$/, q{Don't match negated <?InHanunoo>} );
ok(not "\c[HANUNOO LETTER A]" ~~ m/^<-InHanunoo>$/, q{Don't match inverted <?InHanunoo>} );
ok(not "\x[6F4F]"  ~~ m/^<?InHanunoo>$/, q{Don't match unrelated <?InHanunoo>} );
ok("\x[6F4F]"  ~~ m/^<!InHanunoo>.$/, q{Match unrelated negated <?InHanunoo>} );
ok("\x[6F4F]"  ~~ m/^<-InHanunoo>$/, q{Match unrelated inverted <?InHanunoo>} );
ok("\x[6F4F]\c[HANUNOO LETTER A]" ~~ m/<?InHanunoo>/, q{Match unanchored <?InHanunoo>} );

# InHebrew


ok("\x[0590]" ~~ m/^<?InHebrew>$/, q{Match <?InHebrew>} );
ok(not "\x[0590]" ~~ m/^<!InHebrew>.$/, q{Don't match negated <?InHebrew>} );
ok(not "\x[0590]" ~~ m/^<-InHebrew>$/, q{Don't match inverted <?InHebrew>} );
ok(not "\x[0777]"  ~~ m/^<?InHebrew>$/, q{Don't match unrelated <?InHebrew>} );
ok("\x[0777]"  ~~ m/^<!InHebrew>.$/, q{Match unrelated negated <?InHebrew>} );
ok("\x[0777]"  ~~ m/^<-InHebrew>$/, q{Match unrelated inverted <?InHebrew>} );
ok("\x[0777]\x[0590]" ~~ m/<?InHebrew>/, q{Match unanchored <?InHebrew>} );

# InHighPrivateUseSurrogates


ok(not "\x[D04F]"  ~~ m/^<?InHighPrivateUseSurrogates>$/, q{Don't match unrelated <?InHighPrivateUseSurrogates>} );
ok("\x[D04F]"  ~~ m/^<!InHighPrivateUseSurrogates>.$/, q{Match unrelated negated <?InHighPrivateUseSurrogates>} );
ok("\x[D04F]"  ~~ m/^<-InHighPrivateUseSurrogates>$/, q{Match unrelated inverted <?InHighPrivateUseSurrogates>} );

# InHighSurrogates


ok(not "\x[D085]"  ~~ m/^<?InHighSurrogates>$/, q{Don't match unrelated <?InHighSurrogates>} );
ok("\x[D085]"  ~~ m/^<!InHighSurrogates>.$/, q{Match unrelated negated <?InHighSurrogates>} );
ok("\x[D085]"  ~~ m/^<-InHighSurrogates>$/, q{Match unrelated inverted <?InHighSurrogates>} );

# InHiragana


ok("\x[3040]" ~~ m/^<?InHiragana>$/, q{Match <?InHiragana>} );
ok(not "\x[3040]" ~~ m/^<!InHiragana>.$/, q{Don't match negated <?InHiragana>} );
ok(not "\x[3040]" ~~ m/^<-InHiragana>$/, q{Don't match inverted <?InHiragana>} );
ok(not "\x[AC7C]"  ~~ m/^<?InHiragana>$/, q{Don't match unrelated <?InHiragana>} );
ok("\x[AC7C]"  ~~ m/^<!InHiragana>.$/, q{Match unrelated negated <?InHiragana>} );
ok("\x[AC7C]"  ~~ m/^<-InHiragana>$/, q{Match unrelated inverted <?InHiragana>} );
ok("\x[AC7C]\x[3040]" ~~ m/<?InHiragana>/, q{Match unanchored <?InHiragana>} );

# InIPAExtensions


ok("\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<?InIPAExtensions>$/, q{Match <?InIPAExtensions>} );
ok(not "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<!InIPAExtensions>.$/, q{Don't match negated <?InIPAExtensions>} );
ok(not "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<-InIPAExtensions>$/, q{Don't match inverted <?InIPAExtensions>} );
ok(not "\c[HANGUL LETTER SSANGIEUNG]"  ~~ m/^<?InIPAExtensions>$/, q{Don't match unrelated <?InIPAExtensions>} );
ok("\c[HANGUL LETTER SSANGIEUNG]"  ~~ m/^<!InIPAExtensions>.$/, q{Match unrelated negated <?InIPAExtensions>} );
ok("\c[HANGUL LETTER SSANGIEUNG]"  ~~ m/^<-InIPAExtensions>$/, q{Match unrelated inverted <?InIPAExtensions>} );
ok("\c[HANGUL LETTER SSANGIEUNG]\c[LATIN SMALL LETTER TURNED A]" ~~ m/<?InIPAExtensions>/, q{Match unanchored <?InIPAExtensions>} );

# InIdeographicDescriptionCharacters


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<?InIdeographicDescriptionCharacters>$/, q{Match <?InIdeographicDescriptionCharacters>} );
ok(not "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<!InIdeographicDescriptionCharacters>.$/, q{Don't match negated <?InIdeographicDescriptionCharacters>} );
ok(not "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<-InIdeographicDescriptionCharacters>$/, q{Don't match inverted <?InIdeographicDescriptionCharacters>} );
ok(not "\x[9160]"  ~~ m/^<?InIdeographicDescriptionCharacters>$/, q{Don't match unrelated <?InIdeographicDescriptionCharacters>} );
ok("\x[9160]"  ~~ m/^<!InIdeographicDescriptionCharacters>.$/, q{Match unrelated negated <?InIdeographicDescriptionCharacters>} );
ok("\x[9160]"  ~~ m/^<-InIdeographicDescriptionCharacters>$/, q{Match unrelated inverted <?InIdeographicDescriptionCharacters>} );
ok("\x[9160]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/<?InIdeographicDescriptionCharacters>/, q{Match unanchored <?InIdeographicDescriptionCharacters>} );

# InKanbun


ok("\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<?InKanbun>$/, q{Match <?InKanbun>} );
ok(not "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<!InKanbun>.$/, q{Don't match negated <?InKanbun>} );
ok(not "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<-InKanbun>$/, q{Don't match inverted <?InKanbun>} );
ok(not "\x[A80C]"  ~~ m/^<?InKanbun>$/, q{Don't match unrelated <?InKanbun>} );
ok("\x[A80C]"  ~~ m/^<!InKanbun>.$/, q{Match unrelated negated <?InKanbun>} );
ok("\x[A80C]"  ~~ m/^<-InKanbun>$/, q{Match unrelated inverted <?InKanbun>} );
ok("\x[A80C]\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/<?InKanbun>/, q{Match unanchored <?InKanbun>} );

# InKangxiRadicals


ok("\c[KANGXI RADICAL ONE]" ~~ m/^<?InKangxiRadicals>$/, q{Match <?InKangxiRadicals>} );
ok(not "\c[KANGXI RADICAL ONE]" ~~ m/^<!InKangxiRadicals>.$/, q{Don't match negated <?InKangxiRadicals>} );
ok(not "\c[KANGXI RADICAL ONE]" ~~ m/^<-InKangxiRadicals>$/, q{Don't match inverted <?InKangxiRadicals>} );
ok(not "\x[891A]"  ~~ m/^<?InKangxiRadicals>$/, q{Don't match unrelated <?InKangxiRadicals>} );
ok("\x[891A]"  ~~ m/^<!InKangxiRadicals>.$/, q{Match unrelated negated <?InKangxiRadicals>} );
ok("\x[891A]"  ~~ m/^<-InKangxiRadicals>$/, q{Match unrelated inverted <?InKangxiRadicals>} );
ok("\x[891A]\c[KANGXI RADICAL ONE]" ~~ m/<?InKangxiRadicals>/, q{Match unanchored <?InKangxiRadicals>} );

# InKannada


ok("\x[0C80]" ~~ m/^<?InKannada>$/, q{Match <?InKannada>} );
ok(not "\x[0C80]" ~~ m/^<!InKannada>.$/, q{Don't match negated <?InKannada>} );
ok(not "\x[0C80]" ~~ m/^<-InKannada>$/, q{Don't match inverted <?InKannada>} );
ok(not "\x[B614]"  ~~ m/^<?InKannada>$/, q{Don't match unrelated <?InKannada>} );
ok("\x[B614]"  ~~ m/^<!InKannada>.$/, q{Match unrelated negated <?InKannada>} );
ok("\x[B614]"  ~~ m/^<-InKannada>$/, q{Match unrelated inverted <?InKannada>} );
ok("\x[B614]\x[0C80]" ~~ m/<?InKannada>/, q{Match unanchored <?InKannada>} );

# InKatakana


ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<?InKatakana>$/, q{Match <?InKatakana>} );
ok(not "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<!InKatakana>.$/, q{Don't match negated <?InKatakana>} );
ok(not "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<-InKatakana>$/, q{Don't match inverted <?InKatakana>} );
ok(not "\x[7EB8]"  ~~ m/^<?InKatakana>$/, q{Don't match unrelated <?InKatakana>} );
ok("\x[7EB8]"  ~~ m/^<!InKatakana>.$/, q{Match unrelated negated <?InKatakana>} );
ok("\x[7EB8]"  ~~ m/^<-InKatakana>$/, q{Match unrelated inverted <?InKatakana>} );
ok("\x[7EB8]\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/<?InKatakana>/, q{Match unanchored <?InKatakana>} );

# InKatakanaPhoneticExtensions


ok("\c[KATAKANA LETTER SMALL KU]" ~~ m/^<?InKatakanaPhoneticExtensions>$/, q{Match <?InKatakanaPhoneticExtensions>} );
ok(not "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<!InKatakanaPhoneticExtensions>.$/, q{Don't match negated <?InKatakanaPhoneticExtensions>} );
ok(not "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<-InKatakanaPhoneticExtensions>$/, q{Don't match inverted <?InKatakanaPhoneticExtensions>} );
ok(not "\x[97C2]"  ~~ m/^<?InKatakanaPhoneticExtensions>$/, q{Don't match unrelated <?InKatakanaPhoneticExtensions>} );
ok("\x[97C2]"  ~~ m/^<!InKatakanaPhoneticExtensions>.$/, q{Match unrelated negated <?InKatakanaPhoneticExtensions>} );
ok("\x[97C2]"  ~~ m/^<-InKatakanaPhoneticExtensions>$/, q{Match unrelated inverted <?InKatakanaPhoneticExtensions>} );
ok("\x[97C2]\c[KATAKANA LETTER SMALL KU]" ~~ m/<?InKatakanaPhoneticExtensions>/, q{Match unanchored <?InKatakanaPhoneticExtensions>} );

# InKhmer


ok("\c[KHMER LETTER KA]" ~~ m/^<?InKhmer>$/, q{Match <?InKhmer>} );
ok(not "\c[KHMER LETTER KA]" ~~ m/^<!InKhmer>.$/, q{Don't match negated <?InKhmer>} );
ok(not "\c[KHMER LETTER KA]" ~~ m/^<-InKhmer>$/, q{Don't match inverted <?InKhmer>} );
ok(not "\x[CAFA]"  ~~ m/^<?InKhmer>$/, q{Don't match unrelated <?InKhmer>} );
ok("\x[CAFA]"  ~~ m/^<!InKhmer>.$/, q{Match unrelated negated <?InKhmer>} );
ok("\x[CAFA]"  ~~ m/^<-InKhmer>$/, q{Match unrelated inverted <?InKhmer>} );
ok("\x[CAFA]\c[KHMER LETTER KA]" ~~ m/<?InKhmer>/, q{Match unanchored <?InKhmer>} );

# InLao


ok("\x[0E80]" ~~ m/^<?InLao>$/, q{Match <?InLao>} );
ok(not "\x[0E80]" ~~ m/^<!InLao>.$/, q{Don't match negated <?InLao>} );
ok(not "\x[0E80]" ~~ m/^<-InLao>$/, q{Don't match inverted <?InLao>} );
ok(not "\x[07BF]"  ~~ m/^<?InLao>$/, q{Don't match unrelated <?InLao>} );
ok("\x[07BF]"  ~~ m/^<!InLao>.$/, q{Match unrelated negated <?InLao>} );
ok("\x[07BF]"  ~~ m/^<-InLao>$/, q{Match unrelated inverted <?InLao>} );
ok("\x[07BF]\x[0E80]" ~~ m/<?InLao>/, q{Match unanchored <?InLao>} );

# InLatin1Supplement


ok("\x[0080]" ~~ m/^<?InLatin1Supplement>$/, q{Match <?InLatin1Supplement>} );
ok(not "\x[0080]" ~~ m/^<!InLatin1Supplement>.$/, q{Don't match negated <?InLatin1Supplement>} );
ok(not "\x[0080]" ~~ m/^<-InLatin1Supplement>$/, q{Don't match inverted <?InLatin1Supplement>} );
ok(not "\x[D062]"  ~~ m/^<?InLatin1Supplement>$/, q{Don't match unrelated <?InLatin1Supplement>} );
ok("\x[D062]"  ~~ m/^<!InLatin1Supplement>.$/, q{Match unrelated negated <?InLatin1Supplement>} );
ok("\x[D062]"  ~~ m/^<-InLatin1Supplement>$/, q{Match unrelated inverted <?InLatin1Supplement>} );
ok("\x[D062]\x[0080]" ~~ m/<?InLatin1Supplement>/, q{Match unanchored <?InLatin1Supplement>} );

# InLatinExtendedA


ok("\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<?InLatinExtendedA>$/, q{Match <?InLatinExtendedA>} );
ok(not "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<!InLatinExtendedA>.$/, q{Don't match negated <?InLatinExtendedA>} );
ok(not "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<-InLatinExtendedA>$/, q{Don't match inverted <?InLatinExtendedA>} );
ok(not "\c[IDEOGRAPHIC ANNOTATION EARTH MARK]"  ~~ m/^<?InLatinExtendedA>$/, q{Don't match unrelated <?InLatinExtendedA>} );
ok("\c[IDEOGRAPHIC ANNOTATION EARTH MARK]"  ~~ m/^<!InLatinExtendedA>.$/, q{Match unrelated negated <?InLatinExtendedA>} );
ok("\c[IDEOGRAPHIC ANNOTATION EARTH MARK]"  ~~ m/^<-InLatinExtendedA>$/, q{Match unrelated inverted <?InLatinExtendedA>} );
ok("\c[IDEOGRAPHIC ANNOTATION EARTH MARK]\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/<?InLatinExtendedA>/, q{Match unanchored <?InLatinExtendedA>} );

# InLatinExtendedAdditional


ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<?InLatinExtendedAdditional>$/, q{Match <?InLatinExtendedAdditional>} );
ok(not "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<!InLatinExtendedAdditional>.$/, q{Don't match negated <?InLatinExtendedAdditional>} );
ok(not "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<-InLatinExtendedAdditional>$/, q{Don't match inverted <?InLatinExtendedAdditional>} );
ok(not "\x[9A44]"  ~~ m/^<?InLatinExtendedAdditional>$/, q{Don't match unrelated <?InLatinExtendedAdditional>} );
ok("\x[9A44]"  ~~ m/^<!InLatinExtendedAdditional>.$/, q{Match unrelated negated <?InLatinExtendedAdditional>} );
ok("\x[9A44]"  ~~ m/^<-InLatinExtendedAdditional>$/, q{Match unrelated inverted <?InLatinExtendedAdditional>} );
ok("\x[9A44]\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/<?InLatinExtendedAdditional>/, q{Match unanchored <?InLatinExtendedAdditional>} );

# InLatinExtendedB


ok("\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<?InLatinExtendedB>$/, q{Match <?InLatinExtendedB>} );
ok(not "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<!InLatinExtendedB>.$/, q{Don't match negated <?InLatinExtendedB>} );
ok(not "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<-InLatinExtendedB>$/, q{Don't match inverted <?InLatinExtendedB>} );
ok(not "\x[7544]"  ~~ m/^<?InLatinExtendedB>$/, q{Don't match unrelated <?InLatinExtendedB>} );
ok("\x[7544]"  ~~ m/^<!InLatinExtendedB>.$/, q{Match unrelated negated <?InLatinExtendedB>} );
ok("\x[7544]"  ~~ m/^<-InLatinExtendedB>$/, q{Match unrelated inverted <?InLatinExtendedB>} );
ok("\x[7544]\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/<?InLatinExtendedB>/, q{Match unanchored <?InLatinExtendedB>} );

# InLetterlikeSymbols


ok("\c[ACCOUNT OF]" ~~ m/^<?InLetterlikeSymbols>$/, q{Match <?InLetterlikeSymbols>} );
ok(not "\c[ACCOUNT OF]" ~~ m/^<!InLetterlikeSymbols>.$/, q{Don't match negated <?InLetterlikeSymbols>} );
ok(not "\c[ACCOUNT OF]" ~~ m/^<-InLetterlikeSymbols>$/, q{Don't match inverted <?InLetterlikeSymbols>} );
ok(not "\c[LATIN CAPITAL LETTER X WITH DOT ABOVE]"  ~~ m/^<?InLetterlikeSymbols>$/, q{Don't match unrelated <?InLetterlikeSymbols>} );
ok("\c[LATIN CAPITAL LETTER X WITH DOT ABOVE]"  ~~ m/^<!InLetterlikeSymbols>.$/, q{Match unrelated negated <?InLetterlikeSymbols>} );
ok("\c[LATIN CAPITAL LETTER X WITH DOT ABOVE]"  ~~ m/^<-InLetterlikeSymbols>$/, q{Match unrelated inverted <?InLetterlikeSymbols>} );
ok("\c[LATIN CAPITAL LETTER X WITH DOT ABOVE]\c[ACCOUNT OF]" ~~ m/<?InLetterlikeSymbols>/, q{Match unanchored <?InLetterlikeSymbols>} );

# InLowSurrogates


ok(not "\x[5ECC]"  ~~ m/^<?InLowSurrogates>$/, q{Don't match unrelated <?InLowSurrogates>} );
ok("\x[5ECC]"  ~~ m/^<!InLowSurrogates>.$/, q{Match unrelated negated <?InLowSurrogates>} );
ok("\x[5ECC]"  ~~ m/^<-InLowSurrogates>$/, q{Match unrelated inverted <?InLowSurrogates>} );

# InMalayalam


ok("\x[0D00]" ~~ m/^<?InMalayalam>$/, q{Match <?InMalayalam>} );
ok(not "\x[0D00]" ~~ m/^<!InMalayalam>.$/, q{Don't match negated <?InMalayalam>} );
ok(not "\x[0D00]" ~~ m/^<-InMalayalam>$/, q{Don't match inverted <?InMalayalam>} );
ok(not "\x[3457]"  ~~ m/^<?InMalayalam>$/, q{Don't match unrelated <?InMalayalam>} );
ok("\x[3457]"  ~~ m/^<!InMalayalam>.$/, q{Match unrelated negated <?InMalayalam>} );
ok("\x[3457]"  ~~ m/^<-InMalayalam>$/, q{Match unrelated inverted <?InMalayalam>} );
ok("\x[3457]\x[0D00]" ~~ m/<?InMalayalam>/, q{Match unanchored <?InMalayalam>} );

# InMathematicalAlphanumericSymbols


ok(not "\x[6B79]"  ~~ m/^<?InMathematicalAlphanumericSymbols>$/, q{Don't match unrelated <?InMathematicalAlphanumericSymbols>} );
ok("\x[6B79]"  ~~ m/^<!InMathematicalAlphanumericSymbols>.$/, q{Match unrelated negated <?InMathematicalAlphanumericSymbols>} );
ok("\x[6B79]"  ~~ m/^<-InMathematicalAlphanumericSymbols>$/, q{Match unrelated inverted <?InMathematicalAlphanumericSymbols>} );

# InMathematicalOperators


ok("\c[FOR ALL]" ~~ m/^<?InMathematicalOperators>$/, q{Match <?InMathematicalOperators>} );
ok(not "\c[FOR ALL]" ~~ m/^<!InMathematicalOperators>.$/, q{Don't match negated <?InMathematicalOperators>} );
ok(not "\c[FOR ALL]" ~~ m/^<-InMathematicalOperators>$/, q{Don't match inverted <?InMathematicalOperators>} );
ok(not "\x[BBC6]"  ~~ m/^<?InMathematicalOperators>$/, q{Don't match unrelated <?InMathematicalOperators>} );
ok("\x[BBC6]"  ~~ m/^<!InMathematicalOperators>.$/, q{Match unrelated negated <?InMathematicalOperators>} );
ok("\x[BBC6]"  ~~ m/^<-InMathematicalOperators>$/, q{Match unrelated inverted <?InMathematicalOperators>} );
ok("\x[BBC6]\c[FOR ALL]" ~~ m/<?InMathematicalOperators>/, q{Match unanchored <?InMathematicalOperators>} );

# InMiscellaneousMathematicalSymbolsA


ok("\x[27C0]" ~~ m/^<?InMiscellaneousMathematicalSymbolsA>$/, q{Match <?InMiscellaneousMathematicalSymbolsA>} );
ok(not "\x[27C0]" ~~ m/^<!InMiscellaneousMathematicalSymbolsA>.$/, q{Don't match negated <?InMiscellaneousMathematicalSymbolsA>} );
ok(not "\x[27C0]" ~~ m/^<-InMiscellaneousMathematicalSymbolsA>$/, q{Don't match inverted <?InMiscellaneousMathematicalSymbolsA>} );
ok(not "\x[065D]"  ~~ m/^<?InMiscellaneousMathematicalSymbolsA>$/, q{Don't match unrelated <?InMiscellaneousMathematicalSymbolsA>} );
ok("\x[065D]"  ~~ m/^<!InMiscellaneousMathematicalSymbolsA>.$/, q{Match unrelated negated <?InMiscellaneousMathematicalSymbolsA>} );
ok("\x[065D]"  ~~ m/^<-InMiscellaneousMathematicalSymbolsA>$/, q{Match unrelated inverted <?InMiscellaneousMathematicalSymbolsA>} );
ok("\x[065D]\x[27C0]" ~~ m/<?InMiscellaneousMathematicalSymbolsA>/, q{Match unanchored <?InMiscellaneousMathematicalSymbolsA>} );

# InMiscellaneousMathematicalSymbolsB


ok("\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<?InMiscellaneousMathematicalSymbolsB>$/, q{Match <?InMiscellaneousMathematicalSymbolsB>} );
ok(not "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<!InMiscellaneousMathematicalSymbolsB>.$/, q{Don't match negated <?InMiscellaneousMathematicalSymbolsB>} );
ok(not "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<-InMiscellaneousMathematicalSymbolsB>$/, q{Don't match inverted <?InMiscellaneousMathematicalSymbolsB>} );
ok(not "\x[56A6]"  ~~ m/^<?InMiscellaneousMathematicalSymbolsB>$/, q{Don't match unrelated <?InMiscellaneousMathematicalSymbolsB>} );
ok("\x[56A6]"  ~~ m/^<!InMiscellaneousMathematicalSymbolsB>.$/, q{Match unrelated negated <?InMiscellaneousMathematicalSymbolsB>} );
ok("\x[56A6]"  ~~ m/^<-InMiscellaneousMathematicalSymbolsB>$/, q{Match unrelated inverted <?InMiscellaneousMathematicalSymbolsB>} );
ok("\x[56A6]\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/<?InMiscellaneousMathematicalSymbolsB>/, q{Match unanchored <?InMiscellaneousMathematicalSymbolsB>} );

# InMiscellaneousSymbols


ok("\c[BLACK SUN WITH RAYS]" ~~ m/^<?InMiscellaneousSymbols>$/, q{Match <?InMiscellaneousSymbols>} );
ok(not "\c[BLACK SUN WITH RAYS]" ~~ m/^<!InMiscellaneousSymbols>.$/, q{Don't match negated <?InMiscellaneousSymbols>} );
ok(not "\c[BLACK SUN WITH RAYS]" ~~ m/^<-InMiscellaneousSymbols>$/, q{Don't match inverted <?InMiscellaneousSymbols>} );
ok(not "\x[3EE7]"  ~~ m/^<?InMiscellaneousSymbols>$/, q{Don't match unrelated <?InMiscellaneousSymbols>} );
ok("\x[3EE7]"  ~~ m/^<!InMiscellaneousSymbols>.$/, q{Match unrelated negated <?InMiscellaneousSymbols>} );
ok("\x[3EE7]"  ~~ m/^<-InMiscellaneousSymbols>$/, q{Match unrelated inverted <?InMiscellaneousSymbols>} );
ok("\x[3EE7]\c[BLACK SUN WITH RAYS]" ~~ m/<?InMiscellaneousSymbols>/, q{Match unanchored <?InMiscellaneousSymbols>} );

# InMiscellaneousTechnical


ok("\c[DIAMETER SIGN]" ~~ m/^<?InMiscellaneousTechnical>$/, q{Match <?InMiscellaneousTechnical>} );
ok(not "\c[DIAMETER SIGN]" ~~ m/^<!InMiscellaneousTechnical>.$/, q{Don't match negated <?InMiscellaneousTechnical>} );
ok(not "\c[DIAMETER SIGN]" ~~ m/^<-InMiscellaneousTechnical>$/, q{Don't match inverted <?InMiscellaneousTechnical>} );
ok(not "\x[2EFC]"  ~~ m/^<?InMiscellaneousTechnical>$/, q{Don't match unrelated <?InMiscellaneousTechnical>} );
ok("\x[2EFC]"  ~~ m/^<!InMiscellaneousTechnical>.$/, q{Match unrelated negated <?InMiscellaneousTechnical>} );
ok("\x[2EFC]"  ~~ m/^<-InMiscellaneousTechnical>$/, q{Match unrelated inverted <?InMiscellaneousTechnical>} );
ok("\x[2EFC]\c[DIAMETER SIGN]" ~~ m/<?InMiscellaneousTechnical>/, q{Match unanchored <?InMiscellaneousTechnical>} );

# InMongolian


ok("\c[MONGOLIAN BIRGA]" ~~ m/^<?InMongolian>$/, q{Match <?InMongolian>} );
ok(not "\c[MONGOLIAN BIRGA]" ~~ m/^<!InMongolian>.$/, q{Don't match negated <?InMongolian>} );
ok(not "\c[MONGOLIAN BIRGA]" ~~ m/^<-InMongolian>$/, q{Don't match inverted <?InMongolian>} );
ok(not "\x[AFB4]"  ~~ m/^<?InMongolian>$/, q{Don't match unrelated <?InMongolian>} );
ok("\x[AFB4]"  ~~ m/^<!InMongolian>.$/, q{Match unrelated negated <?InMongolian>} );
ok("\x[AFB4]"  ~~ m/^<-InMongolian>$/, q{Match unrelated inverted <?InMongolian>} );
ok("\x[AFB4]\c[MONGOLIAN BIRGA]" ~~ m/<?InMongolian>/, q{Match unanchored <?InMongolian>} );

# InMusicalSymbols


ok(not "\x[0CE4]"  ~~ m/^<?InMusicalSymbols>$/, q{Don't match unrelated <?InMusicalSymbols>} );
ok("\x[0CE4]"  ~~ m/^<!InMusicalSymbols>.$/, q{Match unrelated negated <?InMusicalSymbols>} );
ok("\x[0CE4]"  ~~ m/^<-InMusicalSymbols>$/, q{Match unrelated inverted <?InMusicalSymbols>} );

# InMyanmar


ok("\c[MYANMAR LETTER KA]" ~~ m/^<?InMyanmar>$/, q{Match <?InMyanmar>} );
ok(not "\c[MYANMAR LETTER KA]" ~~ m/^<!InMyanmar>.$/, q{Don't match negated <?InMyanmar>} );
ok(not "\c[MYANMAR LETTER KA]" ~~ m/^<-InMyanmar>$/, q{Don't match inverted <?InMyanmar>} );
ok(not "\x[1DDB]"  ~~ m/^<?InMyanmar>$/, q{Don't match unrelated <?InMyanmar>} );
ok("\x[1DDB]"  ~~ m/^<!InMyanmar>.$/, q{Match unrelated negated <?InMyanmar>} );
ok("\x[1DDB]"  ~~ m/^<-InMyanmar>$/, q{Match unrelated inverted <?InMyanmar>} );
ok("\x[1DDB]\c[MYANMAR LETTER KA]" ~~ m/<?InMyanmar>/, q{Match unanchored <?InMyanmar>} );

# InNumberForms


ok("\x[2150]" ~~ m/^<?InNumberForms>$/, q{Match <?InNumberForms>} );
ok(not "\x[2150]" ~~ m/^<!InNumberForms>.$/, q{Don't match negated <?InNumberForms>} );
ok(not "\x[2150]" ~~ m/^<-InNumberForms>$/, q{Don't match inverted <?InNumberForms>} );
ok(not "\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<?InNumberForms>$/, q{Don't match unrelated <?InNumberForms>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<!InNumberForms>.$/, q{Match unrelated negated <?InNumberForms>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<-InNumberForms>$/, q{Match unrelated inverted <?InNumberForms>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]\x[2150]" ~~ m/<?InNumberForms>/, q{Match unanchored <?InNumberForms>} );

# InOgham


ok("\c[OGHAM SPACE MARK]" ~~ m/^<?InOgham>$/, q{Match <?InOgham>} );
ok(not "\c[OGHAM SPACE MARK]" ~~ m/^<!InOgham>.$/, q{Don't match negated <?InOgham>} );
ok(not "\c[OGHAM SPACE MARK]" ~~ m/^<-InOgham>$/, q{Don't match inverted <?InOgham>} );
ok(not "\x[768C]"  ~~ m/^<?InOgham>$/, q{Don't match unrelated <?InOgham>} );
ok("\x[768C]"  ~~ m/^<!InOgham>.$/, q{Match unrelated negated <?InOgham>} );
ok("\x[768C]"  ~~ m/^<-InOgham>$/, q{Match unrelated inverted <?InOgham>} );
ok("\x[768C]\c[OGHAM SPACE MARK]" ~~ m/<?InOgham>/, q{Match unanchored <?InOgham>} );

# InOldItalic


ok(not "\x[C597]"  ~~ m/^<?InOldItalic>$/, q{Don't match unrelated <?InOldItalic>} );
ok("\x[C597]"  ~~ m/^<!InOldItalic>.$/, q{Match unrelated negated <?InOldItalic>} );
ok("\x[C597]"  ~~ m/^<-InOldItalic>$/, q{Match unrelated inverted <?InOldItalic>} );

# InOpticalCharacterRecognition


ok("\c[OCR HOOK]" ~~ m/^<?InOpticalCharacterRecognition>$/, q{Match <?InOpticalCharacterRecognition>} );
ok(not "\c[OCR HOOK]" ~~ m/^<!InOpticalCharacterRecognition>.$/, q{Don't match negated <?InOpticalCharacterRecognition>} );
ok(not "\c[OCR HOOK]" ~~ m/^<-InOpticalCharacterRecognition>$/, q{Don't match inverted <?InOpticalCharacterRecognition>} );
ok(not "\x[BE80]"  ~~ m/^<?InOpticalCharacterRecognition>$/, q{Don't match unrelated <?InOpticalCharacterRecognition>} );
ok("\x[BE80]"  ~~ m/^<!InOpticalCharacterRecognition>.$/, q{Match unrelated negated <?InOpticalCharacterRecognition>} );
ok("\x[BE80]"  ~~ m/^<-InOpticalCharacterRecognition>$/, q{Match unrelated inverted <?InOpticalCharacterRecognition>} );
ok("\x[BE80]\c[OCR HOOK]" ~~ m/<?InOpticalCharacterRecognition>/, q{Match unanchored <?InOpticalCharacterRecognition>} );

# InOriya


ok("\x[0B00]" ~~ m/^<?InOriya>$/, q{Match <?InOriya>} );
ok(not "\x[0B00]" ~~ m/^<!InOriya>.$/, q{Don't match negated <?InOriya>} );
ok(not "\x[0B00]" ~~ m/^<-InOriya>$/, q{Don't match inverted <?InOriya>} );
ok(not "\c[YI SYLLABLE GGEX]"  ~~ m/^<?InOriya>$/, q{Don't match unrelated <?InOriya>} );
ok("\c[YI SYLLABLE GGEX]"  ~~ m/^<!InOriya>.$/, q{Match unrelated negated <?InOriya>} );
ok("\c[YI SYLLABLE GGEX]"  ~~ m/^<-InOriya>$/, q{Match unrelated inverted <?InOriya>} );
ok("\c[YI SYLLABLE GGEX]\x[0B00]" ~~ m/<?InOriya>/, q{Match unanchored <?InOriya>} );

# InPrivateUseArea


ok(not "\x[B6B1]"  ~~ m/^<?InPrivateUseArea>$/, q{Don't match unrelated <?InPrivateUseArea>} );
ok("\x[B6B1]"  ~~ m/^<!InPrivateUseArea>.$/, q{Match unrelated negated <?InPrivateUseArea>} );
ok("\x[B6B1]"  ~~ m/^<-InPrivateUseArea>$/, q{Match unrelated inverted <?InPrivateUseArea>} );

# InRunic


ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<?InRunic>$/, q{Match <?InRunic>} );
ok(not "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<!InRunic>.$/, q{Don't match negated <?InRunic>} );
ok(not "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<-InRunic>$/, q{Don't match inverted <?InRunic>} );
ok(not "\c[SINHALA LETTER MAHAAPRAANA KAYANNA]"  ~~ m/^<?InRunic>$/, q{Don't match unrelated <?InRunic>} );
ok("\c[SINHALA LETTER MAHAAPRAANA KAYANNA]"  ~~ m/^<!InRunic>.$/, q{Match unrelated negated <?InRunic>} );
ok("\c[SINHALA LETTER MAHAAPRAANA KAYANNA]"  ~~ m/^<-InRunic>$/, q{Match unrelated inverted <?InRunic>} );
ok("\c[SINHALA LETTER MAHAAPRAANA KAYANNA]\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/<?InRunic>/, q{Match unanchored <?InRunic>} );

# InSinhala


ok("\x[0D80]" ~~ m/^<?InSinhala>$/, q{Match <?InSinhala>} );
ok(not "\x[0D80]" ~~ m/^<!InSinhala>.$/, q{Don't match negated <?InSinhala>} );
ok(not "\x[0D80]" ~~ m/^<-InSinhala>$/, q{Don't match inverted <?InSinhala>} );
ok(not "\x[1060]"  ~~ m/^<?InSinhala>$/, q{Don't match unrelated <?InSinhala>} );
ok("\x[1060]"  ~~ m/^<!InSinhala>.$/, q{Match unrelated negated <?InSinhala>} );
ok("\x[1060]"  ~~ m/^<-InSinhala>$/, q{Match unrelated inverted <?InSinhala>} );
ok("\x[1060]\x[0D80]" ~~ m/<?InSinhala>/, q{Match unanchored <?InSinhala>} );

# InSmallFormVariants


ok(not "\x[5285]"  ~~ m/^<?InSmallFormVariants>$/, q{Don't match unrelated <?InSmallFormVariants>} );
ok("\x[5285]"  ~~ m/^<!InSmallFormVariants>.$/, q{Match unrelated negated <?InSmallFormVariants>} );
ok("\x[5285]"  ~~ m/^<-InSmallFormVariants>$/, q{Match unrelated inverted <?InSmallFormVariants>} );

# InSpacingModifierLetters


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<?InSpacingModifierLetters>$/, q{Match <?InSpacingModifierLetters>} );
ok(not "\c[MODIFIER LETTER SMALL H]" ~~ m/^<!InSpacingModifierLetters>.$/, q{Don't match negated <?InSpacingModifierLetters>} );
ok(not "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-InSpacingModifierLetters>$/, q{Don't match inverted <?InSpacingModifierLetters>} );
ok(not "\x[5326]"  ~~ m/^<?InSpacingModifierLetters>$/, q{Don't match unrelated <?InSpacingModifierLetters>} );
ok("\x[5326]"  ~~ m/^<!InSpacingModifierLetters>.$/, q{Match unrelated negated <?InSpacingModifierLetters>} );
ok("\x[5326]"  ~~ m/^<-InSpacingModifierLetters>$/, q{Match unrelated inverted <?InSpacingModifierLetters>} );
ok("\x[5326]\c[MODIFIER LETTER SMALL H]" ~~ m/<?InSpacingModifierLetters>/, q{Match unanchored <?InSpacingModifierLetters>} );

# InSpecials


ok(not "\x[3DF1]"  ~~ m/^<?InSpecials>$/, q{Don't match unrelated <?InSpecials>} );
ok("\x[3DF1]"  ~~ m/^<!InSpecials>.$/, q{Match unrelated negated <?InSpecials>} );
ok("\x[3DF1]"  ~~ m/^<-InSpecials>$/, q{Match unrelated inverted <?InSpecials>} );

# InSuperscriptsAndSubscripts


ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<?InSuperscriptsAndSubscripts>$/, q{Match <?InSuperscriptsAndSubscripts>} );
ok(not "\c[SUPERSCRIPT ZERO]" ~~ m/^<!InSuperscriptsAndSubscripts>.$/, q{Don't match negated <?InSuperscriptsAndSubscripts>} );
ok(not "\c[SUPERSCRIPT ZERO]" ~~ m/^<-InSuperscriptsAndSubscripts>$/, q{Don't match inverted <?InSuperscriptsAndSubscripts>} );
ok(not "\x[3E71]"  ~~ m/^<?InSuperscriptsAndSubscripts>$/, q{Don't match unrelated <?InSuperscriptsAndSubscripts>} );
ok("\x[3E71]"  ~~ m/^<!InSuperscriptsAndSubscripts>.$/, q{Match unrelated negated <?InSuperscriptsAndSubscripts>} );
ok("\x[3E71]"  ~~ m/^<-InSuperscriptsAndSubscripts>$/, q{Match unrelated inverted <?InSuperscriptsAndSubscripts>} );
ok("\x[3E71]\c[SUPERSCRIPT ZERO]" ~~ m/<?InSuperscriptsAndSubscripts>/, q{Match unanchored <?InSuperscriptsAndSubscripts>} );

# InSupplementalArrowsA


ok("\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<?InSupplementalArrowsA>$/, q{Match <?InSupplementalArrowsA>} );
ok(not "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<!InSupplementalArrowsA>.$/, q{Don't match negated <?InSupplementalArrowsA>} );
ok(not "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<-InSupplementalArrowsA>$/, q{Don't match inverted <?InSupplementalArrowsA>} );
ok(not "\c[GREEK SMALL LETTER OMICRON WITH TONOS]"  ~~ m/^<?InSupplementalArrowsA>$/, q{Don't match unrelated <?InSupplementalArrowsA>} );
ok("\c[GREEK SMALL LETTER OMICRON WITH TONOS]"  ~~ m/^<!InSupplementalArrowsA>.$/, q{Match unrelated negated <?InSupplementalArrowsA>} );
ok("\c[GREEK SMALL LETTER OMICRON WITH TONOS]"  ~~ m/^<-InSupplementalArrowsA>$/, q{Match unrelated inverted <?InSupplementalArrowsA>} );
ok("\c[GREEK SMALL LETTER OMICRON WITH TONOS]\c[UPWARDS QUADRUPLE ARROW]" ~~ m/<?InSupplementalArrowsA>/, q{Match unanchored <?InSupplementalArrowsA>} );

# InSupplementalArrowsB


ok("\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<?InSupplementalArrowsB>$/, q{Match <?InSupplementalArrowsB>} );
ok(not "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<!InSupplementalArrowsB>.$/, q{Don't match negated <?InSupplementalArrowsB>} );
ok(not "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<-InSupplementalArrowsB>$/, q{Don't match inverted <?InSupplementalArrowsB>} );
ok(not "\x[C1A9]"  ~~ m/^<?InSupplementalArrowsB>$/, q{Don't match unrelated <?InSupplementalArrowsB>} );
ok("\x[C1A9]"  ~~ m/^<!InSupplementalArrowsB>.$/, q{Match unrelated negated <?InSupplementalArrowsB>} );
ok("\x[C1A9]"  ~~ m/^<-InSupplementalArrowsB>$/, q{Match unrelated inverted <?InSupplementalArrowsB>} );
ok("\x[C1A9]\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/<?InSupplementalArrowsB>/, q{Match unanchored <?InSupplementalArrowsB>} );

# InSupplementalMathematicalOperators


ok("\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<?InSupplementalMathematicalOperators>$/, q{Match <?InSupplementalMathematicalOperators>} );
ok(not "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<!InSupplementalMathematicalOperators>.$/, q{Don't match negated <?InSupplementalMathematicalOperators>} );
ok(not "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<-InSupplementalMathematicalOperators>$/, q{Don't match inverted <?InSupplementalMathematicalOperators>} );
ok(not "\x[9EBD]"  ~~ m/^<?InSupplementalMathematicalOperators>$/, q{Don't match unrelated <?InSupplementalMathematicalOperators>} );
ok("\x[9EBD]"  ~~ m/^<!InSupplementalMathematicalOperators>.$/, q{Match unrelated negated <?InSupplementalMathematicalOperators>} );
ok("\x[9EBD]"  ~~ m/^<-InSupplementalMathematicalOperators>$/, q{Match unrelated inverted <?InSupplementalMathematicalOperators>} );
ok("\x[9EBD]\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/<?InSupplementalMathematicalOperators>/, q{Match unanchored <?InSupplementalMathematicalOperators>} );

# InSupplementaryPrivateUseAreaA


ok(not "\x[07E3]"  ~~ m/^<?InSupplementaryPrivateUseAreaA>$/, q{Don't match unrelated <?InSupplementaryPrivateUseAreaA>} );
ok("\x[07E3]"  ~~ m/^<!InSupplementaryPrivateUseAreaA>.$/, q{Match unrelated negated <?InSupplementaryPrivateUseAreaA>} );
ok("\x[07E3]"  ~~ m/^<-InSupplementaryPrivateUseAreaA>$/, q{Match unrelated inverted <?InSupplementaryPrivateUseAreaA>} );

# InSupplementaryPrivateUseAreaB


ok(not "\x[4C48]"  ~~ m/^<?InSupplementaryPrivateUseAreaB>$/, q{Don't match unrelated <?InSupplementaryPrivateUseAreaB>} );
ok("\x[4C48]"  ~~ m/^<!InSupplementaryPrivateUseAreaB>.$/, q{Match unrelated negated <?InSupplementaryPrivateUseAreaB>} );
ok("\x[4C48]"  ~~ m/^<-InSupplementaryPrivateUseAreaB>$/, q{Match unrelated inverted <?InSupplementaryPrivateUseAreaB>} );

# InSyriac


ok("\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<?InSyriac>$/, q{Match <?InSyriac>} );
ok(not "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<!InSyriac>.$/, q{Don't match negated <?InSyriac>} );
ok(not "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<-InSyriac>$/, q{Don't match inverted <?InSyriac>} );
ok(not "\c[YI SYLLABLE NZIEP]"  ~~ m/^<?InSyriac>$/, q{Don't match unrelated <?InSyriac>} );
ok("\c[YI SYLLABLE NZIEP]"  ~~ m/^<!InSyriac>.$/, q{Match unrelated negated <?InSyriac>} );
ok("\c[YI SYLLABLE NZIEP]"  ~~ m/^<-InSyriac>$/, q{Match unrelated inverted <?InSyriac>} );
ok("\c[YI SYLLABLE NZIEP]\c[SYRIAC END OF PARAGRAPH]" ~~ m/<?InSyriac>/, q{Match unanchored <?InSyriac>} );

# InTagalog


ok("\c[TAGALOG LETTER A]" ~~ m/^<?InTagalog>$/, q{Match <?InTagalog>} );
ok(not "\c[TAGALOG LETTER A]" ~~ m/^<!InTagalog>.$/, q{Don't match negated <?InTagalog>} );
ok(not "\c[TAGALOG LETTER A]" ~~ m/^<-InTagalog>$/, q{Don't match inverted <?InTagalog>} );
ok(not "\c[GEORGIAN LETTER BAN]"  ~~ m/^<?InTagalog>$/, q{Don't match unrelated <?InTagalog>} );
ok("\c[GEORGIAN LETTER BAN]"  ~~ m/^<!InTagalog>.$/, q{Match unrelated negated <?InTagalog>} );
ok("\c[GEORGIAN LETTER BAN]"  ~~ m/^<-InTagalog>$/, q{Match unrelated inverted <?InTagalog>} );
ok("\c[GEORGIAN LETTER BAN]\c[TAGALOG LETTER A]" ~~ m/<?InTagalog>/, q{Match unanchored <?InTagalog>} );

# InTagbanwa


ok("\c[TAGBANWA LETTER A]" ~~ m/^<?InTagbanwa>$/, q{Match <?InTagbanwa>} );
ok(not "\c[TAGBANWA LETTER A]" ~~ m/^<!InTagbanwa>.$/, q{Don't match negated <?InTagbanwa>} );
ok(not "\c[TAGBANWA LETTER A]" ~~ m/^<-InTagbanwa>$/, q{Don't match inverted <?InTagbanwa>} );
ok(not "\x[5776]"  ~~ m/^<?InTagbanwa>$/, q{Don't match unrelated <?InTagbanwa>} );
ok("\x[5776]"  ~~ m/^<!InTagbanwa>.$/, q{Match unrelated negated <?InTagbanwa>} );
ok("\x[5776]"  ~~ m/^<-InTagbanwa>$/, q{Match unrelated inverted <?InTagbanwa>} );
ok("\x[5776]\c[TAGBANWA LETTER A]" ~~ m/<?InTagbanwa>/, q{Match unanchored <?InTagbanwa>} );

# InTags


ok(not "\x[3674]"  ~~ m/^<?InTags>$/, q{Don't match unrelated <?InTags>} );
ok("\x[3674]"  ~~ m/^<!InTags>.$/, q{Match unrelated negated <?InTags>} );
ok("\x[3674]"  ~~ m/^<-InTags>$/, q{Match unrelated inverted <?InTags>} );

# InTamil


ok("\x[0B80]" ~~ m/^<?InTamil>$/, q{Match <?InTamil>} );
ok(not "\x[0B80]" ~~ m/^<!InTamil>.$/, q{Don't match negated <?InTamil>} );
ok(not "\x[0B80]" ~~ m/^<-InTamil>$/, q{Don't match inverted <?InTamil>} );
ok(not "\x[B58F]"  ~~ m/^<?InTamil>$/, q{Don't match unrelated <?InTamil>} );
ok("\x[B58F]"  ~~ m/^<!InTamil>.$/, q{Match unrelated negated <?InTamil>} );
ok("\x[B58F]"  ~~ m/^<-InTamil>$/, q{Match unrelated inverted <?InTamil>} );
ok("\x[B58F]\x[0B80]" ~~ m/<?InTamil>/, q{Match unanchored <?InTamil>} );

# InTelugu


ok("\x[0C00]" ~~ m/^<?InTelugu>$/, q{Match <?InTelugu>} );
ok(not "\x[0C00]" ~~ m/^<!InTelugu>.$/, q{Don't match negated <?InTelugu>} );
ok(not "\x[0C00]" ~~ m/^<-InTelugu>$/, q{Don't match inverted <?InTelugu>} );
ok(not "\x[8AC5]"  ~~ m/^<?InTelugu>$/, q{Don't match unrelated <?InTelugu>} );
ok("\x[8AC5]"  ~~ m/^<!InTelugu>.$/, q{Match unrelated negated <?InTelugu>} );
ok("\x[8AC5]"  ~~ m/^<-InTelugu>$/, q{Match unrelated inverted <?InTelugu>} );
ok("\x[8AC5]\x[0C00]" ~~ m/<?InTelugu>/, q{Match unanchored <?InTelugu>} );

# InThaana


ok("\c[THAANA LETTER HAA]" ~~ m/^<?InThaana>$/, q{Match <?InThaana>} );
ok(not "\c[THAANA LETTER HAA]" ~~ m/^<!InThaana>.$/, q{Don't match negated <?InThaana>} );
ok(not "\c[THAANA LETTER HAA]" ~~ m/^<-InThaana>$/, q{Don't match inverted <?InThaana>} );
ok(not "\x[BB8F]"  ~~ m/^<?InThaana>$/, q{Don't match unrelated <?InThaana>} );
ok("\x[BB8F]"  ~~ m/^<!InThaana>.$/, q{Match unrelated negated <?InThaana>} );
ok("\x[BB8F]"  ~~ m/^<-InThaana>$/, q{Match unrelated inverted <?InThaana>} );
ok("\x[BB8F]\c[THAANA LETTER HAA]" ~~ m/<?InThaana>/, q{Match unanchored <?InThaana>} );

# InThai


ok("\x[0E00]" ~~ m/^<?InThai>$/, q{Match <?InThai>} );
ok(not "\x[0E00]" ~~ m/^<!InThai>.$/, q{Don't match negated <?InThai>} );
ok(not "\x[0E00]" ~~ m/^<-InThai>$/, q{Don't match inverted <?InThai>} );
ok(not "\x[9395]"  ~~ m/^<?InThai>$/, q{Don't match unrelated <?InThai>} );
ok("\x[9395]"  ~~ m/^<!InThai>.$/, q{Match unrelated negated <?InThai>} );
ok("\x[9395]"  ~~ m/^<-InThai>$/, q{Match unrelated inverted <?InThai>} );
ok("\x[9395]\x[0E00]" ~~ m/<?InThai>/, q{Match unanchored <?InThai>} );

# InTibetan


ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<?InTibetan>$/, q{Match <?InTibetan>} );
ok(not "\c[TIBETAN SYLLABLE OM]" ~~ m/^<!InTibetan>.$/, q{Don't match negated <?InTibetan>} );
ok(not "\c[TIBETAN SYLLABLE OM]" ~~ m/^<-InTibetan>$/, q{Don't match inverted <?InTibetan>} );
ok(not "\x[957A]"  ~~ m/^<?InTibetan>$/, q{Don't match unrelated <?InTibetan>} );
ok("\x[957A]"  ~~ m/^<!InTibetan>.$/, q{Match unrelated negated <?InTibetan>} );
ok("\x[957A]"  ~~ m/^<-InTibetan>$/, q{Match unrelated inverted <?InTibetan>} );
ok("\x[957A]\c[TIBETAN SYLLABLE OM]" ~~ m/<?InTibetan>/, q{Match unanchored <?InTibetan>} );

# InUnifiedCanadianAboriginalSyllabics


ok("\x[1400]" ~~ m/^<?InUnifiedCanadianAboriginalSyllabics>$/, q{Match <?InUnifiedCanadianAboriginalSyllabics>} );
ok(not "\x[1400]" ~~ m/^<!InUnifiedCanadianAboriginalSyllabics>.$/, q{Don't match negated <?InUnifiedCanadianAboriginalSyllabics>} );
ok(not "\x[1400]" ~~ m/^<-InUnifiedCanadianAboriginalSyllabics>$/, q{Don't match inverted <?InUnifiedCanadianAboriginalSyllabics>} );
ok(not "\x[9470]"  ~~ m/^<?InUnifiedCanadianAboriginalSyllabics>$/, q{Don't match unrelated <?InUnifiedCanadianAboriginalSyllabics>} );
ok("\x[9470]"  ~~ m/^<!InUnifiedCanadianAboriginalSyllabics>.$/, q{Match unrelated negated <?InUnifiedCanadianAboriginalSyllabics>} );
ok("\x[9470]"  ~~ m/^<-InUnifiedCanadianAboriginalSyllabics>$/, q{Match unrelated inverted <?InUnifiedCanadianAboriginalSyllabics>} );
ok("\x[9470]\x[1400]" ~~ m/<?InUnifiedCanadianAboriginalSyllabics>/, q{Match unanchored <?InUnifiedCanadianAboriginalSyllabics>} );

# InVariationSelectors


ok(not "\x[764D]"  ~~ m/^<?InVariationSelectors>$/, q{Don't match unrelated <?InVariationSelectors>} );
ok("\x[764D]"  ~~ m/^<!InVariationSelectors>.$/, q{Match unrelated negated <?InVariationSelectors>} );
ok("\x[764D]"  ~~ m/^<-InVariationSelectors>$/, q{Match unrelated inverted <?InVariationSelectors>} );

# InYiRadicals


ok("\c[YI RADICAL QOT]" ~~ m/^<?InYiRadicals>$/, q{Match <?InYiRadicals>} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<!InYiRadicals>.$/, q{Don't match negated <?InYiRadicals>} );
ok(not "\c[YI RADICAL QOT]" ~~ m/^<-InYiRadicals>$/, q{Don't match inverted <?InYiRadicals>} );
ok(not "\x[3A4E]"  ~~ m/^<?InYiRadicals>$/, q{Don't match unrelated <?InYiRadicals>} );
ok("\x[3A4E]"  ~~ m/^<!InYiRadicals>.$/, q{Match unrelated negated <?InYiRadicals>} );
ok("\x[3A4E]"  ~~ m/^<-InYiRadicals>$/, q{Match unrelated inverted <?InYiRadicals>} );
ok("\x[3A4E]\c[YI RADICAL QOT]" ~~ m/<?InYiRadicals>/, q{Match unanchored <?InYiRadicals>} );

# InYiSyllables


ok("\c[YI SYLLABLE IT]" ~~ m/^<?InYiSyllables>$/, q{Match <?InYiSyllables>} );
ok(not "\c[YI SYLLABLE IT]" ~~ m/^<!InYiSyllables>.$/, q{Don't match negated <?InYiSyllables>} );
ok(not "\c[YI SYLLABLE IT]" ~~ m/^<-InYiSyllables>$/, q{Don't match inverted <?InYiSyllables>} );
ok(not "\c[PARALLEL WITH HORIZONTAL STROKE]"  ~~ m/^<?InYiSyllables>$/, q{Don't match unrelated <?InYiSyllables>} );
ok("\c[PARALLEL WITH HORIZONTAL STROKE]"  ~~ m/^<!InYiSyllables>.$/, q{Match unrelated negated <?InYiSyllables>} );
ok("\c[PARALLEL WITH HORIZONTAL STROKE]"  ~~ m/^<-InYiSyllables>$/, q{Match unrelated inverted <?InYiSyllables>} );
ok("\c[PARALLEL WITH HORIZONTAL STROKE]\c[YI SYLLABLE IT]" ~~ m/<?InYiSyllables>/, q{Match unanchored <?InYiSyllables>} );
