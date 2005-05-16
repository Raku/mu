#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/propcharset_slow_to_compile.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 2349;

# L           Letter


ok("\x[45CD]" ~~ m/^<+<L>>$/, q{Match <L> (Letter)} );
ok("\x[45CD]" ~~ m/^<[A]+<L>>$/, q{Match compound <L> (Letter)} );
ok(!( "\x[45CD]" ~~ m/^<-<L>>$/ ), q{Don't match externally inverted <L> (Letter)} );
ok(!( "\x[45CD]" ~~ m/^<[A]-<L>>$/ ), q{Don't match compound inverted <L> (Letter)} );
ok(!( "\x[45CD]" ~~ m/^<+<-L>>$/ ), q{Don't match internally inverted <L> (Letter)} );
ok(!( "\x[4DB6]"  ~~ m/^<+<L>>$/ ), q{Don't match unrelated <L> (Letter)} );
ok("\x[4DB6]"  ~~ m/^<-<L>>$/, q{Match unrelated externally inverted <L> (Letter)} );
ok("\x[4DB6]"  ~~ m/^<+<-L>>$/, q{Match unrelated internally inverted <L> (Letter)} );
ok("\x[4DB6]\x[45CD]" ~~ m/<+<L>>/, q{Match unanchored <L> (Letter)} );

ok("\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<+<?Letter>>$/, q{Match <?Letter>} );
ok("\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<[A]+<?Letter>>$/, q{Match compound <?Letter>} );
ok(!( "\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<-<?Letter>>$/ ), q{Don't match externally inverted <?Letter>} );
ok(!( "\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<[A]-<?Letter>>$/ ), q{Don't match compound inverted <?Letter>} );
ok(!( "\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/^<+<-Letter>>$/ ), q{Don't match internally inverted <?Letter>} );
ok(!( "\x[318F]"  ~~ m/^<+<?Letter>>$/ ), q{Don't match unrelated <?Letter>} );
ok("\x[318F]"  ~~ m/^<-<?Letter>>$/, q{Match unrelated externally inverted <?Letter>} );
ok("\x[318F]"  ~~ m/^<+<-Letter>>$/, q{Match unrelated internally inverted <?Letter>} );
ok("\x[318F]\c[HANGUL LETTER SIOS-PIEUP]" ~~ m/<+<?Letter>>/, q{Match unanchored <?Letter>} );

# Lu          UppercaseLetter


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?Lu>>$/, q{Match <?Lu> (UppercaseLetter)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?Lu>>$/, q{Match compound <?Lu> (UppercaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?Lu>>$/ ), q{Don't match externally inverted <?Lu> (UppercaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?Lu>>$/ ), q{Don't match compound inverted <?Lu> (UppercaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-Lu>>$/ ), q{Don't match internally inverted <?Lu> (UppercaseLetter)} );
ok(!( "\x[5E52]"  ~~ m/^<+<?Lu>>$/ ), q{Don't match unrelated <?Lu> (UppercaseLetter)} );
ok("\x[5E52]"  ~~ m/^<-<?Lu>>$/, q{Match unrelated externally inverted <?Lu> (UppercaseLetter)} );
ok("\x[5E52]"  ~~ m/^<+<-Lu>>$/, q{Match unrelated internally inverted <?Lu> (UppercaseLetter)} );
ok(!( "\x[5E52]" ~~ m/^<+<?Lu>>$/ ), q{Don't match related <?Lu> (UppercaseLetter)} );
ok("\x[5E52]" ~~ m/^<+<-Lu>>$/, q{Match related internally inverted <?Lu> (UppercaseLetter)} );
ok("\x[5E52]" ~~ m/^<-<?Lu>>$/, q{Match related externally inverted <?Lu> (UppercaseLetter)} );
ok("\x[5E52]\x[5E52]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?Lu>>/, q{Match unanchored <?Lu> (UppercaseLetter)} );

ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<?UppercaseLetter>>$/, q{Match <?UppercaseLetter>} );
ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]+<?UppercaseLetter>>$/, q{Match compound <?UppercaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<-<?UppercaseLetter>>$/ ), q{Don't match externally inverted <?UppercaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]-<?UppercaseLetter>>$/ ), q{Don't match compound inverted <?UppercaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<-UppercaseLetter>>$/ ), q{Don't match internally inverted <?UppercaseLetter>} );
ok(!( "\x[1DB9]"  ~~ m/^<+<?UppercaseLetter>>$/ ), q{Don't match unrelated <?UppercaseLetter>} );
ok("\x[1DB9]"  ~~ m/^<-<?UppercaseLetter>>$/, q{Match unrelated externally inverted <?UppercaseLetter>} );
ok("\x[1DB9]"  ~~ m/^<+<-UppercaseLetter>>$/, q{Match unrelated internally inverted <?UppercaseLetter>} );
ok("\x[1DB9]\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/<+<?UppercaseLetter>>/, q{Match unanchored <?UppercaseLetter>} );

# Ll          LowercaseLetter


ok("\c[LATIN SMALL LETTER A]" ~~ m/^<+<?Ll>>$/, q{Match <?Ll> (LowercaseLetter)} );
ok("\c[LATIN SMALL LETTER A]" ~~ m/^<[A]+<?Ll>>$/, q{Match compound <?Ll> (LowercaseLetter)} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<-<?Ll>>$/ ), q{Don't match externally inverted <?Ll> (LowercaseLetter)} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<[A]-<?Ll>>$/ ), q{Don't match compound inverted <?Ll> (LowercaseLetter)} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<+<-Ll>>$/ ), q{Don't match internally inverted <?Ll> (LowercaseLetter)} );
ok(!( "\x[83AD]"  ~~ m/^<+<?Ll>>$/ ), q{Don't match unrelated <?Ll> (LowercaseLetter)} );
ok("\x[83AD]"  ~~ m/^<-<?Ll>>$/, q{Match unrelated externally inverted <?Ll> (LowercaseLetter)} );
ok("\x[83AD]"  ~~ m/^<+<-Ll>>$/, q{Match unrelated internally inverted <?Ll> (LowercaseLetter)} );
ok(!( "\x[83AD]" ~~ m/^<+<?Ll>>$/ ), q{Don't match related <?Ll> (LowercaseLetter)} );
ok("\x[83AD]" ~~ m/^<+<-Ll>>$/, q{Match related internally inverted <?Ll> (LowercaseLetter)} );
ok("\x[83AD]" ~~ m/^<-<?Ll>>$/, q{Match related externally inverted <?Ll> (LowercaseLetter)} );
ok("\x[83AD]\x[83AD]\c[LATIN SMALL LETTER A]" ~~ m/<+<?Ll>>/, q{Match unanchored <?Ll> (LowercaseLetter)} );

ok("\c[LATIN SMALL LETTER A]" ~~ m/^<+<?LowercaseLetter>>$/, q{Match <?LowercaseLetter>} );
ok("\c[LATIN SMALL LETTER A]" ~~ m/^<[A]+<?LowercaseLetter>>$/, q{Match compound <?LowercaseLetter>} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<-<?LowercaseLetter>>$/ ), q{Don't match externally inverted <?LowercaseLetter>} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<[A]-<?LowercaseLetter>>$/ ), q{Don't match compound inverted <?LowercaseLetter>} );
ok(!( "\c[LATIN SMALL LETTER A]" ~~ m/^<+<-LowercaseLetter>>$/ ), q{Don't match internally inverted <?LowercaseLetter>} );
ok(!( "\x[A9A8]"  ~~ m/^<+<?LowercaseLetter>>$/ ), q{Don't match unrelated <?LowercaseLetter>} );
ok("\x[A9A8]"  ~~ m/^<-<?LowercaseLetter>>$/, q{Match unrelated externally inverted <?LowercaseLetter>} );
ok("\x[A9A8]"  ~~ m/^<+<-LowercaseLetter>>$/, q{Match unrelated internally inverted <?LowercaseLetter>} );
ok(!( "\x[AC00]" ~~ m/^<+<?LowercaseLetter>>$/ ), q{Don't match related <?LowercaseLetter>} );
ok("\x[AC00]" ~~ m/^<+<-LowercaseLetter>>$/, q{Match related internally inverted <?LowercaseLetter>} );
ok("\x[AC00]" ~~ m/^<-<?LowercaseLetter>>$/, q{Match related externally inverted <?LowercaseLetter>} );
ok("\x[A9A8]\x[AC00]\c[LATIN SMALL LETTER A]" ~~ m/<+<?LowercaseLetter>>/, q{Match unanchored <?LowercaseLetter>} );

# Lt          TitlecaseLetter


ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<?Lt>>$/, q{Match <?Lt> (TitlecaseLetter)} );
ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]+<?Lt>>$/, q{Match compound <?Lt> (TitlecaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<-<?Lt>>$/ ), q{Don't match externally inverted <?Lt> (TitlecaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]-<?Lt>>$/ ), q{Don't match compound inverted <?Lt> (TitlecaseLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<-Lt>>$/ ), q{Don't match internally inverted <?Lt> (TitlecaseLetter)} );
ok(!( "\x[D187]"  ~~ m/^<+<?Lt>>$/ ), q{Don't match unrelated <?Lt> (TitlecaseLetter)} );
ok("\x[D187]"  ~~ m/^<-<?Lt>>$/, q{Match unrelated externally inverted <?Lt> (TitlecaseLetter)} );
ok("\x[D187]"  ~~ m/^<+<-Lt>>$/, q{Match unrelated internally inverted <?Lt> (TitlecaseLetter)} );
ok(!( "\x[D187]" ~~ m/^<+<?Lt>>$/ ), q{Don't match related <?Lt> (TitlecaseLetter)} );
ok("\x[D187]" ~~ m/^<+<-Lt>>$/, q{Match related internally inverted <?Lt> (TitlecaseLetter)} );
ok("\x[D187]" ~~ m/^<-<?Lt>>$/, q{Match related externally inverted <?Lt> (TitlecaseLetter)} );
ok("\x[D187]\x[D187]\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/<+<?Lt>>/, q{Match unanchored <?Lt> (TitlecaseLetter)} );

ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<?TitlecaseLetter>>$/, q{Match <?TitlecaseLetter>} );
ok("\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]+<?TitlecaseLetter>>$/, q{Match compound <?TitlecaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<-<?TitlecaseLetter>>$/ ), q{Don't match externally inverted <?TitlecaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<[A]-<?TitlecaseLetter>>$/ ), q{Don't match compound inverted <?TitlecaseLetter>} );
ok(!( "\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/^<+<-TitlecaseLetter>>$/ ), q{Don't match internally inverted <?TitlecaseLetter>} );
ok(!( "\x[C2A9]"  ~~ m/^<+<?TitlecaseLetter>>$/ ), q{Don't match unrelated <?TitlecaseLetter>} );
ok("\x[C2A9]"  ~~ m/^<-<?TitlecaseLetter>>$/, q{Match unrelated externally inverted <?TitlecaseLetter>} );
ok("\x[C2A9]"  ~~ m/^<+<-TitlecaseLetter>>$/, q{Match unrelated internally inverted <?TitlecaseLetter>} );
ok("\x[C2A9]\c[LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON]" ~~ m/<+<?TitlecaseLetter>>/, q{Match unanchored <?TitlecaseLetter>} );

# Lm          ModifierLetter


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?Lm>>$/, q{Match <?Lm> (ModifierLetter)} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?Lm>>$/, q{Match compound <?Lm> (ModifierLetter)} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?Lm>>$/ ), q{Don't match externally inverted <?Lm> (ModifierLetter)} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?Lm>>$/ ), q{Don't match compound inverted <?Lm> (ModifierLetter)} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-Lm>>$/ ), q{Don't match internally inverted <?Lm> (ModifierLetter)} );
ok(!( "\x[8C34]"  ~~ m/^<+<?Lm>>$/ ), q{Don't match unrelated <?Lm> (ModifierLetter)} );
ok("\x[8C34]"  ~~ m/^<-<?Lm>>$/, q{Match unrelated externally inverted <?Lm> (ModifierLetter)} );
ok("\x[8C34]"  ~~ m/^<+<-Lm>>$/, q{Match unrelated internally inverted <?Lm> (ModifierLetter)} );
ok(!( "\x[8C34]" ~~ m/^<+<?Lm>>$/ ), q{Don't match related <?Lm> (ModifierLetter)} );
ok("\x[8C34]" ~~ m/^<+<-Lm>>$/, q{Match related internally inverted <?Lm> (ModifierLetter)} );
ok("\x[8C34]" ~~ m/^<-<?Lm>>$/, q{Match related externally inverted <?Lm> (ModifierLetter)} );
ok("\x[8C34]\x[8C34]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?Lm>>/, q{Match unanchored <?Lm> (ModifierLetter)} );

ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?ModifierLetter>>$/, q{Match <?ModifierLetter>} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?ModifierLetter>>$/, q{Match compound <?ModifierLetter>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?ModifierLetter>>$/ ), q{Don't match externally inverted <?ModifierLetter>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?ModifierLetter>>$/ ), q{Don't match compound inverted <?ModifierLetter>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-ModifierLetter>>$/ ), q{Don't match internally inverted <?ModifierLetter>} );
ok(!( "\c[YI SYLLABLE NZAX]"  ~~ m/^<+<?ModifierLetter>>$/ ), q{Don't match unrelated <?ModifierLetter>} );
ok("\c[YI SYLLABLE NZAX]"  ~~ m/^<-<?ModifierLetter>>$/, q{Match unrelated externally inverted <?ModifierLetter>} );
ok("\c[YI SYLLABLE NZAX]"  ~~ m/^<+<-ModifierLetter>>$/, q{Match unrelated internally inverted <?ModifierLetter>} );
ok("\c[YI SYLLABLE NZAX]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?ModifierLetter>>/, q{Match unanchored <?ModifierLetter>} );

# Lo          OtherLetter


ok("\x[8CC9]" ~~ m/^<+<?Lo>>$/, q{Match <?Lo> (OtherLetter)} );
ok("\x[8CC9]" ~~ m/^<[A]+<?Lo>>$/, q{Match compound <?Lo> (OtherLetter)} );
ok(!( "\x[8CC9]" ~~ m/^<-<?Lo>>$/ ), q{Don't match externally inverted <?Lo> (OtherLetter)} );
ok(!( "\x[8CC9]" ~~ m/^<[A]-<?Lo>>$/ ), q{Don't match compound inverted <?Lo> (OtherLetter)} );
ok(!( "\x[8CC9]" ~~ m/^<+<-Lo>>$/ ), q{Don't match internally inverted <?Lo> (OtherLetter)} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?Lo>>$/ ), q{Don't match unrelated <?Lo> (OtherLetter)} );
ok("\x[9FA6]"  ~~ m/^<-<?Lo>>$/, q{Match unrelated externally inverted <?Lo> (OtherLetter)} );
ok("\x[9FA6]"  ~~ m/^<+<-Lo>>$/, q{Match unrelated internally inverted <?Lo> (OtherLetter)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?Lo>>$/ ), q{Don't match related <?Lo> (OtherLetter)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-Lo>>$/, q{Match related internally inverted <?Lo> (OtherLetter)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?Lo>>$/, q{Match related externally inverted <?Lo> (OtherLetter)} );
ok("\x[9FA6]\c[LATIN CAPITAL LETTER A]\x[8CC9]" ~~ m/<+<?Lo>>/, q{Match unanchored <?Lo> (OtherLetter)} );

ok("\x[BC7D]" ~~ m/^<+<?OtherLetter>>$/, q{Match <?OtherLetter>} );
ok("\x[BC7D]" ~~ m/^<[A]+<?OtherLetter>>$/, q{Match compound <?OtherLetter>} );
ok(!( "\x[BC7D]" ~~ m/^<-<?OtherLetter>>$/ ), q{Don't match externally inverted <?OtherLetter>} );
ok(!( "\x[BC7D]" ~~ m/^<[A]-<?OtherLetter>>$/ ), q{Don't match compound inverted <?OtherLetter>} );
ok(!( "\x[BC7D]" ~~ m/^<+<-OtherLetter>>$/ ), q{Don't match internally inverted <?OtherLetter>} );
ok(!( "\x[D7A4]"  ~~ m/^<+<?OtherLetter>>$/ ), q{Don't match unrelated <?OtherLetter>} );
ok("\x[D7A4]"  ~~ m/^<-<?OtherLetter>>$/, q{Match unrelated externally inverted <?OtherLetter>} );
ok("\x[D7A4]"  ~~ m/^<+<-OtherLetter>>$/, q{Match unrelated internally inverted <?OtherLetter>} );
ok("\x[D7A4]\x[BC7D]" ~~ m/<+<?OtherLetter>>/, q{Match unanchored <?OtherLetter>} );

# Lr		 	# Alias for "Ll", "Lu", and "Lt".


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?Lr>>$/, q{Match (Alias for "Ll", "Lu", and "Lt".)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?Lr>>$/, q{Match compound (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?Lr>>$/ ), q{Don't match externally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?Lr>>$/ ), q{Don't match compound inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-Lr>>$/ ), q{Don't match internally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\x[CD29]"  ~~ m/^<+<?Lr>>$/ ), q{Don't match unrelated (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]"  ~~ m/^<-<?Lr>>$/, q{Match unrelated externally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]"  ~~ m/^<+<-Lr>>$/, q{Match unrelated internally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok(!( "\x[CD29]" ~~ m/^<+<?Lr>>$/ ), q{Don't match related (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]" ~~ m/^<+<-Lr>>$/, q{Match related internally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]" ~~ m/^<-<?Lr>>$/, q{Match related externally inverted (Alias for "Ll", "Lu", and "Lt".)} );
ok("\x[CD29]\x[CD29]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?Lr>>/, q{Match unanchored (Alias for "Ll", "Lu", and "Lt".)} );


# M           Mark


ok("\c[TAGALOG VOWEL SIGN I]" ~~ m/^<+<M>>$/, q{Match <M> (Mark)} );
ok("\c[TAGALOG VOWEL SIGN I]" ~~ m/^<[A]+<M>>$/, q{Match compound <M> (Mark)} );
ok(!( "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<-<M>>$/ ), q{Don't match externally inverted <M> (Mark)} );
ok(!( "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<[A]-<M>>$/ ), q{Don't match compound inverted <M> (Mark)} );
ok(!( "\c[TAGALOG VOWEL SIGN I]" ~~ m/^<+<-M>>$/ ), q{Don't match internally inverted <M> (Mark)} );
ok(!( "\c[CANADIAN SYLLABICS KAAI]"  ~~ m/^<+<M>>$/ ), q{Don't match unrelated <M> (Mark)} );
ok("\c[CANADIAN SYLLABICS KAAI]"  ~~ m/^<-<M>>$/, q{Match unrelated externally inverted <M> (Mark)} );
ok("\c[CANADIAN SYLLABICS KAAI]"  ~~ m/^<+<-M>>$/, q{Match unrelated internally inverted <M> (Mark)} );
ok("\c[CANADIAN SYLLABICS KAAI]\c[TAGALOG VOWEL SIGN I]" ~~ m/<+<M>>/, q{Match unanchored <M> (Mark)} );

ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?Mark>>$/, q{Match <?Mark>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?Mark>>$/, q{Match compound <?Mark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?Mark>>$/ ), q{Don't match externally inverted <?Mark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?Mark>>$/ ), q{Don't match compound inverted <?Mark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-Mark>>$/ ), q{Don't match internally inverted <?Mark>} );
ok(!( "\x[4BF0]"  ~~ m/^<+<?Mark>>$/ ), q{Don't match unrelated <?Mark>} );
ok("\x[4BF0]"  ~~ m/^<-<?Mark>>$/, q{Match unrelated externally inverted <?Mark>} );
ok("\x[4BF0]"  ~~ m/^<+<-Mark>>$/, q{Match unrelated internally inverted <?Mark>} );
ok("\x[4BF0]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?Mark>>/, q{Match unanchored <?Mark>} );

# Mn          NonspacingMark


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?Mn>>$/, q{Match <?Mn> (NonspacingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?Mn>>$/, q{Match compound <?Mn> (NonspacingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?Mn>>$/ ), q{Don't match externally inverted <?Mn> (NonspacingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?Mn>>$/ ), q{Don't match compound inverted <?Mn> (NonspacingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-Mn>>$/ ), q{Don't match internally inverted <?Mn> (NonspacingMark)} );
ok(!( "\x[CF2C]"  ~~ m/^<+<?Mn>>$/ ), q{Don't match unrelated <?Mn> (NonspacingMark)} );
ok("\x[CF2C]"  ~~ m/^<-<?Mn>>$/, q{Match unrelated externally inverted <?Mn> (NonspacingMark)} );
ok("\x[CF2C]"  ~~ m/^<+<-Mn>>$/, q{Match unrelated internally inverted <?Mn> (NonspacingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<?Mn>>$/ ), q{Don't match related <?Mn> (NonspacingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<-Mn>>$/, q{Match related internally inverted <?Mn> (NonspacingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-<?Mn>>$/, q{Match related externally inverted <?Mn> (NonspacingMark)} );
ok("\x[CF2C]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?Mn>>/, q{Match unanchored <?Mn> (NonspacingMark)} );

ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?NonspacingMark>>$/, q{Match <?NonspacingMark>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?NonspacingMark>>$/, q{Match compound <?NonspacingMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?NonspacingMark>>$/ ), q{Don't match externally inverted <?NonspacingMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?NonspacingMark>>$/ ), q{Don't match compound inverted <?NonspacingMark>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-NonspacingMark>>$/ ), q{Don't match internally inverted <?NonspacingMark>} );
ok(!( "\x[B617]"  ~~ m/^<+<?NonspacingMark>>$/ ), q{Don't match unrelated <?NonspacingMark>} );
ok("\x[B617]"  ~~ m/^<-<?NonspacingMark>>$/, q{Match unrelated externally inverted <?NonspacingMark>} );
ok("\x[B617]"  ~~ m/^<+<-NonspacingMark>>$/, q{Match unrelated internally inverted <?NonspacingMark>} );
ok("\x[B617]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?NonspacingMark>>/, q{Match unanchored <?NonspacingMark>} );

# Mc          SpacingMark


ok("\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<+<?Mc>>$/, q{Match <?Mc> (SpacingMark)} );
ok("\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<[A]+<?Mc>>$/, q{Match compound <?Mc> (SpacingMark)} );
ok(!( "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<-<?Mc>>$/ ), q{Don't match externally inverted <?Mc> (SpacingMark)} );
ok(!( "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<[A]-<?Mc>>$/ ), q{Don't match compound inverted <?Mc> (SpacingMark)} );
ok(!( "\c[DEVANAGARI SIGN VISARGA]" ~~ m/^<+<-Mc>>$/ ), q{Don't match internally inverted <?Mc> (SpacingMark)} );
ok(!( "\c[BALLOT BOX WITH CHECK]"  ~~ m/^<+<?Mc>>$/ ), q{Don't match unrelated <?Mc> (SpacingMark)} );
ok("\c[BALLOT BOX WITH CHECK]"  ~~ m/^<-<?Mc>>$/, q{Match unrelated externally inverted <?Mc> (SpacingMark)} );
ok("\c[BALLOT BOX WITH CHECK]"  ~~ m/^<+<-Mc>>$/, q{Match unrelated internally inverted <?Mc> (SpacingMark)} );
ok(!( "\c[IDEOGRAPHIC LEVEL TONE MARK]" ~~ m/^<+<?Mc>>$/ ), q{Don't match related <?Mc> (SpacingMark)} );
ok("\c[IDEOGRAPHIC LEVEL TONE MARK]" ~~ m/^<+<-Mc>>$/, q{Match related internally inverted <?Mc> (SpacingMark)} );
ok("\c[IDEOGRAPHIC LEVEL TONE MARK]" ~~ m/^<-<?Mc>>$/, q{Match related externally inverted <?Mc> (SpacingMark)} );
ok("\c[BALLOT BOX WITH CHECK]\c[IDEOGRAPHIC LEVEL TONE MARK]\c[DEVANAGARI SIGN VISARGA]" ~~ m/<+<?Mc>>/, q{Match unanchored <?Mc> (SpacingMark)} );

ok("\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<+<?SpacingMark>>$/, q{Match <?SpacingMark>} );
ok("\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<[A]+<?SpacingMark>>$/, q{Match compound <?SpacingMark>} );
ok(!( "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<-<?SpacingMark>>$/ ), q{Don't match externally inverted <?SpacingMark>} );
ok(!( "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<[A]-<?SpacingMark>>$/ ), q{Don't match compound inverted <?SpacingMark>} );
ok(!( "\c[MALAYALAM SIGN ANUSVARA]" ~~ m/^<+<-SpacingMark>>$/ ), q{Don't match internally inverted <?SpacingMark>} );
ok(!( "\c[KANNADA LETTER VOCALIC LL]"  ~~ m/^<+<?SpacingMark>>$/ ), q{Don't match unrelated <?SpacingMark>} );
ok("\c[KANNADA LETTER VOCALIC LL]"  ~~ m/^<-<?SpacingMark>>$/, q{Match unrelated externally inverted <?SpacingMark>} );
ok("\c[KANNADA LETTER VOCALIC LL]"  ~~ m/^<+<-SpacingMark>>$/, q{Match unrelated internally inverted <?SpacingMark>} );
ok("\c[KANNADA LETTER VOCALIC LL]\c[MALAYALAM SIGN ANUSVARA]" ~~ m/<+<?SpacingMark>>/, q{Match unanchored <?SpacingMark>} );

# Me          EnclosingMark


ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<?Me>>$/, q{Match <?Me> (EnclosingMark)} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]+<?Me>>$/, q{Match compound <?Me> (EnclosingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-<?Me>>$/ ), q{Don't match externally inverted <?Me> (EnclosingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]-<?Me>>$/ ), q{Don't match compound inverted <?Me> (EnclosingMark)} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<-Me>>$/ ), q{Don't match internally inverted <?Me> (EnclosingMark)} );
ok(!( "\x[C680]"  ~~ m/^<+<?Me>>$/ ), q{Don't match unrelated <?Me> (EnclosingMark)} );
ok("\x[C680]"  ~~ m/^<-<?Me>>$/, q{Match unrelated externally inverted <?Me> (EnclosingMark)} );
ok("\x[C680]"  ~~ m/^<+<-Me>>$/, q{Match unrelated internally inverted <?Me> (EnclosingMark)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?Me>>$/ ), q{Don't match related <?Me> (EnclosingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-Me>>$/, q{Match related internally inverted <?Me> (EnclosingMark)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?Me>>$/, q{Match related externally inverted <?Me> (EnclosingMark)} );
ok("\x[C680]\c[COMBINING GRAVE ACCENT]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/<+<?Me>>/, q{Match unanchored <?Me> (EnclosingMark)} );

ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<?EnclosingMark>>$/, q{Match <?EnclosingMark>} );
ok("\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]+<?EnclosingMark>>$/, q{Match compound <?EnclosingMark>} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<-<?EnclosingMark>>$/ ), q{Don't match externally inverted <?EnclosingMark>} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<[A]-<?EnclosingMark>>$/ ), q{Don't match compound inverted <?EnclosingMark>} );
ok(!( "\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/^<+<-EnclosingMark>>$/ ), q{Don't match internally inverted <?EnclosingMark>} );
ok(!( "\x[911E]"  ~~ m/^<+<?EnclosingMark>>$/ ), q{Don't match unrelated <?EnclosingMark>} );
ok("\x[911E]"  ~~ m/^<-<?EnclosingMark>>$/, q{Match unrelated externally inverted <?EnclosingMark>} );
ok("\x[911E]"  ~~ m/^<+<-EnclosingMark>>$/, q{Match unrelated internally inverted <?EnclosingMark>} );
ok("\x[911E]\c[COMBINING CYRILLIC HUNDRED THOUSANDS SIGN]" ~~ m/<+<?EnclosingMark>>/, q{Match unanchored <?EnclosingMark>} );

# N           Number


ok("\c[DIGIT ZERO]" ~~ m/^<+<N>>$/, q{Match <N> (Number)} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<N>>$/, q{Match compound <N> (Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<N>>$/ ), q{Don't match externally inverted <N> (Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<N>>$/ ), q{Don't match compound inverted <N> (Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-N>>$/ ), q{Don't match internally inverted <N> (Number)} );
ok(!( "\x[3BA3]"  ~~ m/^<+<N>>$/ ), q{Don't match unrelated <N> (Number)} );
ok("\x[3BA3]"  ~~ m/^<-<N>>$/, q{Match unrelated externally inverted <N> (Number)} );
ok("\x[3BA3]"  ~~ m/^<+<-N>>$/, q{Match unrelated internally inverted <N> (Number)} );
ok("\x[3BA3]\c[DIGIT ZERO]" ~~ m/<+<N>>/, q{Match unanchored <N> (Number)} );

ok("\c[DIGIT ZERO]" ~~ m/^<+<?Number>>$/, q{Match <?Number>} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?Number>>$/, q{Match compound <?Number>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?Number>>$/ ), q{Don't match externally inverted <?Number>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?Number>>$/ ), q{Don't match compound inverted <?Number>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-Number>>$/ ), q{Don't match internally inverted <?Number>} );
ok(!( "\x[37D0]"  ~~ m/^<+<?Number>>$/ ), q{Don't match unrelated <?Number>} );
ok("\x[37D0]"  ~~ m/^<-<?Number>>$/, q{Match unrelated externally inverted <?Number>} );
ok("\x[37D0]"  ~~ m/^<+<-Number>>$/, q{Match unrelated internally inverted <?Number>} );
ok("\x[37D0]\c[DIGIT ZERO]" ~~ m/<+<?Number>>/, q{Match unanchored <?Number>} );

# Nd          DecimalNumber


ok("\c[DIGIT ZERO]" ~~ m/^<+<?Nd>>$/, q{Match <?Nd> (DecimalNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?Nd>>$/, q{Match compound <?Nd> (DecimalNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?Nd>>$/ ), q{Don't match externally inverted <?Nd> (DecimalNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?Nd>>$/ ), q{Don't match compound inverted <?Nd> (DecimalNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-Nd>>$/ ), q{Don't match internally inverted <?Nd> (DecimalNumber)} );
ok(!( "\x[8536]"  ~~ m/^<+<?Nd>>$/ ), q{Don't match unrelated <?Nd> (DecimalNumber)} );
ok("\x[8536]"  ~~ m/^<-<?Nd>>$/, q{Match unrelated externally inverted <?Nd> (DecimalNumber)} );
ok("\x[8536]"  ~~ m/^<+<-Nd>>$/, q{Match unrelated internally inverted <?Nd> (DecimalNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<+<?Nd>>$/ ), q{Don't match related <?Nd> (DecimalNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<+<-Nd>>$/, q{Match related internally inverted <?Nd> (DecimalNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<-<?Nd>>$/, q{Match related externally inverted <?Nd> (DecimalNumber)} );
ok("\x[8536]\c[SUPERSCRIPT TWO]\c[DIGIT ZERO]" ~~ m/<+<?Nd>>/, q{Match unanchored <?Nd> (DecimalNumber)} );

ok("\c[KHMER DIGIT ZERO]" ~~ m/^<+<?DecimalNumber>>$/, q{Match <?DecimalNumber>} );
ok("\c[KHMER DIGIT ZERO]" ~~ m/^<[A]+<?DecimalNumber>>$/, q{Match compound <?DecimalNumber>} );
ok(!( "\c[KHMER DIGIT ZERO]" ~~ m/^<-<?DecimalNumber>>$/ ), q{Don't match externally inverted <?DecimalNumber>} );
ok(!( "\c[KHMER DIGIT ZERO]" ~~ m/^<[A]-<?DecimalNumber>>$/ ), q{Don't match compound inverted <?DecimalNumber>} );
ok(!( "\c[KHMER DIGIT ZERO]" ~~ m/^<+<-DecimalNumber>>$/ ), q{Don't match internally inverted <?DecimalNumber>} );
ok(!( "\c[CANADIAN SYLLABICS NWE]"  ~~ m/^<+<?DecimalNumber>>$/ ), q{Don't match unrelated <?DecimalNumber>} );
ok("\c[CANADIAN SYLLABICS NWE]"  ~~ m/^<-<?DecimalNumber>>$/, q{Match unrelated externally inverted <?DecimalNumber>} );
ok("\c[CANADIAN SYLLABICS NWE]"  ~~ m/^<+<-DecimalNumber>>$/, q{Match unrelated internally inverted <?DecimalNumber>} );
ok("\c[CANADIAN SYLLABICS NWE]\c[KHMER DIGIT ZERO]" ~~ m/<+<?DecimalNumber>>/, q{Match unanchored <?DecimalNumber>} );

# Nl          LetterNumber


ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<+<?Nl>>$/, q{Match <?Nl> (LetterNumber)} );
ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]+<?Nl>>$/, q{Match compound <?Nl> (LetterNumber)} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<-<?Nl>>$/ ), q{Don't match externally inverted <?Nl> (LetterNumber)} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]-<?Nl>>$/ ), q{Don't match compound inverted <?Nl> (LetterNumber)} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<+<-Nl>>$/ ), q{Don't match internally inverted <?Nl> (LetterNumber)} );
ok(!( "\x[1B95]"  ~~ m/^<+<?Nl>>$/ ), q{Don't match unrelated <?Nl> (LetterNumber)} );
ok("\x[1B95]"  ~~ m/^<-<?Nl>>$/, q{Match unrelated externally inverted <?Nl> (LetterNumber)} );
ok("\x[1B95]"  ~~ m/^<+<-Nl>>$/, q{Match unrelated internally inverted <?Nl> (LetterNumber)} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<+<?Nl>>$/ ), q{Don't match related <?Nl> (LetterNumber)} );
ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<+<-Nl>>$/, q{Match related internally inverted <?Nl> (LetterNumber)} );
ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<-<?Nl>>$/, q{Match related externally inverted <?Nl> (LetterNumber)} );
ok("\x[1B95]\c[SUPERSCRIPT ZERO]\c[ROMAN NUMERAL ONE]" ~~ m/<+<?Nl>>/, q{Match unanchored <?Nl> (LetterNumber)} );

ok("\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<+<?LetterNumber>>$/, q{Match <?LetterNumber>} );
ok("\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<[A]+<?LetterNumber>>$/, q{Match compound <?LetterNumber>} );
ok(!( "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<-<?LetterNumber>>$/ ), q{Don't match externally inverted <?LetterNumber>} );
ok(!( "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<[A]-<?LetterNumber>>$/ ), q{Don't match compound inverted <?LetterNumber>} );
ok(!( "\c[RUNIC ARLAUG SYMBOL]" ~~ m/^<+<-LetterNumber>>$/ ), q{Don't match internally inverted <?LetterNumber>} );
ok(!( "\x[9B4F]"  ~~ m/^<+<?LetterNumber>>$/ ), q{Don't match unrelated <?LetterNumber>} );
ok("\x[9B4F]"  ~~ m/^<-<?LetterNumber>>$/, q{Match unrelated externally inverted <?LetterNumber>} );
ok("\x[9B4F]"  ~~ m/^<+<-LetterNumber>>$/, q{Match unrelated internally inverted <?LetterNumber>} );
ok(!( "\x[9B4F]" ~~ m/^<+<?LetterNumber>>$/ ), q{Don't match related <?LetterNumber>} );
ok("\x[9B4F]" ~~ m/^<+<-LetterNumber>>$/, q{Match related internally inverted <?LetterNumber>} );
ok("\x[9B4F]" ~~ m/^<-<?LetterNumber>>$/, q{Match related externally inverted <?LetterNumber>} );
ok("\x[9B4F]\x[9B4F]\c[RUNIC ARLAUG SYMBOL]" ~~ m/<+<?LetterNumber>>/, q{Match unanchored <?LetterNumber>} );

# No          OtherNumber


ok("\c[SUPERSCRIPT TWO]" ~~ m/^<+<?No>>$/, q{Match <?No> (OtherNumber)} );
ok("\c[SUPERSCRIPT TWO]" ~~ m/^<[A]+<?No>>$/, q{Match compound <?No> (OtherNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<-<?No>>$/ ), q{Don't match externally inverted <?No> (OtherNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<[A]-<?No>>$/ ), q{Don't match compound inverted <?No> (OtherNumber)} );
ok(!( "\c[SUPERSCRIPT TWO]" ~~ m/^<+<-No>>$/ ), q{Don't match internally inverted <?No> (OtherNumber)} );
ok(!( "\x[8F9A]"  ~~ m/^<+<?No>>$/ ), q{Don't match unrelated <?No> (OtherNumber)} );
ok("\x[8F9A]"  ~~ m/^<-<?No>>$/, q{Match unrelated externally inverted <?No> (OtherNumber)} );
ok("\x[8F9A]"  ~~ m/^<+<-No>>$/, q{Match unrelated internally inverted <?No> (OtherNumber)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<?No>>$/ ), q{Don't match related <?No> (OtherNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<+<-No>>$/, q{Match related internally inverted <?No> (OtherNumber)} );
ok("\c[DIGIT ZERO]" ~~ m/^<-<?No>>$/, q{Match related externally inverted <?No> (OtherNumber)} );
ok("\x[8F9A]\c[DIGIT ZERO]\c[SUPERSCRIPT TWO]" ~~ m/<+<?No>>/, q{Match unanchored <?No> (OtherNumber)} );

ok("\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<+<?OtherNumber>>$/, q{Match <?OtherNumber>} );
ok("\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<[A]+<?OtherNumber>>$/, q{Match compound <?OtherNumber>} );
ok(!( "\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<-<?OtherNumber>>$/ ), q{Don't match externally inverted <?OtherNumber>} );
ok(!( "\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<[A]-<?OtherNumber>>$/ ), q{Don't match compound inverted <?OtherNumber>} );
ok(!( "\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/^<+<-OtherNumber>>$/ ), q{Don't match internally inverted <?OtherNumber>} );
ok(!( "\x[0522]"  ~~ m/^<+<?OtherNumber>>$/ ), q{Don't match unrelated <?OtherNumber>} );
ok("\x[0522]"  ~~ m/^<-<?OtherNumber>>$/, q{Match unrelated externally inverted <?OtherNumber>} );
ok("\x[0522]"  ~~ m/^<+<-OtherNumber>>$/, q{Match unrelated internally inverted <?OtherNumber>} );
ok("\x[0522]\c[BENGALI CURRENCY NUMERATOR ONE]" ~~ m/<+<?OtherNumber>>/, q{Match unanchored <?OtherNumber>} );

# P           Punctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<+<P>>$/, q{Match <P> (Punctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<P>>$/, q{Match compound <P> (Punctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<P>>$/ ), q{Don't match externally inverted <P> (Punctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<P>>$/ ), q{Don't match compound inverted <P> (Punctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-P>>$/ ), q{Don't match internally inverted <P> (Punctuation)} );
ok(!( "\x[3753]"  ~~ m/^<+<P>>$/ ), q{Don't match unrelated <P> (Punctuation)} );
ok("\x[3753]"  ~~ m/^<-<P>>$/, q{Match unrelated externally inverted <P> (Punctuation)} );
ok("\x[3753]"  ~~ m/^<+<-P>>$/, q{Match unrelated internally inverted <P> (Punctuation)} );
ok("\x[3753]\c[EXCLAMATION MARK]" ~~ m/<+<P>>/, q{Match unanchored <P> (Punctuation)} );

ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?Punctuation>>$/, q{Match <?Punctuation>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?Punctuation>>$/, q{Match compound <?Punctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?Punctuation>>$/ ), q{Don't match externally inverted <?Punctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?Punctuation>>$/ ), q{Don't match compound inverted <?Punctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-Punctuation>>$/ ), q{Don't match internally inverted <?Punctuation>} );
ok(!( "\x[9C5E]"  ~~ m/^<+<?Punctuation>>$/ ), q{Don't match unrelated <?Punctuation>} );
ok("\x[9C5E]"  ~~ m/^<-<?Punctuation>>$/, q{Match unrelated externally inverted <?Punctuation>} );
ok("\x[9C5E]"  ~~ m/^<+<-Punctuation>>$/, q{Match unrelated internally inverted <?Punctuation>} );
ok("\x[9C5E]\c[EXCLAMATION MARK]" ~~ m/<+<?Punctuation>>/, q{Match unanchored <?Punctuation>} );

# Pc          ConnectorPunctuation


ok("\c[LOW LINE]" ~~ m/^<+<?Pc>>$/, q{Match <?Pc> (ConnectorPunctuation)} );
ok("\c[LOW LINE]" ~~ m/^<[A]+<?Pc>>$/, q{Match compound <?Pc> (ConnectorPunctuation)} );
ok(!( "\c[LOW LINE]" ~~ m/^<-<?Pc>>$/ ), q{Don't match externally inverted <?Pc> (ConnectorPunctuation)} );
ok(!( "\c[LOW LINE]" ~~ m/^<[A]-<?Pc>>$/ ), q{Don't match compound inverted <?Pc> (ConnectorPunctuation)} );
ok(!( "\c[LOW LINE]" ~~ m/^<+<-Pc>>$/ ), q{Don't match internally inverted <?Pc> (ConnectorPunctuation)} );
ok(!( "\x[B2C9]"  ~~ m/^<+<?Pc>>$/ ), q{Don't match unrelated <?Pc> (ConnectorPunctuation)} );
ok("\x[B2C9]"  ~~ m/^<-<?Pc>>$/, q{Match unrelated externally inverted <?Pc> (ConnectorPunctuation)} );
ok("\x[B2C9]"  ~~ m/^<+<-Pc>>$/, q{Match unrelated internally inverted <?Pc> (ConnectorPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?Pc>>$/ ), q{Don't match related <?Pc> (ConnectorPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-Pc>>$/, q{Match related internally inverted <?Pc> (ConnectorPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?Pc>>$/, q{Match related externally inverted <?Pc> (ConnectorPunctuation)} );
ok("\x[B2C9]\c[EXCLAMATION MARK]\c[LOW LINE]" ~~ m/<+<?Pc>>/, q{Match unanchored <?Pc> (ConnectorPunctuation)} );

ok("\c[LOW LINE]" ~~ m/^<+<?ConnectorPunctuation>>$/, q{Match <?ConnectorPunctuation>} );
ok("\c[LOW LINE]" ~~ m/^<[A]+<?ConnectorPunctuation>>$/, q{Match compound <?ConnectorPunctuation>} );
ok(!( "\c[LOW LINE]" ~~ m/^<-<?ConnectorPunctuation>>$/ ), q{Don't match externally inverted <?ConnectorPunctuation>} );
ok(!( "\c[LOW LINE]" ~~ m/^<[A]-<?ConnectorPunctuation>>$/ ), q{Don't match compound inverted <?ConnectorPunctuation>} );
ok(!( "\c[LOW LINE]" ~~ m/^<+<-ConnectorPunctuation>>$/ ), q{Don't match internally inverted <?ConnectorPunctuation>} );
ok(!( "\x[AEFC]"  ~~ m/^<+<?ConnectorPunctuation>>$/ ), q{Don't match unrelated <?ConnectorPunctuation>} );
ok("\x[AEFC]"  ~~ m/^<-<?ConnectorPunctuation>>$/, q{Match unrelated externally inverted <?ConnectorPunctuation>} );
ok("\x[AEFC]"  ~~ m/^<+<-ConnectorPunctuation>>$/, q{Match unrelated internally inverted <?ConnectorPunctuation>} );
ok("\x[AEFC]\c[LOW LINE]" ~~ m/<+<?ConnectorPunctuation>>/, q{Match unanchored <?ConnectorPunctuation>} );

# Pd          DashPunctuation


ok("\c[HYPHEN-MINUS]" ~~ m/^<+<?Pd>>$/, q{Match <?Pd> (DashPunctuation)} );
ok("\c[HYPHEN-MINUS]" ~~ m/^<[A]+<?Pd>>$/, q{Match compound <?Pd> (DashPunctuation)} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<-<?Pd>>$/ ), q{Don't match externally inverted <?Pd> (DashPunctuation)} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<[A]-<?Pd>>$/ ), q{Don't match compound inverted <?Pd> (DashPunctuation)} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<+<-Pd>>$/ ), q{Don't match internally inverted <?Pd> (DashPunctuation)} );
ok(!( "\x[86C8]"  ~~ m/^<+<?Pd>>$/ ), q{Don't match unrelated <?Pd> (DashPunctuation)} );
ok("\x[86C8]"  ~~ m/^<-<?Pd>>$/, q{Match unrelated externally inverted <?Pd> (DashPunctuation)} );
ok("\x[86C8]"  ~~ m/^<+<-Pd>>$/, q{Match unrelated internally inverted <?Pd> (DashPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?Pd>>$/ ), q{Don't match related <?Pd> (DashPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-Pd>>$/, q{Match related internally inverted <?Pd> (DashPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?Pd>>$/, q{Match related externally inverted <?Pd> (DashPunctuation)} );
ok("\x[86C8]\c[EXCLAMATION MARK]\c[HYPHEN-MINUS]" ~~ m/<+<?Pd>>/, q{Match unanchored <?Pd> (DashPunctuation)} );

ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<?DashPunctuation>>$/, q{Match <?DashPunctuation>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]+<?DashPunctuation>>$/, q{Match compound <?DashPunctuation>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<-<?DashPunctuation>>$/ ), q{Don't match externally inverted <?DashPunctuation>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]-<?DashPunctuation>>$/ ), q{Don't match compound inverted <?DashPunctuation>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<-DashPunctuation>>$/ ), q{Don't match internally inverted <?DashPunctuation>} );
ok(!( "\c[HIRAGANA LETTER NI]"  ~~ m/^<+<?DashPunctuation>>$/ ), q{Don't match unrelated <?DashPunctuation>} );
ok("\c[HIRAGANA LETTER NI]"  ~~ m/^<-<?DashPunctuation>>$/, q{Match unrelated externally inverted <?DashPunctuation>} );
ok("\c[HIRAGANA LETTER NI]"  ~~ m/^<+<-DashPunctuation>>$/, q{Match unrelated internally inverted <?DashPunctuation>} );
ok("\c[HIRAGANA LETTER NI]\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/<+<?DashPunctuation>>/, q{Match unanchored <?DashPunctuation>} );

# Ps          OpenPunctuation


ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?Ps>>$/, q{Match <?Ps> (OpenPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?Ps>>$/, q{Match compound <?Ps> (OpenPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?Ps>>$/ ), q{Don't match externally inverted <?Ps> (OpenPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?Ps>>$/ ), q{Don't match compound inverted <?Ps> (OpenPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-Ps>>$/ ), q{Don't match internally inverted <?Ps> (OpenPunctuation)} );
ok(!( "\x[B601]"  ~~ m/^<+<?Ps>>$/ ), q{Don't match unrelated <?Ps> (OpenPunctuation)} );
ok("\x[B601]"  ~~ m/^<-<?Ps>>$/, q{Match unrelated externally inverted <?Ps> (OpenPunctuation)} );
ok("\x[B601]"  ~~ m/^<+<-Ps>>$/, q{Match unrelated internally inverted <?Ps> (OpenPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?Ps>>$/ ), q{Don't match related <?Ps> (OpenPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-Ps>>$/, q{Match related internally inverted <?Ps> (OpenPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?Ps>>$/, q{Match related externally inverted <?Ps> (OpenPunctuation)} );
ok("\x[B601]\c[EXCLAMATION MARK]\c[LEFT PARENTHESIS]" ~~ m/<+<?Ps>>/, q{Match unanchored <?Ps> (OpenPunctuation)} );

ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?OpenPunctuation>>$/, q{Match <?OpenPunctuation>} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?OpenPunctuation>>$/, q{Match compound <?OpenPunctuation>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?OpenPunctuation>>$/ ), q{Don't match externally inverted <?OpenPunctuation>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?OpenPunctuation>>$/ ), q{Don't match compound inverted <?OpenPunctuation>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-OpenPunctuation>>$/ ), q{Don't match internally inverted <?OpenPunctuation>} );
ok(!( "\x[89C1]"  ~~ m/^<+<?OpenPunctuation>>$/ ), q{Don't match unrelated <?OpenPunctuation>} );
ok("\x[89C1]"  ~~ m/^<-<?OpenPunctuation>>$/, q{Match unrelated externally inverted <?OpenPunctuation>} );
ok("\x[89C1]"  ~~ m/^<+<-OpenPunctuation>>$/, q{Match unrelated internally inverted <?OpenPunctuation>} );
ok("\x[89C1]\c[LEFT PARENTHESIS]" ~~ m/<+<?OpenPunctuation>>/, q{Match unanchored <?OpenPunctuation>} );

# Pe          ClosePunctuation


ok("\c[RIGHT PARENTHESIS]" ~~ m/^<+<?Pe>>$/, q{Match <?Pe> (ClosePunctuation)} );
ok("\c[RIGHT PARENTHESIS]" ~~ m/^<[A]+<?Pe>>$/, q{Match compound <?Pe> (ClosePunctuation)} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<-<?Pe>>$/ ), q{Don't match externally inverted <?Pe> (ClosePunctuation)} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<[A]-<?Pe>>$/ ), q{Don't match compound inverted <?Pe> (ClosePunctuation)} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<+<-Pe>>$/ ), q{Don't match internally inverted <?Pe> (ClosePunctuation)} );
ok(!( "\x[5561]"  ~~ m/^<+<?Pe>>$/ ), q{Don't match unrelated <?Pe> (ClosePunctuation)} );
ok("\x[5561]"  ~~ m/^<-<?Pe>>$/, q{Match unrelated externally inverted <?Pe> (ClosePunctuation)} );
ok("\x[5561]"  ~~ m/^<+<-Pe>>$/, q{Match unrelated internally inverted <?Pe> (ClosePunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?Pe>>$/ ), q{Don't match related <?Pe> (ClosePunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-Pe>>$/, q{Match related internally inverted <?Pe> (ClosePunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?Pe>>$/, q{Match related externally inverted <?Pe> (ClosePunctuation)} );
ok("\x[5561]\c[EXCLAMATION MARK]\c[RIGHT PARENTHESIS]" ~~ m/<+<?Pe>>/, q{Match unanchored <?Pe> (ClosePunctuation)} );

ok("\c[RIGHT PARENTHESIS]" ~~ m/^<+<?ClosePunctuation>>$/, q{Match <?ClosePunctuation>} );
ok("\c[RIGHT PARENTHESIS]" ~~ m/^<[A]+<?ClosePunctuation>>$/, q{Match compound <?ClosePunctuation>} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<-<?ClosePunctuation>>$/ ), q{Don't match externally inverted <?ClosePunctuation>} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<[A]-<?ClosePunctuation>>$/ ), q{Don't match compound inverted <?ClosePunctuation>} );
ok(!( "\c[RIGHT PARENTHESIS]" ~~ m/^<+<-ClosePunctuation>>$/ ), q{Don't match internally inverted <?ClosePunctuation>} );
ok(!( "\x[98D0]"  ~~ m/^<+<?ClosePunctuation>>$/ ), q{Don't match unrelated <?ClosePunctuation>} );
ok("\x[98D0]"  ~~ m/^<-<?ClosePunctuation>>$/, q{Match unrelated externally inverted <?ClosePunctuation>} );
ok("\x[98D0]"  ~~ m/^<+<-ClosePunctuation>>$/, q{Match unrelated internally inverted <?ClosePunctuation>} );
ok("\x[98D0]\c[RIGHT PARENTHESIS]" ~~ m/<+<?ClosePunctuation>>/, q{Match unanchored <?ClosePunctuation>} );

# Pi          InitialPunctuation


ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<?Pi>>$/, q{Match <?Pi> (InitialPunctuation)} );
ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]+<?Pi>>$/, q{Match compound <?Pi> (InitialPunctuation)} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-<?Pi>>$/ ), q{Don't match externally inverted <?Pi> (InitialPunctuation)} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]-<?Pi>>$/ ), q{Don't match compound inverted <?Pi> (InitialPunctuation)} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<-Pi>>$/ ), q{Don't match internally inverted <?Pi> (InitialPunctuation)} );
ok(!( "\x[D76F]"  ~~ m/^<+<?Pi>>$/ ), q{Don't match unrelated <?Pi> (InitialPunctuation)} );
ok("\x[D76F]"  ~~ m/^<-<?Pi>>$/, q{Match unrelated externally inverted <?Pi> (InitialPunctuation)} );
ok("\x[D76F]"  ~~ m/^<+<-Pi>>$/, q{Match unrelated internally inverted <?Pi> (InitialPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<?Pi>>$/ ), q{Don't match related <?Pi> (InitialPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<+<-Pi>>$/, q{Match related internally inverted <?Pi> (InitialPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<-<?Pi>>$/, q{Match related externally inverted <?Pi> (InitialPunctuation)} );
ok("\x[D76F]\c[EXCLAMATION MARK]\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<+<?Pi>>/, q{Match unanchored <?Pi> (InitialPunctuation)} );

ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<?InitialPunctuation>>$/, q{Match <?InitialPunctuation>} );
ok("\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]+<?InitialPunctuation>>$/, q{Match compound <?InitialPunctuation>} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<-<?InitialPunctuation>>$/ ), q{Don't match externally inverted <?InitialPunctuation>} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<[A]-<?InitialPunctuation>>$/ ), q{Don't match compound inverted <?InitialPunctuation>} );
ok(!( "\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/^<+<-InitialPunctuation>>$/ ), q{Don't match internally inverted <?InitialPunctuation>} );
ok(!( "\x[C96E]"  ~~ m/^<+<?InitialPunctuation>>$/ ), q{Don't match unrelated <?InitialPunctuation>} );
ok("\x[C96E]"  ~~ m/^<-<?InitialPunctuation>>$/, q{Match unrelated externally inverted <?InitialPunctuation>} );
ok("\x[C96E]"  ~~ m/^<+<-InitialPunctuation>>$/, q{Match unrelated internally inverted <?InitialPunctuation>} );
ok("\x[C96E]\c[LEFT-POINTING DOUBLE ANGLE QUOTATION MARK]" ~~ m/<+<?InitialPunctuation>>/, q{Match unanchored <?InitialPunctuation>} );

# Pf          FinalPunctuation


ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<?Pf>>$/, q{Match <?Pf> (FinalPunctuation)} );
ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]+<?Pf>>$/, q{Match compound <?Pf> (FinalPunctuation)} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<-<?Pf>>$/ ), q{Don't match externally inverted <?Pf> (FinalPunctuation)} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]-<?Pf>>$/ ), q{Don't match compound inverted <?Pf> (FinalPunctuation)} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<-Pf>>$/ ), q{Don't match internally inverted <?Pf> (FinalPunctuation)} );
ok(!( "\x[0515]"  ~~ m/^<+<?Pf>>$/ ), q{Don't match unrelated <?Pf> (FinalPunctuation)} );
ok("\x[0515]"  ~~ m/^<-<?Pf>>$/, q{Match unrelated externally inverted <?Pf> (FinalPunctuation)} );
ok("\x[0515]"  ~~ m/^<+<-Pf>>$/, q{Match unrelated internally inverted <?Pf> (FinalPunctuation)} );
ok(!( "\c[ARMENIAN APOSTROPHE]" ~~ m/^<+<?Pf>>$/ ), q{Don't match related <?Pf> (FinalPunctuation)} );
ok("\c[ARMENIAN APOSTROPHE]" ~~ m/^<+<-Pf>>$/, q{Match related internally inverted <?Pf> (FinalPunctuation)} );
ok("\c[ARMENIAN APOSTROPHE]" ~~ m/^<-<?Pf>>$/, q{Match related externally inverted <?Pf> (FinalPunctuation)} );
ok("\x[0515]\c[ARMENIAN APOSTROPHE]\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/<+<?Pf>>/, q{Match unanchored <?Pf> (FinalPunctuation)} );

ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<?FinalPunctuation>>$/, q{Match <?FinalPunctuation>} );
ok("\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]+<?FinalPunctuation>>$/, q{Match compound <?FinalPunctuation>} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<-<?FinalPunctuation>>$/ ), q{Don't match externally inverted <?FinalPunctuation>} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<[A]-<?FinalPunctuation>>$/ ), q{Don't match compound inverted <?FinalPunctuation>} );
ok(!( "\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/^<+<-FinalPunctuation>>$/ ), q{Don't match internally inverted <?FinalPunctuation>} );
ok(!( "\c[MODIFIER LETTER PRIME]"  ~~ m/^<+<?FinalPunctuation>>$/ ), q{Don't match unrelated <?FinalPunctuation>} );
ok("\c[MODIFIER LETTER PRIME]"  ~~ m/^<-<?FinalPunctuation>>$/, q{Match unrelated externally inverted <?FinalPunctuation>} );
ok("\c[MODIFIER LETTER PRIME]"  ~~ m/^<+<-FinalPunctuation>>$/, q{Match unrelated internally inverted <?FinalPunctuation>} );
ok("\c[MODIFIER LETTER PRIME]\c[RIGHT SINGLE QUOTATION MARK]" ~~ m/<+<?FinalPunctuation>>/, q{Match unanchored <?FinalPunctuation>} );

# Po          OtherPunctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?Po>>$/, q{Match <?Po> (OtherPunctuation)} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?Po>>$/, q{Match compound <?Po> (OtherPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?Po>>$/ ), q{Don't match externally inverted <?Po> (OtherPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?Po>>$/ ), q{Don't match compound inverted <?Po> (OtherPunctuation)} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-Po>>$/ ), q{Don't match internally inverted <?Po> (OtherPunctuation)} );
ok(!( "\x[A586]"  ~~ m/^<+<?Po>>$/ ), q{Don't match unrelated <?Po> (OtherPunctuation)} );
ok("\x[A586]"  ~~ m/^<-<?Po>>$/, q{Match unrelated externally inverted <?Po> (OtherPunctuation)} );
ok("\x[A586]"  ~~ m/^<+<-Po>>$/, q{Match unrelated internally inverted <?Po> (OtherPunctuation)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<?Po>>$/ ), q{Don't match related <?Po> (OtherPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<-Po>>$/, q{Match related internally inverted <?Po> (OtherPunctuation)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<-<?Po>>$/, q{Match related externally inverted <?Po> (OtherPunctuation)} );
ok("\x[A586]\c[LEFT PARENTHESIS]\c[EXCLAMATION MARK]" ~~ m/<+<?Po>>/, q{Match unanchored <?Po> (OtherPunctuation)} );

ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?OtherPunctuation>>$/, q{Match <?OtherPunctuation>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?OtherPunctuation>>$/, q{Match compound <?OtherPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?OtherPunctuation>>$/ ), q{Don't match externally inverted <?OtherPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?OtherPunctuation>>$/ ), q{Don't match compound inverted <?OtherPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-OtherPunctuation>>$/ ), q{Don't match internally inverted <?OtherPunctuation>} );
ok(!( "\x[5FBD]"  ~~ m/^<+<?OtherPunctuation>>$/ ), q{Don't match unrelated <?OtherPunctuation>} );
ok("\x[5FBD]"  ~~ m/^<-<?OtherPunctuation>>$/, q{Match unrelated externally inverted <?OtherPunctuation>} );
ok("\x[5FBD]"  ~~ m/^<+<-OtherPunctuation>>$/, q{Match unrelated internally inverted <?OtherPunctuation>} );
ok("\x[5FBD]\c[EXCLAMATION MARK]" ~~ m/<+<?OtherPunctuation>>/, q{Match unanchored <?OtherPunctuation>} );

# S           Symbol


ok("\c[GUJARATI RUPEE SIGN]" ~~ m/^<+<S>>$/, q{Match <S> (Symbol)} );
ok("\c[GUJARATI RUPEE SIGN]" ~~ m/^<[A]+<S>>$/, q{Match compound <S> (Symbol)} );
ok(!( "\c[GUJARATI RUPEE SIGN]" ~~ m/^<-<S>>$/ ), q{Don't match externally inverted <S> (Symbol)} );
ok(!( "\c[GUJARATI RUPEE SIGN]" ~~ m/^<[A]-<S>>$/ ), q{Don't match compound inverted <S> (Symbol)} );
ok(!( "\c[GUJARATI RUPEE SIGN]" ~~ m/^<+<-S>>$/ ), q{Don't match internally inverted <S> (Symbol)} );
ok(!( "\c[GURMUKHI SIGN ADAK BINDI]"  ~~ m/^<+<S>>$/ ), q{Don't match unrelated <S> (Symbol)} );
ok("\c[GURMUKHI SIGN ADAK BINDI]"  ~~ m/^<-<S>>$/, q{Match unrelated externally inverted <S> (Symbol)} );
ok("\c[GURMUKHI SIGN ADAK BINDI]"  ~~ m/^<+<-S>>$/, q{Match unrelated internally inverted <S> (Symbol)} );
ok("\c[GURMUKHI SIGN ADAK BINDI]\c[GUJARATI RUPEE SIGN]" ~~ m/<+<S>>/, q{Match unanchored <S> (Symbol)} );

ok("\c[LIMBU SIGN LOO]" ~~ m/^<+<?Symbol>>$/, q{Match <?Symbol>} );
ok("\c[LIMBU SIGN LOO]" ~~ m/^<[A]+<?Symbol>>$/, q{Match compound <?Symbol>} );
ok(!( "\c[LIMBU SIGN LOO]" ~~ m/^<-<?Symbol>>$/ ), q{Don't match externally inverted <?Symbol>} );
ok(!( "\c[LIMBU SIGN LOO]" ~~ m/^<[A]-<?Symbol>>$/ ), q{Don't match compound inverted <?Symbol>} );
ok(!( "\c[LIMBU SIGN LOO]" ~~ m/^<+<-Symbol>>$/ ), q{Don't match internally inverted <?Symbol>} );
ok(!( "\x[192C]"  ~~ m/^<+<?Symbol>>$/ ), q{Don't match unrelated <?Symbol>} );
ok("\x[192C]"  ~~ m/^<-<?Symbol>>$/, q{Match unrelated externally inverted <?Symbol>} );
ok("\x[192C]"  ~~ m/^<+<-Symbol>>$/, q{Match unrelated internally inverted <?Symbol>} );
ok("\x[192C]\c[LIMBU SIGN LOO]" ~~ m/<+<?Symbol>>/, q{Match unanchored <?Symbol>} );

# Sm          MathSymbol


ok("\c[PLUS SIGN]" ~~ m/^<+<?Sm>>$/, q{Match <?Sm> (MathSymbol)} );
ok("\c[PLUS SIGN]" ~~ m/^<[A]+<?Sm>>$/, q{Match compound <?Sm> (MathSymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<-<?Sm>>$/ ), q{Don't match externally inverted <?Sm> (MathSymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<[A]-<?Sm>>$/ ), q{Don't match compound inverted <?Sm> (MathSymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<+<-Sm>>$/ ), q{Don't match internally inverted <?Sm> (MathSymbol)} );
ok(!( "\x[769B]"  ~~ m/^<+<?Sm>>$/ ), q{Don't match unrelated <?Sm> (MathSymbol)} );
ok("\x[769B]"  ~~ m/^<-<?Sm>>$/, q{Match unrelated externally inverted <?Sm> (MathSymbol)} );
ok("\x[769B]"  ~~ m/^<+<-Sm>>$/, q{Match unrelated internally inverted <?Sm> (MathSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<?Sm>>$/ ), q{Don't match related <?Sm> (MathSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<+<-Sm>>$/, q{Match related internally inverted <?Sm> (MathSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-<?Sm>>$/, q{Match related externally inverted <?Sm> (MathSymbol)} );
ok("\x[769B]\c[YI RADICAL QOT]\c[PLUS SIGN]" ~~ m/<+<?Sm>>/, q{Match unanchored <?Sm> (MathSymbol)} );

ok("\c[FRACTION SLASH]" ~~ m/^<+<?MathSymbol>>$/, q{Match <?MathSymbol>} );
ok("\c[FRACTION SLASH]" ~~ m/^<[A]+<?MathSymbol>>$/, q{Match compound <?MathSymbol>} );
ok(!( "\c[FRACTION SLASH]" ~~ m/^<-<?MathSymbol>>$/ ), q{Don't match externally inverted <?MathSymbol>} );
ok(!( "\c[FRACTION SLASH]" ~~ m/^<[A]-<?MathSymbol>>$/ ), q{Don't match compound inverted <?MathSymbol>} );
ok(!( "\c[FRACTION SLASH]" ~~ m/^<+<-MathSymbol>>$/ ), q{Don't match internally inverted <?MathSymbol>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]"  ~~ m/^<+<?MathSymbol>>$/ ), q{Don't match unrelated <?MathSymbol>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]"  ~~ m/^<-<?MathSymbol>>$/, q{Match unrelated externally inverted <?MathSymbol>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]"  ~~ m/^<+<-MathSymbol>>$/, q{Match unrelated internally inverted <?MathSymbol>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<?MathSymbol>>$/ ), q{Don't match related <?MathSymbol>} );
ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<-MathSymbol>>$/, q{Match related internally inverted <?MathSymbol>} );
ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<-<?MathSymbol>>$/, q{Match related externally inverted <?MathSymbol>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI]\c[COMBINING LEFT HARPOON ABOVE]\c[FRACTION SLASH]" ~~ m/<+<?MathSymbol>>/, q{Match unanchored <?MathSymbol>} );

# Sc          CurrencySymbol


ok("\c[DOLLAR SIGN]" ~~ m/^<+<?Sc>>$/, q{Match <?Sc> (CurrencySymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<[A]+<?Sc>>$/, q{Match compound <?Sc> (CurrencySymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<-<?Sc>>$/ ), q{Don't match externally inverted <?Sc> (CurrencySymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<[A]-<?Sc>>$/ ), q{Don't match compound inverted <?Sc> (CurrencySymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<+<-Sc>>$/ ), q{Don't match internally inverted <?Sc> (CurrencySymbol)} );
ok(!( "\x[B6B4]"  ~~ m/^<+<?Sc>>$/ ), q{Don't match unrelated <?Sc> (CurrencySymbol)} );
ok("\x[B6B4]"  ~~ m/^<-<?Sc>>$/, q{Match unrelated externally inverted <?Sc> (CurrencySymbol)} );
ok("\x[B6B4]"  ~~ m/^<+<-Sc>>$/, q{Match unrelated internally inverted <?Sc> (CurrencySymbol)} );
ok(!( "\c[PLUS SIGN]" ~~ m/^<+<?Sc>>$/ ), q{Don't match related <?Sc> (CurrencySymbol)} );
ok("\c[PLUS SIGN]" ~~ m/^<+<-Sc>>$/, q{Match related internally inverted <?Sc> (CurrencySymbol)} );
ok("\c[PLUS SIGN]" ~~ m/^<-<?Sc>>$/, q{Match related externally inverted <?Sc> (CurrencySymbol)} );
ok("\x[B6B4]\c[PLUS SIGN]\c[DOLLAR SIGN]" ~~ m/<+<?Sc>>/, q{Match unanchored <?Sc> (CurrencySymbol)} );

ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<+<?CurrencySymbol>>$/, q{Match <?CurrencySymbol>} );
ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]+<?CurrencySymbol>>$/, q{Match compound <?CurrencySymbol>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<-<?CurrencySymbol>>$/ ), q{Don't match externally inverted <?CurrencySymbol>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]-<?CurrencySymbol>>$/ ), q{Don't match compound inverted <?CurrencySymbol>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<+<-CurrencySymbol>>$/ ), q{Don't match internally inverted <?CurrencySymbol>} );
ok(!( "\x[1CD3]"  ~~ m/^<+<?CurrencySymbol>>$/ ), q{Don't match unrelated <?CurrencySymbol>} );
ok("\x[1CD3]"  ~~ m/^<-<?CurrencySymbol>>$/, q{Match unrelated externally inverted <?CurrencySymbol>} );
ok("\x[1CD3]"  ~~ m/^<+<-CurrencySymbol>>$/, q{Match unrelated internally inverted <?CurrencySymbol>} );
ok("\x[1CD3]\c[EURO-CURRENCY SIGN]" ~~ m/<+<?CurrencySymbol>>/, q{Match unanchored <?CurrencySymbol>} );

# Sk          ModifierSymbol


ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<?Sk>>$/, q{Match <?Sk> (ModifierSymbol)} );
ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]+<?Sk>>$/, q{Match compound <?Sk> (ModifierSymbol)} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-<?Sk>>$/ ), q{Don't match externally inverted <?Sk> (ModifierSymbol)} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]-<?Sk>>$/ ), q{Don't match compound inverted <?Sk> (ModifierSymbol)} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<-Sk>>$/ ), q{Don't match internally inverted <?Sk> (ModifierSymbol)} );
ok(!( "\x[68FA]"  ~~ m/^<+<?Sk>>$/ ), q{Don't match unrelated <?Sk> (ModifierSymbol)} );
ok("\x[68FA]"  ~~ m/^<-<?Sk>>$/, q{Match unrelated externally inverted <?Sk> (ModifierSymbol)} );
ok("\x[68FA]"  ~~ m/^<+<-Sk>>$/, q{Match unrelated internally inverted <?Sk> (ModifierSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<?Sk>>$/ ), q{Don't match related <?Sk> (ModifierSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<+<-Sk>>$/, q{Match related internally inverted <?Sk> (ModifierSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-<?Sk>>$/, q{Match related externally inverted <?Sk> (ModifierSymbol)} );
ok("\x[68FA]\c[YI RADICAL QOT]\c[CIRCUMFLEX ACCENT]" ~~ m/<+<?Sk>>/, q{Match unanchored <?Sk> (ModifierSymbol)} );

ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<?ModifierSymbol>>$/, q{Match <?ModifierSymbol>} );
ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]+<?ModifierSymbol>>$/, q{Match compound <?ModifierSymbol>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-<?ModifierSymbol>>$/ ), q{Don't match externally inverted <?ModifierSymbol>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]-<?ModifierSymbol>>$/ ), q{Don't match compound inverted <?ModifierSymbol>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<-ModifierSymbol>>$/ ), q{Don't match internally inverted <?ModifierSymbol>} );
ok(!( "\x[69E7]"  ~~ m/^<+<?ModifierSymbol>>$/ ), q{Don't match unrelated <?ModifierSymbol>} );
ok("\x[69E7]"  ~~ m/^<-<?ModifierSymbol>>$/, q{Match unrelated externally inverted <?ModifierSymbol>} );
ok("\x[69E7]"  ~~ m/^<+<-ModifierSymbol>>$/, q{Match unrelated internally inverted <?ModifierSymbol>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?ModifierSymbol>>$/ ), q{Don't match related <?ModifierSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-ModifierSymbol>>$/, q{Match related internally inverted <?ModifierSymbol>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?ModifierSymbol>>$/, q{Match related externally inverted <?ModifierSymbol>} );
ok("\x[69E7]\c[COMBINING GRAVE ACCENT]\c[CIRCUMFLEX ACCENT]" ~~ m/<+<?ModifierSymbol>>/, q{Match unanchored <?ModifierSymbol>} );

# So          OtherSymbol


ok("\c[YI RADICAL QOT]" ~~ m/^<+<?So>>$/, q{Match <?So> (OtherSymbol)} );
ok("\c[YI RADICAL QOT]" ~~ m/^<[A]+<?So>>$/, q{Match compound <?So> (OtherSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<-<?So>>$/ ), q{Don't match externally inverted <?So> (OtherSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<[A]-<?So>>$/ ), q{Don't match compound inverted <?So> (OtherSymbol)} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<-So>>$/ ), q{Don't match internally inverted <?So> (OtherSymbol)} );
ok(!( "\x[8C90]"  ~~ m/^<+<?So>>$/ ), q{Don't match unrelated <?So> (OtherSymbol)} );
ok("\x[8C90]"  ~~ m/^<-<?So>>$/, q{Match unrelated externally inverted <?So> (OtherSymbol)} );
ok("\x[8C90]"  ~~ m/^<+<-So>>$/, q{Match unrelated internally inverted <?So> (OtherSymbol)} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<+<?So>>$/ ), q{Don't match related <?So> (OtherSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<+<-So>>$/, q{Match related internally inverted <?So> (OtherSymbol)} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-<?So>>$/, q{Match related externally inverted <?So> (OtherSymbol)} );
ok("\x[8C90]\c[DOLLAR SIGN]\c[YI RADICAL QOT]" ~~ m/<+<?So>>/, q{Match unanchored <?So> (OtherSymbol)} );

ok("\c[YI RADICAL QOT]" ~~ m/^<+<?OtherSymbol>>$/, q{Match <?OtherSymbol>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<[A]+<?OtherSymbol>>$/, q{Match compound <?OtherSymbol>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<-<?OtherSymbol>>$/ ), q{Don't match externally inverted <?OtherSymbol>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<[A]-<?OtherSymbol>>$/ ), q{Don't match compound inverted <?OtherSymbol>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<-OtherSymbol>>$/ ), q{Don't match internally inverted <?OtherSymbol>} );
ok(!( "\x[95A7]"  ~~ m/^<+<?OtherSymbol>>$/ ), q{Don't match unrelated <?OtherSymbol>} );
ok("\x[95A7]"  ~~ m/^<-<?OtherSymbol>>$/, q{Match unrelated externally inverted <?OtherSymbol>} );
ok("\x[95A7]"  ~~ m/^<+<-OtherSymbol>>$/, q{Match unrelated internally inverted <?OtherSymbol>} );
ok("\x[95A7]\c[YI RADICAL QOT]" ~~ m/<+<?OtherSymbol>>/, q{Match unanchored <?OtherSymbol>} );

# Z           Separator


ok("\c[SPACE]" ~~ m/^<+<Z>>$/, q{Match <Z> (Separator)} );
ok("\c[SPACE]" ~~ m/^<[A]+<Z>>$/, q{Match compound <Z> (Separator)} );
ok(!( "\c[SPACE]" ~~ m/^<-<Z>>$/ ), q{Don't match externally inverted <Z> (Separator)} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<Z>>$/ ), q{Don't match compound inverted <Z> (Separator)} );
ok(!( "\c[SPACE]" ~~ m/^<+<-Z>>$/ ), q{Don't match internally inverted <Z> (Separator)} );
ok(!( "\x[D222]"  ~~ m/^<+<Z>>$/ ), q{Don't match unrelated <Z> (Separator)} );
ok("\x[D222]"  ~~ m/^<-<Z>>$/, q{Match unrelated externally inverted <Z> (Separator)} );
ok("\x[D222]"  ~~ m/^<+<-Z>>$/, q{Match unrelated internally inverted <Z> (Separator)} );
ok("\x[D222]\c[SPACE]" ~~ m/<+<Z>>/, q{Match unanchored <Z> (Separator)} );

ok("\c[SPACE]" ~~ m/^<+<?Separator>>$/, q{Match <?Separator>} );
ok("\c[SPACE]" ~~ m/^<[A]+<?Separator>>$/, q{Match compound <?Separator>} );
ok(!( "\c[SPACE]" ~~ m/^<-<?Separator>>$/ ), q{Don't match externally inverted <?Separator>} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<?Separator>>$/ ), q{Don't match compound inverted <?Separator>} );
ok(!( "\c[SPACE]" ~~ m/^<+<-Separator>>$/ ), q{Don't match internally inverted <?Separator>} );
ok(!( "\x[D7B7]"  ~~ m/^<+<?Separator>>$/ ), q{Don't match unrelated <?Separator>} );
ok("\x[D7B7]"  ~~ m/^<-<?Separator>>$/, q{Match unrelated externally inverted <?Separator>} );
ok("\x[D7B7]"  ~~ m/^<+<-Separator>>$/, q{Match unrelated internally inverted <?Separator>} );
ok(!( "\c[DOLLAR SIGN]" ~~ m/^<+<?Separator>>$/ ), q{Don't match related <?Separator>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<+<-Separator>>$/, q{Match related internally inverted <?Separator>} );
ok("\c[DOLLAR SIGN]" ~~ m/^<-<?Separator>>$/, q{Match related externally inverted <?Separator>} );
ok("\x[D7B7]\c[DOLLAR SIGN]\c[SPACE]" ~~ m/<+<?Separator>>/, q{Match unanchored <?Separator>} );

# Zs          SpaceSeparator


ok("\c[SPACE]" ~~ m/^<+<?Zs>>$/, q{Match <?Zs> (SpaceSeparator)} );
ok("\c[SPACE]" ~~ m/^<[A]+<?Zs>>$/, q{Match compound <?Zs> (SpaceSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<-<?Zs>>$/ ), q{Don't match externally inverted <?Zs> (SpaceSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<?Zs>>$/ ), q{Don't match compound inverted <?Zs> (SpaceSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<+<-Zs>>$/ ), q{Don't match internally inverted <?Zs> (SpaceSeparator)} );
ok(!( "\x[5918]"  ~~ m/^<+<?Zs>>$/ ), q{Don't match unrelated <?Zs> (SpaceSeparator)} );
ok("\x[5918]"  ~~ m/^<-<?Zs>>$/, q{Match unrelated externally inverted <?Zs> (SpaceSeparator)} );
ok("\x[5918]"  ~~ m/^<+<-Zs>>$/, q{Match unrelated internally inverted <?Zs> (SpaceSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<+<?Zs>>$/ ), q{Don't match related <?Zs> (SpaceSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<+<-Zs>>$/, q{Match related internally inverted <?Zs> (SpaceSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<-<?Zs>>$/, q{Match related externally inverted <?Zs> (SpaceSeparator)} );
ok("\x[5918]\c[LINE SEPARATOR]\c[SPACE]" ~~ m/<+<?Zs>>/, q{Match unanchored <?Zs> (SpaceSeparator)} );

ok("\c[SPACE]" ~~ m/^<+<?SpaceSeparator>>$/, q{Match <?SpaceSeparator>} );
ok("\c[SPACE]" ~~ m/^<[A]+<?SpaceSeparator>>$/, q{Match compound <?SpaceSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<-<?SpaceSeparator>>$/ ), q{Don't match externally inverted <?SpaceSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<[A]-<?SpaceSeparator>>$/ ), q{Don't match compound inverted <?SpaceSeparator>} );
ok(!( "\c[SPACE]" ~~ m/^<+<-SpaceSeparator>>$/ ), q{Don't match internally inverted <?SpaceSeparator>} );
ok(!( "\x[3704]"  ~~ m/^<+<?SpaceSeparator>>$/ ), q{Don't match unrelated <?SpaceSeparator>} );
ok("\x[3704]"  ~~ m/^<-<?SpaceSeparator>>$/, q{Match unrelated externally inverted <?SpaceSeparator>} );
ok("\x[3704]"  ~~ m/^<+<-SpaceSeparator>>$/, q{Match unrelated internally inverted <?SpaceSeparator>} );
ok(!( "\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<+<?SpaceSeparator>>$/ ), q{Don't match related <?SpaceSeparator>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<+<-SpaceSeparator>>$/, q{Match related internally inverted <?SpaceSeparator>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]" ~~ m/^<-<?SpaceSeparator>>$/, q{Match related externally inverted <?SpaceSeparator>} );
ok("\x[3704]\c[HEXAGRAM FOR THE CREATIVE HEAVEN]\c[SPACE]" ~~ m/<+<?SpaceSeparator>>/, q{Match unanchored <?SpaceSeparator>} );

# Zl          LineSeparator


ok("\c[LINE SEPARATOR]" ~~ m/^<+<?Zl>>$/, q{Match <?Zl> (LineSeparator)} );
ok("\c[LINE SEPARATOR]" ~~ m/^<[A]+<?Zl>>$/, q{Match compound <?Zl> (LineSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<-<?Zl>>$/ ), q{Don't match externally inverted <?Zl> (LineSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<[A]-<?Zl>>$/ ), q{Don't match compound inverted <?Zl> (LineSeparator)} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<+<-Zl>>$/ ), q{Don't match internally inverted <?Zl> (LineSeparator)} );
ok(!( "\x[ADAD]"  ~~ m/^<+<?Zl>>$/ ), q{Don't match unrelated <?Zl> (LineSeparator)} );
ok("\x[ADAD]"  ~~ m/^<-<?Zl>>$/, q{Match unrelated externally inverted <?Zl> (LineSeparator)} );
ok("\x[ADAD]"  ~~ m/^<+<-Zl>>$/, q{Match unrelated internally inverted <?Zl> (LineSeparator)} );
ok(!( "\c[SPACE]" ~~ m/^<+<?Zl>>$/ ), q{Don't match related <?Zl> (LineSeparator)} );
ok("\c[SPACE]" ~~ m/^<+<-Zl>>$/, q{Match related internally inverted <?Zl> (LineSeparator)} );
ok("\c[SPACE]" ~~ m/^<-<?Zl>>$/, q{Match related externally inverted <?Zl> (LineSeparator)} );
ok("\x[ADAD]\c[SPACE]\c[LINE SEPARATOR]" ~~ m/<+<?Zl>>/, q{Match unanchored <?Zl> (LineSeparator)} );

ok("\c[LINE SEPARATOR]" ~~ m/^<+<?LineSeparator>>$/, q{Match <?LineSeparator>} );
ok("\c[LINE SEPARATOR]" ~~ m/^<[A]+<?LineSeparator>>$/, q{Match compound <?LineSeparator>} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<-<?LineSeparator>>$/ ), q{Don't match externally inverted <?LineSeparator>} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<[A]-<?LineSeparator>>$/ ), q{Don't match compound inverted <?LineSeparator>} );
ok(!( "\c[LINE SEPARATOR]" ~~ m/^<+<-LineSeparator>>$/ ), q{Don't match internally inverted <?LineSeparator>} );
ok(!( "\x[C5E7]"  ~~ m/^<+<?LineSeparator>>$/ ), q{Don't match unrelated <?LineSeparator>} );
ok("\x[C5E7]"  ~~ m/^<-<?LineSeparator>>$/, q{Match unrelated externally inverted <?LineSeparator>} );
ok("\x[C5E7]"  ~~ m/^<+<-LineSeparator>>$/, q{Match unrelated internally inverted <?LineSeparator>} );
ok(!( "\x[C5E7]" ~~ m/^<+<?LineSeparator>>$/ ), q{Don't match related <?LineSeparator>} );
ok("\x[C5E7]" ~~ m/^<+<-LineSeparator>>$/, q{Match related internally inverted <?LineSeparator>} );
ok("\x[C5E7]" ~~ m/^<-<?LineSeparator>>$/, q{Match related externally inverted <?LineSeparator>} );
ok("\x[C5E7]\x[C5E7]\c[LINE SEPARATOR]" ~~ m/<+<?LineSeparator>>/, q{Match unanchored <?LineSeparator>} );

# Zp          ParagraphSeparator


ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<?Zp>>$/, q{Match <?Zp> (ParagraphSeparator)} );
ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]+<?Zp>>$/, q{Match compound <?Zp> (ParagraphSeparator)} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<-<?Zp>>$/ ), q{Don't match externally inverted <?Zp> (ParagraphSeparator)} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]-<?Zp>>$/ ), q{Don't match compound inverted <?Zp> (ParagraphSeparator)} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<-Zp>>$/ ), q{Don't match internally inverted <?Zp> (ParagraphSeparator)} );
ok(!( "\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]"  ~~ m/^<+<?Zp>>$/ ), q{Don't match unrelated <?Zp> (ParagraphSeparator)} );
ok("\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]"  ~~ m/^<-<?Zp>>$/, q{Match unrelated externally inverted <?Zp> (ParagraphSeparator)} );
ok("\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]"  ~~ m/^<+<-Zp>>$/, q{Match unrelated internally inverted <?Zp> (ParagraphSeparator)} );
ok(!( "\c[MONGOLIAN VOWEL SEPARATOR]" ~~ m/^<+<?Zp>>$/ ), q{Don't match related <?Zp> (ParagraphSeparator)} );
ok("\c[MONGOLIAN VOWEL SEPARATOR]" ~~ m/^<+<-Zp>>$/, q{Match related internally inverted <?Zp> (ParagraphSeparator)} );
ok("\c[MONGOLIAN VOWEL SEPARATOR]" ~~ m/^<-<?Zp>>$/, q{Match related externally inverted <?Zp> (ParagraphSeparator)} );
ok("\c[KHMER SYMBOL LEK ATTAK PRAM-BUON]\c[MONGOLIAN VOWEL SEPARATOR]\c[PARAGRAPH SEPARATOR]" ~~ m/<+<?Zp>>/, q{Match unanchored <?Zp> (ParagraphSeparator)} );

ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<?ParagraphSeparator>>$/, q{Match <?ParagraphSeparator>} );
ok("\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]+<?ParagraphSeparator>>$/, q{Match compound <?ParagraphSeparator>} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<-<?ParagraphSeparator>>$/ ), q{Don't match externally inverted <?ParagraphSeparator>} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<[A]-<?ParagraphSeparator>>$/ ), q{Don't match compound inverted <?ParagraphSeparator>} );
ok(!( "\c[PARAGRAPH SEPARATOR]" ~~ m/^<+<-ParagraphSeparator>>$/ ), q{Don't match internally inverted <?ParagraphSeparator>} );
ok(!( "\c[HIRAGANA LETTER KA]"  ~~ m/^<+<?ParagraphSeparator>>$/ ), q{Don't match unrelated <?ParagraphSeparator>} );
ok("\c[HIRAGANA LETTER KA]"  ~~ m/^<-<?ParagraphSeparator>>$/, q{Match unrelated externally inverted <?ParagraphSeparator>} );
ok("\c[HIRAGANA LETTER KA]"  ~~ m/^<+<-ParagraphSeparator>>$/, q{Match unrelated internally inverted <?ParagraphSeparator>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<?ParagraphSeparator>>$/ ), q{Don't match related <?ParagraphSeparator>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<-ParagraphSeparator>>$/, q{Match related internally inverted <?ParagraphSeparator>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<-<?ParagraphSeparator>>$/, q{Match related externally inverted <?ParagraphSeparator>} );
ok("\c[HIRAGANA LETTER KA]\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]\c[PARAGRAPH SEPARATOR]" ~~ m/<+<?ParagraphSeparator>>/, q{Match unanchored <?ParagraphSeparator>} );

# C           Other


ok("\x[9FA6]" ~~ m/^<+<C>>$/, q{Match <C> (Other)} );
ok("\x[9FA6]" ~~ m/^<[A]+<C>>$/, q{Match compound <C> (Other)} );
ok(!( "\x[9FA6]" ~~ m/^<-<C>>$/ ), q{Don't match externally inverted <C> (Other)} );
ok(!( "\x[9FA6]" ~~ m/^<[A]-<C>>$/ ), q{Don't match compound inverted <C> (Other)} );
ok(!( "\x[9FA6]" ~~ m/^<+<-C>>$/ ), q{Don't match internally inverted <C> (Other)} );
ok(!( "\x[56E6]"  ~~ m/^<+<C>>$/ ), q{Don't match unrelated <C> (Other)} );
ok("\x[56E6]"  ~~ m/^<-<C>>$/, q{Match unrelated externally inverted <C> (Other)} );
ok("\x[56E6]"  ~~ m/^<+<-C>>$/, q{Match unrelated internally inverted <C> (Other)} );
ok("\x[56E6]\x[9FA6]" ~~ m/<+<C>>/, q{Match unanchored <C> (Other)} );

ok("\x[0EAC]" ~~ m/^<+<?Other>>$/, q{Match <?Other>} );
ok("\x[0EAC]" ~~ m/^<[A]+<?Other>>$/, q{Match compound <?Other>} );
ok(!( "\x[0EAC]" ~~ m/^<-<?Other>>$/ ), q{Don't match externally inverted <?Other>} );
ok(!( "\x[0EAC]" ~~ m/^<[A]-<?Other>>$/ ), q{Don't match compound inverted <?Other>} );
ok(!( "\x[0EAC]" ~~ m/^<+<-Other>>$/ ), q{Don't match internally inverted <?Other>} );
ok(!( "\c[LAO LETTER HO SUNG]"  ~~ m/^<+<?Other>>$/ ), q{Don't match unrelated <?Other>} );
ok("\c[LAO LETTER HO SUNG]"  ~~ m/^<-<?Other>>$/, q{Match unrelated externally inverted <?Other>} );
ok("\c[LAO LETTER HO SUNG]"  ~~ m/^<+<-Other>>$/, q{Match unrelated internally inverted <?Other>} );
ok("\c[LAO LETTER HO SUNG]\x[0EAC]" ~~ m/<+<?Other>>/, q{Match unanchored <?Other>} );

# Cc          Control


ok("\c[NULL]" ~~ m/^<+<?Cc>>$/, q{Match <?Cc> (Control)} );
ok("\c[NULL]" ~~ m/^<[A]+<?Cc>>$/, q{Match compound <?Cc> (Control)} );
ok(!( "\c[NULL]" ~~ m/^<-<?Cc>>$/ ), q{Don't match externally inverted <?Cc> (Control)} );
ok(!( "\c[NULL]" ~~ m/^<[A]-<?Cc>>$/ ), q{Don't match compound inverted <?Cc> (Control)} );
ok(!( "\c[NULL]" ~~ m/^<+<-Cc>>$/ ), q{Don't match internally inverted <?Cc> (Control)} );
ok(!( "\c[OGONEK]"  ~~ m/^<+<?Cc>>$/ ), q{Don't match unrelated <?Cc> (Control)} );
ok("\c[OGONEK]"  ~~ m/^<-<?Cc>>$/, q{Match unrelated externally inverted <?Cc> (Control)} );
ok("\c[OGONEK]"  ~~ m/^<+<-Cc>>$/, q{Match unrelated internally inverted <?Cc> (Control)} );
ok(!( "\x[0358]" ~~ m/^<+<?Cc>>$/ ), q{Don't match related <?Cc> (Control)} );
ok("\x[0358]" ~~ m/^<+<-Cc>>$/, q{Match related internally inverted <?Cc> (Control)} );
ok("\x[0358]" ~~ m/^<-<?Cc>>$/, q{Match related externally inverted <?Cc> (Control)} );
ok("\c[OGONEK]\x[0358]\c[NULL]" ~~ m/<+<?Cc>>/, q{Match unanchored <?Cc> (Control)} );

ok("\c[NULL]" ~~ m/^<+<?Control>>$/, q{Match <?Control>} );
ok("\c[NULL]" ~~ m/^<[A]+<?Control>>$/, q{Match compound <?Control>} );
ok(!( "\c[NULL]" ~~ m/^<-<?Control>>$/ ), q{Don't match externally inverted <?Control>} );
ok(!( "\c[NULL]" ~~ m/^<[A]-<?Control>>$/ ), q{Don't match compound inverted <?Control>} );
ok(!( "\c[NULL]" ~~ m/^<+<-Control>>$/ ), q{Don't match internally inverted <?Control>} );
ok(!( "\x[4A20]"  ~~ m/^<+<?Control>>$/ ), q{Don't match unrelated <?Control>} );
ok("\x[4A20]"  ~~ m/^<-<?Control>>$/, q{Match unrelated externally inverted <?Control>} );
ok("\x[4A20]"  ~~ m/^<+<-Control>>$/, q{Match unrelated internally inverted <?Control>} );
ok(!( "\x[4DB6]" ~~ m/^<+<?Control>>$/ ), q{Don't match related <?Control>} );
ok("\x[4DB6]" ~~ m/^<+<-Control>>$/, q{Match related internally inverted <?Control>} );
ok("\x[4DB6]" ~~ m/^<-<?Control>>$/, q{Match related externally inverted <?Control>} );
ok("\x[4A20]\x[4DB6]\c[NULL]" ~~ m/<+<?Control>>/, q{Match unanchored <?Control>} );

# Cf          Format


ok("\c[SOFT HYPHEN]" ~~ m/^<+<?Cf>>$/, q{Match <?Cf> (Format)} );
ok("\c[SOFT HYPHEN]" ~~ m/^<[A]+<?Cf>>$/, q{Match compound <?Cf> (Format)} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<-<?Cf>>$/ ), q{Don't match externally inverted <?Cf> (Format)} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<[A]-<?Cf>>$/ ), q{Don't match compound inverted <?Cf> (Format)} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<+<-Cf>>$/ ), q{Don't match internally inverted <?Cf> (Format)} );
ok(!( "\x[AECE]"  ~~ m/^<+<?Cf>>$/ ), q{Don't match unrelated <?Cf> (Format)} );
ok("\x[AECE]"  ~~ m/^<-<?Cf>>$/, q{Match unrelated externally inverted <?Cf> (Format)} );
ok("\x[AECE]"  ~~ m/^<+<-Cf>>$/, q{Match unrelated internally inverted <?Cf> (Format)} );
ok(!( "\x[D7A4]" ~~ m/^<+<?Cf>>$/ ), q{Don't match related <?Cf> (Format)} );
ok("\x[D7A4]" ~~ m/^<+<-Cf>>$/, q{Match related internally inverted <?Cf> (Format)} );
ok("\x[D7A4]" ~~ m/^<-<?Cf>>$/, q{Match related externally inverted <?Cf> (Format)} );
ok("\x[AECE]\x[D7A4]\c[SOFT HYPHEN]" ~~ m/<+<?Cf>>/, q{Match unanchored <?Cf> (Format)} );

ok("\c[SOFT HYPHEN]" ~~ m/^<+<?Format>>$/, q{Match <?Format>} );
ok("\c[SOFT HYPHEN]" ~~ m/^<[A]+<?Format>>$/, q{Match compound <?Format>} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<-<?Format>>$/ ), q{Don't match externally inverted <?Format>} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<[A]-<?Format>>$/ ), q{Don't match compound inverted <?Format>} );
ok(!( "\c[SOFT HYPHEN]" ~~ m/^<+<-Format>>$/ ), q{Don't match internally inverted <?Format>} );
ok(!( "\x[5382]"  ~~ m/^<+<?Format>>$/ ), q{Don't match unrelated <?Format>} );
ok("\x[5382]"  ~~ m/^<-<?Format>>$/, q{Match unrelated externally inverted <?Format>} );
ok("\x[5382]"  ~~ m/^<+<-Format>>$/, q{Match unrelated internally inverted <?Format>} );
ok("\x[5382]\c[SOFT HYPHEN]" ~~ m/<+<?Format>>/, q{Match unanchored <?Format>} );

# BidiL       # Left-to-Right


ok("\c[YI SYLLABLE IT]" ~~ m/^<+<?BidiL>>$/, q{Match (Left-to-Right)} );
ok("\c[YI SYLLABLE IT]" ~~ m/^<[A]+<?BidiL>>$/, q{Match compound (Left-to-Right)} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<-<?BidiL>>$/ ), q{Don't match externally inverted (Left-to-Right)} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<[A]-<?BidiL>>$/ ), q{Don't match compound inverted (Left-to-Right)} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<+<-BidiL>>$/ ), q{Don't match internally inverted (Left-to-Right)} );
ok(!( "\x[5BF5]"  ~~ m/^<+<?BidiL>>$/ ), q{Don't match unrelated (Left-to-Right)} );
ok("\x[5BF5]"  ~~ m/^<-<?BidiL>>$/, q{Match unrelated externally inverted (Left-to-Right)} );
ok("\x[5BF5]"  ~~ m/^<+<-BidiL>>$/, q{Match unrelated internally inverted (Left-to-Right)} );
ok("\x[5BF5]\c[YI SYLLABLE IT]" ~~ m/<+<?BidiL>>/, q{Match unanchored (Left-to-Right)} );

# BidiEN      # European Number


ok("\c[DIGIT ZERO]" ~~ m/^<+<?BidiEN>>$/, q{Match (European Number)} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?BidiEN>>$/, q{Match compound (European Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?BidiEN>>$/ ), q{Don't match externally inverted (European Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?BidiEN>>$/ ), q{Don't match compound inverted (European Number)} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-BidiEN>>$/ ), q{Don't match internally inverted (European Number)} );
ok(!( "\x[5596]"  ~~ m/^<+<?BidiEN>>$/ ), q{Don't match unrelated (European Number)} );
ok("\x[5596]"  ~~ m/^<-<?BidiEN>>$/, q{Match unrelated externally inverted (European Number)} );
ok("\x[5596]"  ~~ m/^<+<-BidiEN>>$/, q{Match unrelated internally inverted (European Number)} );
ok("\x[5596]\c[DIGIT ZERO]" ~~ m/<+<?BidiEN>>/, q{Match unanchored (European Number)} );

# BidiES      # European Number Separator


ok("\c[SOLIDUS]" ~~ m/^<+<?BidiES>>$/, q{Match (European Number Separator)} );
ok("\c[SOLIDUS]" ~~ m/^<[A]+<?BidiES>>$/, q{Match compound (European Number Separator)} );
ok(!( "\c[SOLIDUS]" ~~ m/^<-<?BidiES>>$/ ), q{Don't match externally inverted (European Number Separator)} );
ok(!( "\c[SOLIDUS]" ~~ m/^<[A]-<?BidiES>>$/ ), q{Don't match compound inverted (European Number Separator)} );
ok(!( "\c[SOLIDUS]" ~~ m/^<+<-BidiES>>$/ ), q{Don't match internally inverted (European Number Separator)} );
ok(!( "\x[85D3]"  ~~ m/^<+<?BidiES>>$/ ), q{Don't match unrelated (European Number Separator)} );
ok("\x[85D3]"  ~~ m/^<-<?BidiES>>$/, q{Match unrelated externally inverted (European Number Separator)} );
ok("\x[85D3]"  ~~ m/^<+<-BidiES>>$/, q{Match unrelated internally inverted (European Number Separator)} );
ok("\x[85D3]\c[SOLIDUS]" ~~ m/<+<?BidiES>>/, q{Match unanchored (European Number Separator)} );

# BidiET      # European Number Terminator


ok("\c[NUMBER SIGN]" ~~ m/^<+<?BidiET>>$/, q{Match (European Number Terminator)} );
ok("\c[NUMBER SIGN]" ~~ m/^<[A]+<?BidiET>>$/, q{Match compound (European Number Terminator)} );
ok(!( "\c[NUMBER SIGN]" ~~ m/^<-<?BidiET>>$/ ), q{Don't match externally inverted (European Number Terminator)} );
ok(!( "\c[NUMBER SIGN]" ~~ m/^<[A]-<?BidiET>>$/ ), q{Don't match compound inverted (European Number Terminator)} );
ok(!( "\c[NUMBER SIGN]" ~~ m/^<+<-BidiET>>$/ ), q{Don't match internally inverted (European Number Terminator)} );
ok(!( "\x[9AFC]"  ~~ m/^<+<?BidiET>>$/ ), q{Don't match unrelated (European Number Terminator)} );
ok("\x[9AFC]"  ~~ m/^<-<?BidiET>>$/, q{Match unrelated externally inverted (European Number Terminator)} );
ok("\x[9AFC]"  ~~ m/^<+<-BidiET>>$/, q{Match unrelated internally inverted (European Number Terminator)} );
ok("\x[9AFC]\c[NUMBER SIGN]" ~~ m/<+<?BidiET>>/, q{Match unanchored (European Number Terminator)} );

# BidiWS      # Whitespace


ok("\c[FORM FEED (FF)]" ~~ m/^<+<?BidiWS>>$/, q{Match (Whitespace)} );
ok("\c[FORM FEED (FF)]" ~~ m/^<[A]+<?BidiWS>>$/, q{Match compound (Whitespace)} );
ok(!( "\c[FORM FEED (FF)]" ~~ m/^<-<?BidiWS>>$/ ), q{Don't match externally inverted (Whitespace)} );
ok(!( "\c[FORM FEED (FF)]" ~~ m/^<[A]-<?BidiWS>>$/ ), q{Don't match compound inverted (Whitespace)} );
ok(!( "\c[FORM FEED (FF)]" ~~ m/^<+<-BidiWS>>$/ ), q{Don't match internally inverted (Whitespace)} );
ok(!( "\x[4441]"  ~~ m/^<+<?BidiWS>>$/ ), q{Don't match unrelated (Whitespace)} );
ok("\x[4441]"  ~~ m/^<-<?BidiWS>>$/, q{Match unrelated externally inverted (Whitespace)} );
ok("\x[4441]"  ~~ m/^<+<-BidiWS>>$/, q{Match unrelated internally inverted (Whitespace)} );
ok("\x[4441]\c[FORM FEED (FF)]" ~~ m/<+<?BidiWS>>/, q{Match unanchored (Whitespace)} );

# Arabic


ok("\c[ARABIC LETTER HAMZA]" ~~ m/^<+<?Arabic>>$/, q{Match <?Arabic>} );
ok("\c[ARABIC LETTER HAMZA]" ~~ m/^<[A]+<?Arabic>>$/, q{Match compound <?Arabic>} );
ok(!( "\c[ARABIC LETTER HAMZA]" ~~ m/^<-<?Arabic>>$/ ), q{Don't match externally inverted <?Arabic>} );
ok(!( "\c[ARABIC LETTER HAMZA]" ~~ m/^<[A]-<?Arabic>>$/ ), q{Don't match compound inverted <?Arabic>} );
ok(!( "\c[ARABIC LETTER HAMZA]" ~~ m/^<+<-Arabic>>$/ ), q{Don't match internally inverted <?Arabic>} );
ok(!( "\c[YI SYLLABLE RYRX]"  ~~ m/^<+<?Arabic>>$/ ), q{Don't match unrelated <?Arabic>} );
ok("\c[YI SYLLABLE RYRX]"  ~~ m/^<-<?Arabic>>$/, q{Match unrelated externally inverted <?Arabic>} );
ok("\c[YI SYLLABLE RYRX]"  ~~ m/^<+<-Arabic>>$/, q{Match unrelated internally inverted <?Arabic>} );
ok("\c[YI SYLLABLE RYRX]\c[ARABIC LETTER HAMZA]" ~~ m/<+<?Arabic>>/, q{Match unanchored <?Arabic>} );

# Armenian


ok("\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<+<?Armenian>>$/, q{Match <?Armenian>} );
ok("\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<[A]+<?Armenian>>$/, q{Match compound <?Armenian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<-<?Armenian>>$/ ), q{Don't match externally inverted <?Armenian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<[A]-<?Armenian>>$/ ), q{Don't match compound inverted <?Armenian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/^<+<-Armenian>>$/ ), q{Don't match internally inverted <?Armenian>} );
ok(!( "\x[B2ED]"  ~~ m/^<+<?Armenian>>$/ ), q{Don't match unrelated <?Armenian>} );
ok("\x[B2ED]"  ~~ m/^<-<?Armenian>>$/, q{Match unrelated externally inverted <?Armenian>} );
ok("\x[B2ED]"  ~~ m/^<+<-Armenian>>$/, q{Match unrelated internally inverted <?Armenian>} );
ok("\x[B2ED]\c[ARMENIAN CAPITAL LETTER AYB]" ~~ m/<+<?Armenian>>/, q{Match unanchored <?Armenian>} );

# Bengali


ok("\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<+<?Bengali>>$/, q{Match <?Bengali>} );
ok("\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<[A]+<?Bengali>>$/, q{Match compound <?Bengali>} );
ok(!( "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<-<?Bengali>>$/ ), q{Don't match externally inverted <?Bengali>} );
ok(!( "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<[A]-<?Bengali>>$/ ), q{Don't match compound inverted <?Bengali>} );
ok(!( "\c[BENGALI SIGN CANDRABINDU]" ~~ m/^<+<-Bengali>>$/ ), q{Don't match internally inverted <?Bengali>} );
ok(!( "\x[4AFD]"  ~~ m/^<+<?Bengali>>$/ ), q{Don't match unrelated <?Bengali>} );
ok("\x[4AFD]"  ~~ m/^<-<?Bengali>>$/, q{Match unrelated externally inverted <?Bengali>} );
ok("\x[4AFD]"  ~~ m/^<+<-Bengali>>$/, q{Match unrelated internally inverted <?Bengali>} );
ok("\x[4AFD]\c[BENGALI SIGN CANDRABINDU]" ~~ m/<+<?Bengali>>/, q{Match unanchored <?Bengali>} );

# Bopomofo


ok("\c[BOPOMOFO LETTER B]" ~~ m/^<+<?Bopomofo>>$/, q{Match <?Bopomofo>} );
ok("\c[BOPOMOFO LETTER B]" ~~ m/^<[A]+<?Bopomofo>>$/, q{Match compound <?Bopomofo>} );
ok(!( "\c[BOPOMOFO LETTER B]" ~~ m/^<-<?Bopomofo>>$/ ), q{Don't match externally inverted <?Bopomofo>} );
ok(!( "\c[BOPOMOFO LETTER B]" ~~ m/^<[A]-<?Bopomofo>>$/ ), q{Don't match compound inverted <?Bopomofo>} );
ok(!( "\c[BOPOMOFO LETTER B]" ~~ m/^<+<-Bopomofo>>$/ ), q{Don't match internally inverted <?Bopomofo>} );
ok(!( "\x[8369]"  ~~ m/^<+<?Bopomofo>>$/ ), q{Don't match unrelated <?Bopomofo>} );
ok("\x[8369]"  ~~ m/^<-<?Bopomofo>>$/, q{Match unrelated externally inverted <?Bopomofo>} );
ok("\x[8369]"  ~~ m/^<+<-Bopomofo>>$/, q{Match unrelated internally inverted <?Bopomofo>} );
ok("\x[8369]\c[BOPOMOFO LETTER B]" ~~ m/<+<?Bopomofo>>/, q{Match unanchored <?Bopomofo>} );

# Buhid


ok("\c[BUHID LETTER A]" ~~ m/^<+<?Buhid>>$/, q{Match <?Buhid>} );
ok("\c[BUHID LETTER A]" ~~ m/^<[A]+<?Buhid>>$/, q{Match compound <?Buhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<-<?Buhid>>$/ ), q{Don't match externally inverted <?Buhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<[A]-<?Buhid>>$/ ), q{Don't match compound inverted <?Buhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<+<-Buhid>>$/ ), q{Don't match internally inverted <?Buhid>} );
ok(!( "\x[877F]"  ~~ m/^<+<?Buhid>>$/ ), q{Don't match unrelated <?Buhid>} );
ok("\x[877F]"  ~~ m/^<-<?Buhid>>$/, q{Match unrelated externally inverted <?Buhid>} );
ok("\x[877F]"  ~~ m/^<+<-Buhid>>$/, q{Match unrelated internally inverted <?Buhid>} );
ok("\x[877F]\c[BUHID LETTER A]" ~~ m/<+<?Buhid>>/, q{Match unanchored <?Buhid>} );

# CanadianAboriginal


ok("\c[CANADIAN SYLLABICS E]" ~~ m/^<+<?CanadianAboriginal>>$/, q{Match <?CanadianAboriginal>} );
ok("\c[CANADIAN SYLLABICS E]" ~~ m/^<[A]+<?CanadianAboriginal>>$/, q{Match compound <?CanadianAboriginal>} );
ok(!( "\c[CANADIAN SYLLABICS E]" ~~ m/^<-<?CanadianAboriginal>>$/ ), q{Don't match externally inverted <?CanadianAboriginal>} );
ok(!( "\c[CANADIAN SYLLABICS E]" ~~ m/^<[A]-<?CanadianAboriginal>>$/ ), q{Don't match compound inverted <?CanadianAboriginal>} );
ok(!( "\c[CANADIAN SYLLABICS E]" ~~ m/^<+<-CanadianAboriginal>>$/ ), q{Don't match internally inverted <?CanadianAboriginal>} );
ok(!( "\x[3A42]"  ~~ m/^<+<?CanadianAboriginal>>$/ ), q{Don't match unrelated <?CanadianAboriginal>} );
ok("\x[3A42]"  ~~ m/^<-<?CanadianAboriginal>>$/, q{Match unrelated externally inverted <?CanadianAboriginal>} );
ok("\x[3A42]"  ~~ m/^<+<-CanadianAboriginal>>$/, q{Match unrelated internally inverted <?CanadianAboriginal>} );
ok(!( "\x[4DB6]" ~~ m/^<+<?CanadianAboriginal>>$/ ), q{Don't match related <?CanadianAboriginal>} );
ok("\x[4DB6]" ~~ m/^<+<-CanadianAboriginal>>$/, q{Match related internally inverted <?CanadianAboriginal>} );
ok("\x[4DB6]" ~~ m/^<-<?CanadianAboriginal>>$/, q{Match related externally inverted <?CanadianAboriginal>} );
ok("\x[3A42]\x[4DB6]\c[CANADIAN SYLLABICS E]" ~~ m/<+<?CanadianAboriginal>>/, q{Match unanchored <?CanadianAboriginal>} );

# Cherokee


ok("\c[CHEROKEE LETTER A]" ~~ m/^<+<?Cherokee>>$/, q{Match <?Cherokee>} );
ok("\c[CHEROKEE LETTER A]" ~~ m/^<[A]+<?Cherokee>>$/, q{Match compound <?Cherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<-<?Cherokee>>$/ ), q{Don't match externally inverted <?Cherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<[A]-<?Cherokee>>$/ ), q{Don't match compound inverted <?Cherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<+<-Cherokee>>$/ ), q{Don't match internally inverted <?Cherokee>} );
ok(!( "\x[A9EF]"  ~~ m/^<+<?Cherokee>>$/ ), q{Don't match unrelated <?Cherokee>} );
ok("\x[A9EF]"  ~~ m/^<-<?Cherokee>>$/, q{Match unrelated externally inverted <?Cherokee>} );
ok("\x[A9EF]"  ~~ m/^<+<-Cherokee>>$/, q{Match unrelated internally inverted <?Cherokee>} );
ok(!( "\x[A9EF]" ~~ m/^<+<?Cherokee>>$/ ), q{Don't match related <?Cherokee>} );
ok("\x[A9EF]" ~~ m/^<+<-Cherokee>>$/, q{Match related internally inverted <?Cherokee>} );
ok("\x[A9EF]" ~~ m/^<-<?Cherokee>>$/, q{Match related externally inverted <?Cherokee>} );
ok("\x[A9EF]\x[A9EF]\c[CHEROKEE LETTER A]" ~~ m/<+<?Cherokee>>/, q{Match unanchored <?Cherokee>} );

# Cyrillic


ok("\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<+<?Cyrillic>>$/, q{Match <?Cyrillic>} );
ok("\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<[A]+<?Cyrillic>>$/, q{Match compound <?Cyrillic>} );
ok(!( "\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<-<?Cyrillic>>$/ ), q{Don't match externally inverted <?Cyrillic>} );
ok(!( "\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<[A]-<?Cyrillic>>$/ ), q{Don't match compound inverted <?Cyrillic>} );
ok(!( "\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/^<+<-Cyrillic>>$/ ), q{Don't match internally inverted <?Cyrillic>} );
ok(!( "\x[07EF]"  ~~ m/^<+<?Cyrillic>>$/ ), q{Don't match unrelated <?Cyrillic>} );
ok("\x[07EF]"  ~~ m/^<-<?Cyrillic>>$/, q{Match unrelated externally inverted <?Cyrillic>} );
ok("\x[07EF]"  ~~ m/^<+<-Cyrillic>>$/, q{Match unrelated internally inverted <?Cyrillic>} );
ok(!( "\x[07EF]" ~~ m/^<+<?Cyrillic>>$/ ), q{Don't match related <?Cyrillic>} );
ok("\x[07EF]" ~~ m/^<+<-Cyrillic>>$/, q{Match related internally inverted <?Cyrillic>} );
ok("\x[07EF]" ~~ m/^<-<?Cyrillic>>$/, q{Match related externally inverted <?Cyrillic>} );
ok("\x[07EF]\x[07EF]\c[CYRILLIC LETTER SMALL CAPITAL EL]" ~~ m/<+<?Cyrillic>>/, q{Match unanchored <?Cyrillic>} );

# Deseret


ok(!( "\x[65BD]"  ~~ m/^<+<?Deseret>>$/ ), q{Don't match unrelated <?Deseret>} );
ok("\x[65BD]"  ~~ m/^<-<?Deseret>>$/, q{Match unrelated externally inverted <?Deseret>} );
ok("\x[65BD]"  ~~ m/^<+<-Deseret>>$/, q{Match unrelated internally inverted <?Deseret>} );

# Devanagari


ok("\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<+<?Devanagari>>$/, q{Match <?Devanagari>} );
ok("\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<[A]+<?Devanagari>>$/, q{Match compound <?Devanagari>} );
ok(!( "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<-<?Devanagari>>$/ ), q{Don't match externally inverted <?Devanagari>} );
ok(!( "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<[A]-<?Devanagari>>$/ ), q{Don't match compound inverted <?Devanagari>} );
ok(!( "\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/^<+<-Devanagari>>$/ ), q{Don't match internally inverted <?Devanagari>} );
ok(!( "\x[653B]"  ~~ m/^<+<?Devanagari>>$/ ), q{Don't match unrelated <?Devanagari>} );
ok("\x[653B]"  ~~ m/^<-<?Devanagari>>$/, q{Match unrelated externally inverted <?Devanagari>} );
ok("\x[653B]"  ~~ m/^<+<-Devanagari>>$/, q{Match unrelated internally inverted <?Devanagari>} );
ok("\x[653B]\c[DEVANAGARI SIGN CANDRABINDU]" ~~ m/<+<?Devanagari>>/, q{Match unanchored <?Devanagari>} );

# Ethiopic


ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<?Ethiopic>>$/, q{Match <?Ethiopic>} );
ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]+<?Ethiopic>>$/, q{Match compound <?Ethiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<-<?Ethiopic>>$/ ), q{Don't match externally inverted <?Ethiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]-<?Ethiopic>>$/ ), q{Don't match compound inverted <?Ethiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<-Ethiopic>>$/ ), q{Don't match internally inverted <?Ethiopic>} );
ok(!( "\x[482C]"  ~~ m/^<+<?Ethiopic>>$/ ), q{Don't match unrelated <?Ethiopic>} );
ok("\x[482C]"  ~~ m/^<-<?Ethiopic>>$/, q{Match unrelated externally inverted <?Ethiopic>} );
ok("\x[482C]"  ~~ m/^<+<-Ethiopic>>$/, q{Match unrelated internally inverted <?Ethiopic>} );
ok("\x[482C]\c[ETHIOPIC SYLLABLE HA]" ~~ m/<+<?Ethiopic>>/, q{Match unanchored <?Ethiopic>} );

# Georgian


ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<?Georgian>>$/, q{Match <?Georgian>} );
ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]+<?Georgian>>$/, q{Match compound <?Georgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<-<?Georgian>>$/ ), q{Don't match externally inverted <?Georgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]-<?Georgian>>$/ ), q{Don't match compound inverted <?Georgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<-Georgian>>$/ ), q{Don't match internally inverted <?Georgian>} );
ok(!( "\x[9BE5]"  ~~ m/^<+<?Georgian>>$/ ), q{Don't match unrelated <?Georgian>} );
ok("\x[9BE5]"  ~~ m/^<-<?Georgian>>$/, q{Match unrelated externally inverted <?Georgian>} );
ok("\x[9BE5]"  ~~ m/^<+<-Georgian>>$/, q{Match unrelated internally inverted <?Georgian>} );
ok("\x[9BE5]\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/<+<?Georgian>>/, q{Match unanchored <?Georgian>} );

# Gothic


ok(!( "\x[4ED2]"  ~~ m/^<+<?Gothic>>$/ ), q{Don't match unrelated <?Gothic>} );
ok("\x[4ED2]"  ~~ m/^<-<?Gothic>>$/, q{Match unrelated externally inverted <?Gothic>} );
ok("\x[4ED2]"  ~~ m/^<+<-Gothic>>$/, q{Match unrelated internally inverted <?Gothic>} );

# Greek


ok("\c[MICRO SIGN]" ~~ m/^<+<?Greek>>$/, q{Match <?Greek>} );
ok("\c[MICRO SIGN]" ~~ m/^<[A]+<?Greek>>$/, q{Match compound <?Greek>} );
ok(!( "\c[MICRO SIGN]" ~~ m/^<-<?Greek>>$/ ), q{Don't match externally inverted <?Greek>} );
ok(!( "\c[MICRO SIGN]" ~~ m/^<[A]-<?Greek>>$/ ), q{Don't match compound inverted <?Greek>} );
ok(!( "\c[MICRO SIGN]" ~~ m/^<+<-Greek>>$/ ), q{Don't match internally inverted <?Greek>} );
ok(!( "\x[D486]"  ~~ m/^<+<?Greek>>$/ ), q{Don't match unrelated <?Greek>} );
ok("\x[D486]"  ~~ m/^<-<?Greek>>$/, q{Match unrelated externally inverted <?Greek>} );
ok("\x[D486]"  ~~ m/^<+<-Greek>>$/, q{Match unrelated internally inverted <?Greek>} );
ok("\x[D486]\c[MICRO SIGN]" ~~ m/<+<?Greek>>/, q{Match unanchored <?Greek>} );

# Gujarati


ok("\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<+<?Gujarati>>$/, q{Match <?Gujarati>} );
ok("\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<[A]+<?Gujarati>>$/, q{Match compound <?Gujarati>} );
ok(!( "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<-<?Gujarati>>$/ ), q{Don't match externally inverted <?Gujarati>} );
ok(!( "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<[A]-<?Gujarati>>$/ ), q{Don't match compound inverted <?Gujarati>} );
ok(!( "\c[GUJARATI SIGN CANDRABINDU]" ~~ m/^<+<-Gujarati>>$/ ), q{Don't match internally inverted <?Gujarati>} );
ok(!( "\x[B3F3]"  ~~ m/^<+<?Gujarati>>$/ ), q{Don't match unrelated <?Gujarati>} );
ok("\x[B3F3]"  ~~ m/^<-<?Gujarati>>$/, q{Match unrelated externally inverted <?Gujarati>} );
ok("\x[B3F3]"  ~~ m/^<+<-Gujarati>>$/, q{Match unrelated internally inverted <?Gujarati>} );
ok("\x[B3F3]\c[GUJARATI SIGN CANDRABINDU]" ~~ m/<+<?Gujarati>>/, q{Match unanchored <?Gujarati>} );

# Gurmukhi


ok("\c[GURMUKHI SIGN BINDI]" ~~ m/^<+<?Gurmukhi>>$/, q{Match <?Gurmukhi>} );
ok("\c[GURMUKHI SIGN BINDI]" ~~ m/^<[A]+<?Gurmukhi>>$/, q{Match compound <?Gurmukhi>} );
ok(!( "\c[GURMUKHI SIGN BINDI]" ~~ m/^<-<?Gurmukhi>>$/ ), q{Don't match externally inverted <?Gurmukhi>} );
ok(!( "\c[GURMUKHI SIGN BINDI]" ~~ m/^<[A]-<?Gurmukhi>>$/ ), q{Don't match compound inverted <?Gurmukhi>} );
ok(!( "\c[GURMUKHI SIGN BINDI]" ~~ m/^<+<-Gurmukhi>>$/ ), q{Don't match internally inverted <?Gurmukhi>} );
ok(!( "\x[6469]"  ~~ m/^<+<?Gurmukhi>>$/ ), q{Don't match unrelated <?Gurmukhi>} );
ok("\x[6469]"  ~~ m/^<-<?Gurmukhi>>$/, q{Match unrelated externally inverted <?Gurmukhi>} );
ok("\x[6469]"  ~~ m/^<+<-Gurmukhi>>$/, q{Match unrelated internally inverted <?Gurmukhi>} );
ok("\x[6469]\c[GURMUKHI SIGN BINDI]" ~~ m/<+<?Gurmukhi>>/, q{Match unanchored <?Gurmukhi>} );

# Han


ok("\x[9DB5]" ~~ m/^<+<?Han>>$/, q{Match <?Han>} );
ok("\x[9DB5]" ~~ m/^<[A]+<?Han>>$/, q{Match compound <?Han>} );
ok(!( "\x[9DB5]" ~~ m/^<-<?Han>>$/ ), q{Don't match externally inverted <?Han>} );
ok(!( "\x[9DB5]" ~~ m/^<[A]-<?Han>>$/ ), q{Don't match compound inverted <?Han>} );
ok(!( "\x[9DB5]" ~~ m/^<+<-Han>>$/ ), q{Don't match internally inverted <?Han>} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?Han>>$/ ), q{Don't match unrelated <?Han>} );
ok("\x[9FA6]"  ~~ m/^<-<?Han>>$/, q{Match unrelated externally inverted <?Han>} );
ok("\x[9FA6]"  ~~ m/^<+<-Han>>$/, q{Match unrelated internally inverted <?Han>} );
ok("\x[9FA6]\x[9DB5]" ~~ m/<+<?Han>>/, q{Match unanchored <?Han>} );

# Hangul


ok("\x[AC00]" ~~ m/^<+<?Hangul>>$/, q{Match <?Hangul>} );
ok("\x[AC00]" ~~ m/^<[A]+<?Hangul>>$/, q{Match compound <?Hangul>} );
ok(!( "\x[AC00]" ~~ m/^<-<?Hangul>>$/ ), q{Don't match externally inverted <?Hangul>} );
ok(!( "\x[AC00]" ~~ m/^<[A]-<?Hangul>>$/ ), q{Don't match compound inverted <?Hangul>} );
ok(!( "\x[AC00]" ~~ m/^<+<-Hangul>>$/ ), q{Don't match internally inverted <?Hangul>} );
ok(!( "\x[9E09]"  ~~ m/^<+<?Hangul>>$/ ), q{Don't match unrelated <?Hangul>} );
ok("\x[9E09]"  ~~ m/^<-<?Hangul>>$/, q{Match unrelated externally inverted <?Hangul>} );
ok("\x[9E09]"  ~~ m/^<+<-Hangul>>$/, q{Match unrelated internally inverted <?Hangul>} );
ok("\x[9E09]\x[AC00]" ~~ m/<+<?Hangul>>/, q{Match unanchored <?Hangul>} );

# Hanunoo


ok("\c[HANUNOO LETTER A]" ~~ m/^<+<?Hanunoo>>$/, q{Match <?Hanunoo>} );
ok("\c[HANUNOO LETTER A]" ~~ m/^<[A]+<?Hanunoo>>$/, q{Match compound <?Hanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<-<?Hanunoo>>$/ ), q{Don't match externally inverted <?Hanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<[A]-<?Hanunoo>>$/ ), q{Don't match compound inverted <?Hanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<+<-Hanunoo>>$/ ), q{Don't match internally inverted <?Hanunoo>} );
ok(!( "\x[580B]"  ~~ m/^<+<?Hanunoo>>$/ ), q{Don't match unrelated <?Hanunoo>} );
ok("\x[580B]"  ~~ m/^<-<?Hanunoo>>$/, q{Match unrelated externally inverted <?Hanunoo>} );
ok("\x[580B]"  ~~ m/^<+<-Hanunoo>>$/, q{Match unrelated internally inverted <?Hanunoo>} );
ok("\x[580B]\c[HANUNOO LETTER A]" ~~ m/<+<?Hanunoo>>/, q{Match unanchored <?Hanunoo>} );

# Hebrew


ok("\c[HEBREW LETTER ALEF]" ~~ m/^<+<?Hebrew>>$/, q{Match <?Hebrew>} );
ok("\c[HEBREW LETTER ALEF]" ~~ m/^<[A]+<?Hebrew>>$/, q{Match compound <?Hebrew>} );
ok(!( "\c[HEBREW LETTER ALEF]" ~~ m/^<-<?Hebrew>>$/ ), q{Don't match externally inverted <?Hebrew>} );
ok(!( "\c[HEBREW LETTER ALEF]" ~~ m/^<[A]-<?Hebrew>>$/ ), q{Don't match compound inverted <?Hebrew>} );
ok(!( "\c[HEBREW LETTER ALEF]" ~~ m/^<+<-Hebrew>>$/ ), q{Don't match internally inverted <?Hebrew>} );
ok(!( "\x[62B4]"  ~~ m/^<+<?Hebrew>>$/ ), q{Don't match unrelated <?Hebrew>} );
ok("\x[62B4]"  ~~ m/^<-<?Hebrew>>$/, q{Match unrelated externally inverted <?Hebrew>} );
ok("\x[62B4]"  ~~ m/^<+<-Hebrew>>$/, q{Match unrelated internally inverted <?Hebrew>} );
ok("\x[62B4]\c[HEBREW LETTER ALEF]" ~~ m/<+<?Hebrew>>/, q{Match unanchored <?Hebrew>} );

# Hiragana


ok("\c[HIRAGANA LETTER SMALL A]" ~~ m/^<+<?Hiragana>>$/, q{Match <?Hiragana>} );
ok("\c[HIRAGANA LETTER SMALL A]" ~~ m/^<[A]+<?Hiragana>>$/, q{Match compound <?Hiragana>} );
ok(!( "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<-<?Hiragana>>$/ ), q{Don't match externally inverted <?Hiragana>} );
ok(!( "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<[A]-<?Hiragana>>$/ ), q{Don't match compound inverted <?Hiragana>} );
ok(!( "\c[HIRAGANA LETTER SMALL A]" ~~ m/^<+<-Hiragana>>$/ ), q{Don't match internally inverted <?Hiragana>} );
ok(!( "\x[9504]"  ~~ m/^<+<?Hiragana>>$/ ), q{Don't match unrelated <?Hiragana>} );
ok("\x[9504]"  ~~ m/^<-<?Hiragana>>$/, q{Match unrelated externally inverted <?Hiragana>} );
ok("\x[9504]"  ~~ m/^<+<-Hiragana>>$/, q{Match unrelated internally inverted <?Hiragana>} );
ok("\x[9504]\c[HIRAGANA LETTER SMALL A]" ~~ m/<+<?Hiragana>>/, q{Match unanchored <?Hiragana>} );

# Inherited


ok("\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<+<?Inherited>>$/, q{Match <?Inherited>} );
ok("\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<[A]+<?Inherited>>$/, q{Match compound <?Inherited>} );
ok(!( "\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<-<?Inherited>>$/ ), q{Don't match externally inverted <?Inherited>} );
ok(!( "\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<[A]-<?Inherited>>$/ ), q{Don't match compound inverted <?Inherited>} );
ok(!( "\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/^<+<-Inherited>>$/ ), q{Don't match internally inverted <?Inherited>} );
ok(!( "\c[TAMIL LETTER RRA]"  ~~ m/^<+<?Inherited>>$/ ), q{Don't match unrelated <?Inherited>} );
ok("\c[TAMIL LETTER RRA]"  ~~ m/^<-<?Inherited>>$/, q{Match unrelated externally inverted <?Inherited>} );
ok("\c[TAMIL LETTER RRA]"  ~~ m/^<+<-Inherited>>$/, q{Match unrelated internally inverted <?Inherited>} );
ok("\c[TAMIL LETTER RRA]\c[MONGOLIAN FREE VARIATION SELECTOR ONE]" ~~ m/<+<?Inherited>>/, q{Match unanchored <?Inherited>} );

# Kannada


ok("\c[KANNADA SIGN ANUSVARA]" ~~ m/^<+<?Kannada>>$/, q{Match <?Kannada>} );
ok("\c[KANNADA SIGN ANUSVARA]" ~~ m/^<[A]+<?Kannada>>$/, q{Match compound <?Kannada>} );
ok(!( "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<-<?Kannada>>$/ ), q{Don't match externally inverted <?Kannada>} );
ok(!( "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<[A]-<?Kannada>>$/ ), q{Don't match compound inverted <?Kannada>} );
ok(!( "\c[KANNADA SIGN ANUSVARA]" ~~ m/^<+<-Kannada>>$/ ), q{Don't match internally inverted <?Kannada>} );
ok(!( "\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<+<?Kannada>>$/ ), q{Don't match unrelated <?Kannada>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<-<?Kannada>>$/, q{Match unrelated externally inverted <?Kannada>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]"  ~~ m/^<+<-Kannada>>$/, q{Match unrelated internally inverted <?Kannada>} );
ok("\c[BLACK RIGHT-POINTING SMALL TRIANGLE]\c[KANNADA SIGN ANUSVARA]" ~~ m/<+<?Kannada>>/, q{Match unanchored <?Kannada>} );

# Katakana


ok("\c[KATAKANA LETTER SMALL A]" ~~ m/^<+<?Katakana>>$/, q{Match <?Katakana>} );
ok("\c[KATAKANA LETTER SMALL A]" ~~ m/^<[A]+<?Katakana>>$/, q{Match compound <?Katakana>} );
ok(!( "\c[KATAKANA LETTER SMALL A]" ~~ m/^<-<?Katakana>>$/ ), q{Don't match externally inverted <?Katakana>} );
ok(!( "\c[KATAKANA LETTER SMALL A]" ~~ m/^<[A]-<?Katakana>>$/ ), q{Don't match compound inverted <?Katakana>} );
ok(!( "\c[KATAKANA LETTER SMALL A]" ~~ m/^<+<-Katakana>>$/ ), q{Don't match internally inverted <?Katakana>} );
ok(!( "\x[40DB]"  ~~ m/^<+<?Katakana>>$/ ), q{Don't match unrelated <?Katakana>} );
ok("\x[40DB]"  ~~ m/^<-<?Katakana>>$/, q{Match unrelated externally inverted <?Katakana>} );
ok("\x[40DB]"  ~~ m/^<+<-Katakana>>$/, q{Match unrelated internally inverted <?Katakana>} );
ok("\x[40DB]\c[KATAKANA LETTER SMALL A]" ~~ m/<+<?Katakana>>/, q{Match unanchored <?Katakana>} );

# Khmer


ok("\c[KHMER LETTER KA]" ~~ m/^<+<?Khmer>>$/, q{Match <?Khmer>} );
ok("\c[KHMER LETTER KA]" ~~ m/^<[A]+<?Khmer>>$/, q{Match compound <?Khmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<-<?Khmer>>$/ ), q{Don't match externally inverted <?Khmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<[A]-<?Khmer>>$/ ), q{Don't match compound inverted <?Khmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<+<-Khmer>>$/ ), q{Don't match internally inverted <?Khmer>} );
ok(!( "\x[AC3E]"  ~~ m/^<+<?Khmer>>$/ ), q{Don't match unrelated <?Khmer>} );
ok("\x[AC3E]"  ~~ m/^<-<?Khmer>>$/, q{Match unrelated externally inverted <?Khmer>} );
ok("\x[AC3E]"  ~~ m/^<+<-Khmer>>$/, q{Match unrelated internally inverted <?Khmer>} );
ok("\x[AC3E]\c[KHMER LETTER KA]" ~~ m/<+<?Khmer>>/, q{Match unanchored <?Khmer>} );

# Lao


ok("\c[LAO LETTER KO]" ~~ m/^<+<?Lao>>$/, q{Match <?Lao>} );
ok("\c[LAO LETTER KO]" ~~ m/^<[A]+<?Lao>>$/, q{Match compound <?Lao>} );
ok(!( "\c[LAO LETTER KO]" ~~ m/^<-<?Lao>>$/ ), q{Don't match externally inverted <?Lao>} );
ok(!( "\c[LAO LETTER KO]" ~~ m/^<[A]-<?Lao>>$/ ), q{Don't match compound inverted <?Lao>} );
ok(!( "\c[LAO LETTER KO]" ~~ m/^<+<-Lao>>$/ ), q{Don't match internally inverted <?Lao>} );
ok(!( "\c[MODIFIER LETTER UNASPIRATED]"  ~~ m/^<+<?Lao>>$/ ), q{Don't match unrelated <?Lao>} );
ok("\c[MODIFIER LETTER UNASPIRATED]"  ~~ m/^<-<?Lao>>$/, q{Match unrelated externally inverted <?Lao>} );
ok("\c[MODIFIER LETTER UNASPIRATED]"  ~~ m/^<+<-Lao>>$/, q{Match unrelated internally inverted <?Lao>} );
ok(!( "\c[MODIFIER LETTER DOUBLE APOSTROPHE]" ~~ m/^<+<?Lao>>$/ ), q{Don't match related <?Lao>} );
ok("\c[MODIFIER LETTER DOUBLE APOSTROPHE]" ~~ m/^<+<-Lao>>$/, q{Match related internally inverted <?Lao>} );
ok("\c[MODIFIER LETTER DOUBLE APOSTROPHE]" ~~ m/^<-<?Lao>>$/, q{Match related externally inverted <?Lao>} );
ok("\c[MODIFIER LETTER UNASPIRATED]\c[MODIFIER LETTER DOUBLE APOSTROPHE]\c[LAO LETTER KO]" ~~ m/<+<?Lao>>/, q{Match unanchored <?Lao>} );

# Latin


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?Latin>>$/, q{Match <?Latin>} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?Latin>>$/, q{Match compound <?Latin>} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?Latin>>$/ ), q{Don't match externally inverted <?Latin>} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?Latin>>$/ ), q{Don't match compound inverted <?Latin>} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-Latin>>$/ ), q{Don't match internally inverted <?Latin>} );
ok(!( "\x[6B4C]"  ~~ m/^<+<?Latin>>$/ ), q{Don't match unrelated <?Latin>} );
ok("\x[6B4C]"  ~~ m/^<-<?Latin>>$/, q{Match unrelated externally inverted <?Latin>} );
ok("\x[6B4C]"  ~~ m/^<+<-Latin>>$/, q{Match unrelated internally inverted <?Latin>} );
ok(!( "\x[6B4C]" ~~ m/^<+<?Latin>>$/ ), q{Don't match related <?Latin>} );
ok("\x[6B4C]" ~~ m/^<+<-Latin>>$/, q{Match related internally inverted <?Latin>} );
ok("\x[6B4C]" ~~ m/^<-<?Latin>>$/, q{Match related externally inverted <?Latin>} );
ok("\x[6B4C]\x[6B4C]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?Latin>>/, q{Match unanchored <?Latin>} );

# Malayalam


ok("\c[MALAYALAM LETTER TA]" ~~ m/^<+<?Malayalam>>$/, q{Match <?Malayalam>} );
ok("\c[MALAYALAM LETTER TA]" ~~ m/^<[A]+<?Malayalam>>$/, q{Match compound <?Malayalam>} );
ok(!( "\c[MALAYALAM LETTER TA]" ~~ m/^<-<?Malayalam>>$/ ), q{Don't match externally inverted <?Malayalam>} );
ok(!( "\c[MALAYALAM LETTER TA]" ~~ m/^<[A]-<?Malayalam>>$/ ), q{Don't match compound inverted <?Malayalam>} );
ok(!( "\c[MALAYALAM LETTER TA]" ~~ m/^<+<-Malayalam>>$/ ), q{Don't match internally inverted <?Malayalam>} );
ok(!( "\x[0D29]"  ~~ m/^<+<?Malayalam>>$/ ), q{Don't match unrelated <?Malayalam>} );
ok("\x[0D29]"  ~~ m/^<-<?Malayalam>>$/, q{Match unrelated externally inverted <?Malayalam>} );
ok("\x[0D29]"  ~~ m/^<+<-Malayalam>>$/, q{Match unrelated internally inverted <?Malayalam>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<?Malayalam>>$/ ), q{Don't match related <?Malayalam>} );
ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<-Malayalam>>$/, q{Match related internally inverted <?Malayalam>} );
ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<-<?Malayalam>>$/, q{Match related externally inverted <?Malayalam>} );
ok("\x[0D29]\c[SINHALA SIGN ANUSVARAYA]\c[MALAYALAM LETTER TA]" ~~ m/<+<?Malayalam>>/, q{Match unanchored <?Malayalam>} );

# Mongolian


ok("\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<+<?Mongolian>>$/, q{Match <?Mongolian>} );
ok("\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<[A]+<?Mongolian>>$/, q{Match compound <?Mongolian>} );
ok(!( "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<-<?Mongolian>>$/ ), q{Don't match externally inverted <?Mongolian>} );
ok(!( "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<[A]-<?Mongolian>>$/ ), q{Don't match compound inverted <?Mongolian>} );
ok(!( "\c[MONGOLIAN DIGIT ZERO]" ~~ m/^<+<-Mongolian>>$/ ), q{Don't match internally inverted <?Mongolian>} );
ok(!( "\x[70C0]"  ~~ m/^<+<?Mongolian>>$/ ), q{Don't match unrelated <?Mongolian>} );
ok("\x[70C0]"  ~~ m/^<-<?Mongolian>>$/, q{Match unrelated externally inverted <?Mongolian>} );
ok("\x[70C0]"  ~~ m/^<+<-Mongolian>>$/, q{Match unrelated internally inverted <?Mongolian>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?Mongolian>>$/ ), q{Don't match related <?Mongolian>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-Mongolian>>$/, q{Match related internally inverted <?Mongolian>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?Mongolian>>$/, q{Match related externally inverted <?Mongolian>} );
ok("\x[70C0]\c[COMBINING GRAVE ACCENT]\c[MONGOLIAN DIGIT ZERO]" ~~ m/<+<?Mongolian>>/, q{Match unanchored <?Mongolian>} );

# Myanmar


ok("\c[MYANMAR LETTER KA]" ~~ m/^<+<?Myanmar>>$/, q{Match <?Myanmar>} );
ok("\c[MYANMAR LETTER KA]" ~~ m/^<[A]+<?Myanmar>>$/, q{Match compound <?Myanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<-<?Myanmar>>$/ ), q{Don't match externally inverted <?Myanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<[A]-<?Myanmar>>$/ ), q{Don't match compound inverted <?Myanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<+<-Myanmar>>$/ ), q{Don't match internally inverted <?Myanmar>} );
ok(!( "\x[3CE3]"  ~~ m/^<+<?Myanmar>>$/ ), q{Don't match unrelated <?Myanmar>} );
ok("\x[3CE3]"  ~~ m/^<-<?Myanmar>>$/, q{Match unrelated externally inverted <?Myanmar>} );
ok("\x[3CE3]"  ~~ m/^<+<-Myanmar>>$/, q{Match unrelated internally inverted <?Myanmar>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?Myanmar>>$/ ), q{Don't match related <?Myanmar>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-Myanmar>>$/, q{Match related internally inverted <?Myanmar>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?Myanmar>>$/, q{Match related externally inverted <?Myanmar>} );
ok("\x[3CE3]\c[COMBINING GRAVE ACCENT]\c[MYANMAR LETTER KA]" ~~ m/<+<?Myanmar>>/, q{Match unanchored <?Myanmar>} );

# Ogham


ok("\c[OGHAM LETTER BEITH]" ~~ m/^<+<?Ogham>>$/, q{Match <?Ogham>} );
ok("\c[OGHAM LETTER BEITH]" ~~ m/^<[A]+<?Ogham>>$/, q{Match compound <?Ogham>} );
ok(!( "\c[OGHAM LETTER BEITH]" ~~ m/^<-<?Ogham>>$/ ), q{Don't match externally inverted <?Ogham>} );
ok(!( "\c[OGHAM LETTER BEITH]" ~~ m/^<[A]-<?Ogham>>$/ ), q{Don't match compound inverted <?Ogham>} );
ok(!( "\c[OGHAM LETTER BEITH]" ~~ m/^<+<-Ogham>>$/ ), q{Don't match internally inverted <?Ogham>} );
ok(!( "\x[077B]"  ~~ m/^<+<?Ogham>>$/ ), q{Don't match unrelated <?Ogham>} );
ok("\x[077B]"  ~~ m/^<-<?Ogham>>$/, q{Match unrelated externally inverted <?Ogham>} );
ok("\x[077B]"  ~~ m/^<+<-Ogham>>$/, q{Match unrelated internally inverted <?Ogham>} );
ok("\x[077B]\c[OGHAM LETTER BEITH]" ~~ m/<+<?Ogham>>/, q{Match unanchored <?Ogham>} );

# OldItalic


ok(!( "\x[562B]"  ~~ m/^<+<?OldItalic>>$/ ), q{Don't match unrelated <?OldItalic>} );
ok("\x[562B]"  ~~ m/^<-<?OldItalic>>$/, q{Match unrelated externally inverted <?OldItalic>} );
ok("\x[562B]"  ~~ m/^<+<-OldItalic>>$/, q{Match unrelated internally inverted <?OldItalic>} );

# Oriya


ok("\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<+<?Oriya>>$/, q{Match <?Oriya>} );
ok("\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<[A]+<?Oriya>>$/, q{Match compound <?Oriya>} );
ok(!( "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<-<?Oriya>>$/ ), q{Don't match externally inverted <?Oriya>} );
ok(!( "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<[A]-<?Oriya>>$/ ), q{Don't match compound inverted <?Oriya>} );
ok(!( "\c[ORIYA SIGN CANDRABINDU]" ~~ m/^<+<-Oriya>>$/ ), q{Don't match internally inverted <?Oriya>} );
ok(!( "\x[3CE7]"  ~~ m/^<+<?Oriya>>$/ ), q{Don't match unrelated <?Oriya>} );
ok("\x[3CE7]"  ~~ m/^<-<?Oriya>>$/, q{Match unrelated externally inverted <?Oriya>} );
ok("\x[3CE7]"  ~~ m/^<+<-Oriya>>$/, q{Match unrelated internally inverted <?Oriya>} );
ok("\x[3CE7]\c[ORIYA SIGN CANDRABINDU]" ~~ m/<+<?Oriya>>/, q{Match unanchored <?Oriya>} );

# Runic


ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<?Runic>>$/, q{Match <?Runic>} );
ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]+<?Runic>>$/, q{Match compound <?Runic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<-<?Runic>>$/ ), q{Don't match externally inverted <?Runic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]-<?Runic>>$/ ), q{Don't match compound inverted <?Runic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<-Runic>>$/ ), q{Don't match internally inverted <?Runic>} );
ok(!( "\c[BLACK HEART SUIT]"  ~~ m/^<+<?Runic>>$/ ), q{Don't match unrelated <?Runic>} );
ok("\c[BLACK HEART SUIT]"  ~~ m/^<-<?Runic>>$/, q{Match unrelated externally inverted <?Runic>} );
ok("\c[BLACK HEART SUIT]"  ~~ m/^<+<-Runic>>$/, q{Match unrelated internally inverted <?Runic>} );
ok("\c[BLACK HEART SUIT]\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/<+<?Runic>>/, q{Match unanchored <?Runic>} );

# Sinhala


ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<?Sinhala>>$/, q{Match <?Sinhala>} );
ok("\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<[A]+<?Sinhala>>$/, q{Match compound <?Sinhala>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<-<?Sinhala>>$/ ), q{Don't match externally inverted <?Sinhala>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<[A]-<?Sinhala>>$/ ), q{Don't match compound inverted <?Sinhala>} );
ok(!( "\c[SINHALA SIGN ANUSVARAYA]" ~~ m/^<+<-Sinhala>>$/ ), q{Don't match internally inverted <?Sinhala>} );
ok(!( "\c[MYANMAR VOWEL SIGN II]"  ~~ m/^<+<?Sinhala>>$/ ), q{Don't match unrelated <?Sinhala>} );
ok("\c[MYANMAR VOWEL SIGN II]"  ~~ m/^<-<?Sinhala>>$/, q{Match unrelated externally inverted <?Sinhala>} );
ok("\c[MYANMAR VOWEL SIGN II]"  ~~ m/^<+<-Sinhala>>$/, q{Match unrelated internally inverted <?Sinhala>} );
ok(!( "\c[KHMER CURRENCY SYMBOL RIEL]" ~~ m/^<+<?Sinhala>>$/ ), q{Don't match related <?Sinhala>} );
ok("\c[KHMER CURRENCY SYMBOL RIEL]" ~~ m/^<+<-Sinhala>>$/, q{Match related internally inverted <?Sinhala>} );
ok("\c[KHMER CURRENCY SYMBOL RIEL]" ~~ m/^<-<?Sinhala>>$/, q{Match related externally inverted <?Sinhala>} );
ok("\c[MYANMAR VOWEL SIGN II]\c[KHMER CURRENCY SYMBOL RIEL]\c[SINHALA SIGN ANUSVARAYA]" ~~ m/<+<?Sinhala>>/, q{Match unanchored <?Sinhala>} );

# Syriac


ok("\c[SYRIAC LETTER ALAPH]" ~~ m/^<+<?Syriac>>$/, q{Match <?Syriac>} );
ok("\c[SYRIAC LETTER ALAPH]" ~~ m/^<[A]+<?Syriac>>$/, q{Match compound <?Syriac>} );
ok(!( "\c[SYRIAC LETTER ALAPH]" ~~ m/^<-<?Syriac>>$/ ), q{Don't match externally inverted <?Syriac>} );
ok(!( "\c[SYRIAC LETTER ALAPH]" ~~ m/^<[A]-<?Syriac>>$/ ), q{Don't match compound inverted <?Syriac>} );
ok(!( "\c[SYRIAC LETTER ALAPH]" ~~ m/^<+<-Syriac>>$/ ), q{Don't match internally inverted <?Syriac>} );
ok(!( "\x[7BAA]"  ~~ m/^<+<?Syriac>>$/ ), q{Don't match unrelated <?Syriac>} );
ok("\x[7BAA]"  ~~ m/^<-<?Syriac>>$/, q{Match unrelated externally inverted <?Syriac>} );
ok("\x[7BAA]"  ~~ m/^<+<-Syriac>>$/, q{Match unrelated internally inverted <?Syriac>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<?Syriac>>$/ ), q{Don't match related <?Syriac>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<+<-Syriac>>$/, q{Match related internally inverted <?Syriac>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<-<?Syriac>>$/, q{Match related externally inverted <?Syriac>} );
ok("\x[7BAA]\c[YI RADICAL QOT]\c[SYRIAC LETTER ALAPH]" ~~ m/<+<?Syriac>>/, q{Match unanchored <?Syriac>} );

# Tagalog


ok("\c[TAGALOG LETTER A]" ~~ m/^<+<?Tagalog>>$/, q{Match <?Tagalog>} );
ok("\c[TAGALOG LETTER A]" ~~ m/^<[A]+<?Tagalog>>$/, q{Match compound <?Tagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<-<?Tagalog>>$/ ), q{Don't match externally inverted <?Tagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<[A]-<?Tagalog>>$/ ), q{Don't match compound inverted <?Tagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<+<-Tagalog>>$/ ), q{Don't match internally inverted <?Tagalog>} );
ok(!( "\x[50F2]"  ~~ m/^<+<?Tagalog>>$/ ), q{Don't match unrelated <?Tagalog>} );
ok("\x[50F2]"  ~~ m/^<-<?Tagalog>>$/, q{Match unrelated externally inverted <?Tagalog>} );
ok("\x[50F2]"  ~~ m/^<+<-Tagalog>>$/, q{Match unrelated internally inverted <?Tagalog>} );
ok("\x[50F2]\c[TAGALOG LETTER A]" ~~ m/<+<?Tagalog>>/, q{Match unanchored <?Tagalog>} );

# Tagbanwa


ok("\c[TAGBANWA LETTER A]" ~~ m/^<+<?Tagbanwa>>$/, q{Match <?Tagbanwa>} );
ok("\c[TAGBANWA LETTER A]" ~~ m/^<[A]+<?Tagbanwa>>$/, q{Match compound <?Tagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<-<?Tagbanwa>>$/ ), q{Don't match externally inverted <?Tagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<[A]-<?Tagbanwa>>$/ ), q{Don't match compound inverted <?Tagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<+<-Tagbanwa>>$/ ), q{Don't match internally inverted <?Tagbanwa>} );
ok(!( "\x[8843]"  ~~ m/^<+<?Tagbanwa>>$/ ), q{Don't match unrelated <?Tagbanwa>} );
ok("\x[8843]"  ~~ m/^<-<?Tagbanwa>>$/, q{Match unrelated externally inverted <?Tagbanwa>} );
ok("\x[8843]"  ~~ m/^<+<-Tagbanwa>>$/, q{Match unrelated internally inverted <?Tagbanwa>} );
ok("\x[8843]\c[TAGBANWA LETTER A]" ~~ m/<+<?Tagbanwa>>/, q{Match unanchored <?Tagbanwa>} );

# Tamil


ok("\c[TAMIL SIGN ANUSVARA]" ~~ m/^<+<?Tamil>>$/, q{Match <?Tamil>} );
ok("\c[TAMIL SIGN ANUSVARA]" ~~ m/^<[A]+<?Tamil>>$/, q{Match compound <?Tamil>} );
ok(!( "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<-<?Tamil>>$/ ), q{Don't match externally inverted <?Tamil>} );
ok(!( "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<[A]-<?Tamil>>$/ ), q{Don't match compound inverted <?Tamil>} );
ok(!( "\c[TAMIL SIGN ANUSVARA]" ~~ m/^<+<-Tamil>>$/ ), q{Don't match internally inverted <?Tamil>} );
ok(!( "\x[76C0]"  ~~ m/^<+<?Tamil>>$/ ), q{Don't match unrelated <?Tamil>} );
ok("\x[76C0]"  ~~ m/^<-<?Tamil>>$/, q{Match unrelated externally inverted <?Tamil>} );
ok("\x[76C0]"  ~~ m/^<+<-Tamil>>$/, q{Match unrelated internally inverted <?Tamil>} );
ok("\x[76C0]\c[TAMIL SIGN ANUSVARA]" ~~ m/<+<?Tamil>>/, q{Match unanchored <?Tamil>} );

# Telugu


ok("\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<+<?Telugu>>$/, q{Match <?Telugu>} );
ok("\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<[A]+<?Telugu>>$/, q{Match compound <?Telugu>} );
ok(!( "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<-<?Telugu>>$/ ), q{Don't match externally inverted <?Telugu>} );
ok(!( "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<[A]-<?Telugu>>$/ ), q{Don't match compound inverted <?Telugu>} );
ok(!( "\c[TELUGU SIGN CANDRABINDU]" ~~ m/^<+<-Telugu>>$/ ), q{Don't match internally inverted <?Telugu>} );
ok(!( "\x[60BD]"  ~~ m/^<+<?Telugu>>$/ ), q{Don't match unrelated <?Telugu>} );
ok("\x[60BD]"  ~~ m/^<-<?Telugu>>$/, q{Match unrelated externally inverted <?Telugu>} );
ok("\x[60BD]"  ~~ m/^<+<-Telugu>>$/, q{Match unrelated internally inverted <?Telugu>} );
ok("\x[60BD]\c[TELUGU SIGN CANDRABINDU]" ~~ m/<+<?Telugu>>/, q{Match unanchored <?Telugu>} );

# Thaana


ok("\c[THAANA LETTER HAA]" ~~ m/^<+<?Thaana>>$/, q{Match <?Thaana>} );
ok("\c[THAANA LETTER HAA]" ~~ m/^<[A]+<?Thaana>>$/, q{Match compound <?Thaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<-<?Thaana>>$/ ), q{Don't match externally inverted <?Thaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<[A]-<?Thaana>>$/ ), q{Don't match compound inverted <?Thaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<+<-Thaana>>$/ ), q{Don't match internally inverted <?Thaana>} );
ok(!( "\x[2E74]"  ~~ m/^<+<?Thaana>>$/ ), q{Don't match unrelated <?Thaana>} );
ok("\x[2E74]"  ~~ m/^<-<?Thaana>>$/, q{Match unrelated externally inverted <?Thaana>} );
ok("\x[2E74]"  ~~ m/^<+<-Thaana>>$/, q{Match unrelated internally inverted <?Thaana>} );
ok("\x[2E74]\c[THAANA LETTER HAA]" ~~ m/<+<?Thaana>>/, q{Match unanchored <?Thaana>} );

# Thai


ok("\c[THAI CHARACTER KO KAI]" ~~ m/^<+<?Thai>>$/, q{Match <?Thai>} );
ok("\c[THAI CHARACTER KO KAI]" ~~ m/^<[A]+<?Thai>>$/, q{Match compound <?Thai>} );
ok(!( "\c[THAI CHARACTER KO KAI]" ~~ m/^<-<?Thai>>$/ ), q{Don't match externally inverted <?Thai>} );
ok(!( "\c[THAI CHARACTER KO KAI]" ~~ m/^<[A]-<?Thai>>$/ ), q{Don't match compound inverted <?Thai>} );
ok(!( "\c[THAI CHARACTER KO KAI]" ~~ m/^<+<-Thai>>$/ ), q{Don't match internally inverted <?Thai>} );
ok(!( "\x[A929]"  ~~ m/^<+<?Thai>>$/ ), q{Don't match unrelated <?Thai>} );
ok("\x[A929]"  ~~ m/^<-<?Thai>>$/, q{Match unrelated externally inverted <?Thai>} );
ok("\x[A929]"  ~~ m/^<+<-Thai>>$/, q{Match unrelated internally inverted <?Thai>} );
ok("\x[A929]\c[THAI CHARACTER KO KAI]" ~~ m/<+<?Thai>>/, q{Match unanchored <?Thai>} );

# Tibetan


ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<?Tibetan>>$/, q{Match <?Tibetan>} );
ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]+<?Tibetan>>$/, q{Match compound <?Tibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<-<?Tibetan>>$/ ), q{Don't match externally inverted <?Tibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]-<?Tibetan>>$/ ), q{Don't match compound inverted <?Tibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<-Tibetan>>$/ ), q{Don't match internally inverted <?Tibetan>} );
ok(!( "\x[19C9]"  ~~ m/^<+<?Tibetan>>$/ ), q{Don't match unrelated <?Tibetan>} );
ok("\x[19C9]"  ~~ m/^<-<?Tibetan>>$/, q{Match unrelated externally inverted <?Tibetan>} );
ok("\x[19C9]"  ~~ m/^<+<-Tibetan>>$/, q{Match unrelated internally inverted <?Tibetan>} );
ok("\x[19C9]\c[TIBETAN SYLLABLE OM]" ~~ m/<+<?Tibetan>>/, q{Match unanchored <?Tibetan>} );

# Yi


ok("\c[YI SYLLABLE IT]" ~~ m/^<+<?Yi>>$/, q{Match <?Yi>} );
ok("\c[YI SYLLABLE IT]" ~~ m/^<[A]+<?Yi>>$/, q{Match compound <?Yi>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<-<?Yi>>$/ ), q{Don't match externally inverted <?Yi>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<[A]-<?Yi>>$/ ), q{Don't match compound inverted <?Yi>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<+<-Yi>>$/ ), q{Don't match internally inverted <?Yi>} );
ok(!( "\x[3A38]"  ~~ m/^<+<?Yi>>$/ ), q{Don't match unrelated <?Yi>} );
ok("\x[3A38]"  ~~ m/^<-<?Yi>>$/, q{Match unrelated externally inverted <?Yi>} );
ok("\x[3A38]"  ~~ m/^<+<-Yi>>$/, q{Match unrelated internally inverted <?Yi>} );
ok("\x[3A38]\c[YI SYLLABLE IT]" ~~ m/<+<?Yi>>/, q{Match unanchored <?Yi>} );

# ASCIIHexDigit


ok("\c[DIGIT ZERO]" ~~ m/^<+<?ASCIIHexDigit>>$/, q{Match <?ASCIIHexDigit>} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?ASCIIHexDigit>>$/, q{Match compound <?ASCIIHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?ASCIIHexDigit>>$/ ), q{Don't match externally inverted <?ASCIIHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?ASCIIHexDigit>>$/ ), q{Don't match compound inverted <?ASCIIHexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-ASCIIHexDigit>>$/ ), q{Don't match internally inverted <?ASCIIHexDigit>} );
ok(!( "\x[55D7]"  ~~ m/^<+<?ASCIIHexDigit>>$/ ), q{Don't match unrelated <?ASCIIHexDigit>} );
ok("\x[55D7]"  ~~ m/^<-<?ASCIIHexDigit>>$/, q{Match unrelated externally inverted <?ASCIIHexDigit>} );
ok("\x[55D7]"  ~~ m/^<+<-ASCIIHexDigit>>$/, q{Match unrelated internally inverted <?ASCIIHexDigit>} );
ok("\x[55D7]\c[DIGIT ZERO]" ~~ m/<+<?ASCIIHexDigit>>/, q{Match unanchored <?ASCIIHexDigit>} );

# Dash


ok("\c[HYPHEN-MINUS]" ~~ m/^<+<?Dash>>$/, q{Match <?Dash>} );
ok("\c[HYPHEN-MINUS]" ~~ m/^<[A]+<?Dash>>$/, q{Match compound <?Dash>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<-<?Dash>>$/ ), q{Don't match externally inverted <?Dash>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<[A]-<?Dash>>$/ ), q{Don't match compound inverted <?Dash>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<+<-Dash>>$/ ), q{Don't match internally inverted <?Dash>} );
ok(!( "\x[4C7F]"  ~~ m/^<+<?Dash>>$/ ), q{Don't match unrelated <?Dash>} );
ok("\x[4C7F]"  ~~ m/^<-<?Dash>>$/, q{Match unrelated externally inverted <?Dash>} );
ok("\x[4C7F]"  ~~ m/^<+<-Dash>>$/, q{Match unrelated internally inverted <?Dash>} );
ok("\x[4C7F]\c[HYPHEN-MINUS]" ~~ m/<+<?Dash>>/, q{Match unanchored <?Dash>} );

# Diacritic


ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<?Diacritic>>$/, q{Match <?Diacritic>} );
ok("\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]+<?Diacritic>>$/, q{Match compound <?Diacritic>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<-<?Diacritic>>$/ ), q{Don't match externally inverted <?Diacritic>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<[A]-<?Diacritic>>$/ ), q{Don't match compound inverted <?Diacritic>} );
ok(!( "\c[CIRCUMFLEX ACCENT]" ~~ m/^<+<-Diacritic>>$/ ), q{Don't match internally inverted <?Diacritic>} );
ok(!( "\x[398E]"  ~~ m/^<+<?Diacritic>>$/ ), q{Don't match unrelated <?Diacritic>} );
ok("\x[398E]"  ~~ m/^<-<?Diacritic>>$/, q{Match unrelated externally inverted <?Diacritic>} );
ok("\x[398E]"  ~~ m/^<+<-Diacritic>>$/, q{Match unrelated internally inverted <?Diacritic>} );
ok("\x[398E]\c[CIRCUMFLEX ACCENT]" ~~ m/<+<?Diacritic>>/, q{Match unanchored <?Diacritic>} );

# Extender


ok("\c[MIDDLE DOT]" ~~ m/^<+<?Extender>>$/, q{Match <?Extender>} );
ok("\c[MIDDLE DOT]" ~~ m/^<[A]+<?Extender>>$/, q{Match compound <?Extender>} );
ok(!( "\c[MIDDLE DOT]" ~~ m/^<-<?Extender>>$/ ), q{Don't match externally inverted <?Extender>} );
ok(!( "\c[MIDDLE DOT]" ~~ m/^<[A]-<?Extender>>$/ ), q{Don't match compound inverted <?Extender>} );
ok(!( "\c[MIDDLE DOT]" ~~ m/^<+<-Extender>>$/ ), q{Don't match internally inverted <?Extender>} );
ok(!( "\x[3F66]"  ~~ m/^<+<?Extender>>$/ ), q{Don't match unrelated <?Extender>} );
ok("\x[3F66]"  ~~ m/^<-<?Extender>>$/, q{Match unrelated externally inverted <?Extender>} );
ok("\x[3F66]"  ~~ m/^<+<-Extender>>$/, q{Match unrelated internally inverted <?Extender>} );
ok("\x[3F66]\c[MIDDLE DOT]" ~~ m/<+<?Extender>>/, q{Match unanchored <?Extender>} );

# GraphemeLink


ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<?GraphemeLink>>$/, q{Match <?GraphemeLink>} );
ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]+<?GraphemeLink>>$/, q{Match compound <?GraphemeLink>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<-<?GraphemeLink>>$/ ), q{Don't match externally inverted <?GraphemeLink>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]-<?GraphemeLink>>$/ ), q{Don't match compound inverted <?GraphemeLink>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<-GraphemeLink>>$/ ), q{Don't match internally inverted <?GraphemeLink>} );
ok(!( "\x[34DA]"  ~~ m/^<+<?GraphemeLink>>$/ ), q{Don't match unrelated <?GraphemeLink>} );
ok("\x[34DA]"  ~~ m/^<-<?GraphemeLink>>$/, q{Match unrelated externally inverted <?GraphemeLink>} );
ok("\x[34DA]"  ~~ m/^<+<-GraphemeLink>>$/, q{Match unrelated internally inverted <?GraphemeLink>} );
ok("\x[34DA]\c[COMBINING GRAPHEME JOINER]" ~~ m/<+<?GraphemeLink>>/, q{Match unanchored <?GraphemeLink>} );

# HexDigit


ok("\c[DIGIT ZERO]" ~~ m/^<+<?HexDigit>>$/, q{Match <?HexDigit>} );
ok("\c[DIGIT ZERO]" ~~ m/^<[A]+<?HexDigit>>$/, q{Match compound <?HexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<-<?HexDigit>>$/ ), q{Don't match externally inverted <?HexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<[A]-<?HexDigit>>$/ ), q{Don't match compound inverted <?HexDigit>} );
ok(!( "\c[DIGIT ZERO]" ~~ m/^<+<-HexDigit>>$/ ), q{Don't match internally inverted <?HexDigit>} );
ok(!( "\x[D446]"  ~~ m/^<+<?HexDigit>>$/ ), q{Don't match unrelated <?HexDigit>} );
ok("\x[D446]"  ~~ m/^<-<?HexDigit>>$/, q{Match unrelated externally inverted <?HexDigit>} );
ok("\x[D446]"  ~~ m/^<+<-HexDigit>>$/, q{Match unrelated internally inverted <?HexDigit>} );
ok("\x[D446]\c[DIGIT ZERO]" ~~ m/<+<?HexDigit>>/, q{Match unanchored <?HexDigit>} );

# Hyphen


ok("\c[HYPHEN-MINUS]" ~~ m/^<+<?Hyphen>>$/, q{Match <?Hyphen>} );
ok("\c[HYPHEN-MINUS]" ~~ m/^<[A]+<?Hyphen>>$/, q{Match compound <?Hyphen>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<-<?Hyphen>>$/ ), q{Don't match externally inverted <?Hyphen>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<[A]-<?Hyphen>>$/ ), q{Don't match compound inverted <?Hyphen>} );
ok(!( "\c[HYPHEN-MINUS]" ~~ m/^<+<-Hyphen>>$/ ), q{Don't match internally inverted <?Hyphen>} );
ok(!( "\c[YI SYLLABLE WOX]"  ~~ m/^<+<?Hyphen>>$/ ), q{Don't match unrelated <?Hyphen>} );
ok("\c[YI SYLLABLE WOX]"  ~~ m/^<-<?Hyphen>>$/, q{Match unrelated externally inverted <?Hyphen>} );
ok("\c[YI SYLLABLE WOX]"  ~~ m/^<+<-Hyphen>>$/, q{Match unrelated internally inverted <?Hyphen>} );
ok("\c[YI SYLLABLE WOX]\c[HYPHEN-MINUS]" ~~ m/<+<?Hyphen>>/, q{Match unanchored <?Hyphen>} );

# Ideographic


ok("\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<+<?Ideographic>>$/, q{Match <?Ideographic>} );
ok("\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<[A]+<?Ideographic>>$/, q{Match compound <?Ideographic>} );
ok(!( "\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<-<?Ideographic>>$/ ), q{Don't match externally inverted <?Ideographic>} );
ok(!( "\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<[A]-<?Ideographic>>$/ ), q{Don't match compound inverted <?Ideographic>} );
ok(!( "\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/^<+<-Ideographic>>$/ ), q{Don't match internally inverted <?Ideographic>} );
ok(!( "\x[CB86]"  ~~ m/^<+<?Ideographic>>$/ ), q{Don't match unrelated <?Ideographic>} );
ok("\x[CB86]"  ~~ m/^<-<?Ideographic>>$/, q{Match unrelated externally inverted <?Ideographic>} );
ok("\x[CB86]"  ~~ m/^<+<-Ideographic>>$/, q{Match unrelated internally inverted <?Ideographic>} );
ok("\x[CB86]\c[IDEOGRAPHIC CLOSING MARK]" ~~ m/<+<?Ideographic>>/, q{Match unanchored <?Ideographic>} );

# IDSBinaryOperator


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<?IDSBinaryOperator>>$/, q{Match <?IDSBinaryOperator>} );
ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]+<?IDSBinaryOperator>>$/, q{Match compound <?IDSBinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<-<?IDSBinaryOperator>>$/ ), q{Don't match externally inverted <?IDSBinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]-<?IDSBinaryOperator>>$/ ), q{Don't match compound inverted <?IDSBinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<-IDSBinaryOperator>>$/ ), q{Don't match internally inverted <?IDSBinaryOperator>} );
ok(!( "\x[54A0]"  ~~ m/^<+<?IDSBinaryOperator>>$/ ), q{Don't match unrelated <?IDSBinaryOperator>} );
ok("\x[54A0]"  ~~ m/^<-<?IDSBinaryOperator>>$/, q{Match unrelated externally inverted <?IDSBinaryOperator>} );
ok("\x[54A0]"  ~~ m/^<+<-IDSBinaryOperator>>$/, q{Match unrelated internally inverted <?IDSBinaryOperator>} );
ok("\x[54A0]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/<+<?IDSBinaryOperator>>/, q{Match unanchored <?IDSBinaryOperator>} );

# IDSTrinaryOperator


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<+<?IDSTrinaryOperator>>$/, q{Match <?IDSTrinaryOperator>} );
ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<[A]+<?IDSTrinaryOperator>>$/, q{Match compound <?IDSTrinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<-<?IDSTrinaryOperator>>$/ ), q{Don't match externally inverted <?IDSTrinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<[A]-<?IDSTrinaryOperator>>$/ ), q{Don't match compound inverted <?IDSTrinaryOperator>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/^<+<-IDSTrinaryOperator>>$/ ), q{Don't match internally inverted <?IDSTrinaryOperator>} );
ok(!( "\x[4900]"  ~~ m/^<+<?IDSTrinaryOperator>>$/ ), q{Don't match unrelated <?IDSTrinaryOperator>} );
ok("\x[4900]"  ~~ m/^<-<?IDSTrinaryOperator>>$/, q{Match unrelated externally inverted <?IDSTrinaryOperator>} );
ok("\x[4900]"  ~~ m/^<+<-IDSTrinaryOperator>>$/, q{Match unrelated internally inverted <?IDSTrinaryOperator>} );
ok("\x[4900]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO MIDDLE AND RIGHT]" ~~ m/<+<?IDSTrinaryOperator>>/, q{Match unanchored <?IDSTrinaryOperator>} );

# JoinControl


ok("\c[ZERO WIDTH NON-JOINER]" ~~ m/^<+<?JoinControl>>$/, q{Match <?JoinControl>} );
ok("\c[ZERO WIDTH NON-JOINER]" ~~ m/^<[A]+<?JoinControl>>$/, q{Match compound <?JoinControl>} );
ok(!( "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<-<?JoinControl>>$/ ), q{Don't match externally inverted <?JoinControl>} );
ok(!( "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<[A]-<?JoinControl>>$/ ), q{Don't match compound inverted <?JoinControl>} );
ok(!( "\c[ZERO WIDTH NON-JOINER]" ~~ m/^<+<-JoinControl>>$/ ), q{Don't match internally inverted <?JoinControl>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER OT]"  ~~ m/^<+<?JoinControl>>$/ ), q{Don't match unrelated <?JoinControl>} );
ok("\c[CYRILLIC CAPITAL LETTER OT]"  ~~ m/^<-<?JoinControl>>$/, q{Match unrelated externally inverted <?JoinControl>} );
ok("\c[CYRILLIC CAPITAL LETTER OT]"  ~~ m/^<+<-JoinControl>>$/, q{Match unrelated internally inverted <?JoinControl>} );
ok("\c[CYRILLIC CAPITAL LETTER OT]\c[ZERO WIDTH NON-JOINER]" ~~ m/<+<?JoinControl>>/, q{Match unanchored <?JoinControl>} );

# LogicalOrderException


ok("\c[THAI CHARACTER SARA E]" ~~ m/^<+<?LogicalOrderException>>$/, q{Match <?LogicalOrderException>} );
ok("\c[THAI CHARACTER SARA E]" ~~ m/^<[A]+<?LogicalOrderException>>$/, q{Match compound <?LogicalOrderException>} );
ok(!( "\c[THAI CHARACTER SARA E]" ~~ m/^<-<?LogicalOrderException>>$/ ), q{Don't match externally inverted <?LogicalOrderException>} );
ok(!( "\c[THAI CHARACTER SARA E]" ~~ m/^<[A]-<?LogicalOrderException>>$/ ), q{Don't match compound inverted <?LogicalOrderException>} );
ok(!( "\c[THAI CHARACTER SARA E]" ~~ m/^<+<-LogicalOrderException>>$/ ), q{Don't match internally inverted <?LogicalOrderException>} );
ok(!( "\x[88D2]"  ~~ m/^<+<?LogicalOrderException>>$/ ), q{Don't match unrelated <?LogicalOrderException>} );
ok("\x[88D2]"  ~~ m/^<-<?LogicalOrderException>>$/, q{Match unrelated externally inverted <?LogicalOrderException>} );
ok("\x[88D2]"  ~~ m/^<+<-LogicalOrderException>>$/, q{Match unrelated internally inverted <?LogicalOrderException>} );
ok(!( "\x[88D2]" ~~ m/^<+<?LogicalOrderException>>$/ ), q{Don't match related <?LogicalOrderException>} );
ok("\x[88D2]" ~~ m/^<+<-LogicalOrderException>>$/, q{Match related internally inverted <?LogicalOrderException>} );
ok("\x[88D2]" ~~ m/^<-<?LogicalOrderException>>$/, q{Match related externally inverted <?LogicalOrderException>} );
ok("\x[88D2]\x[88D2]\c[THAI CHARACTER SARA E]" ~~ m/<+<?LogicalOrderException>>/, q{Match unanchored <?LogicalOrderException>} );

# NoncharacterCodePoint


ok(!( "\c[CIRCLED HANGUL NIEUN A]"  ~~ m/^<+<?NoncharacterCodePoint>>$/ ), q{Don't match unrelated <?NoncharacterCodePoint>} );
ok("\c[CIRCLED HANGUL NIEUN A]"  ~~ m/^<-<?NoncharacterCodePoint>>$/, q{Match unrelated externally inverted <?NoncharacterCodePoint>} );
ok("\c[CIRCLED HANGUL NIEUN A]"  ~~ m/^<+<-NoncharacterCodePoint>>$/, q{Match unrelated internally inverted <?NoncharacterCodePoint>} );
ok(!( "\c[CIRCLED IDEOGRAPH ONE]" ~~ m/^<+<?NoncharacterCodePoint>>$/ ), q{Don't match related <?NoncharacterCodePoint>} );
ok("\c[CIRCLED IDEOGRAPH ONE]" ~~ m/^<+<-NoncharacterCodePoint>>$/, q{Match related internally inverted <?NoncharacterCodePoint>} );
ok("\c[CIRCLED IDEOGRAPH ONE]" ~~ m/^<-<?NoncharacterCodePoint>>$/, q{Match related externally inverted <?NoncharacterCodePoint>} );

# OtherAlphabetic


ok("\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<+<?OtherAlphabetic>>$/, q{Match <?OtherAlphabetic>} );
ok("\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<[A]+<?OtherAlphabetic>>$/, q{Match compound <?OtherAlphabetic>} );
ok(!( "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<-<?OtherAlphabetic>>$/ ), q{Don't match externally inverted <?OtherAlphabetic>} );
ok(!( "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<[A]-<?OtherAlphabetic>>$/ ), q{Don't match compound inverted <?OtherAlphabetic>} );
ok(!( "\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/^<+<-OtherAlphabetic>>$/ ), q{Don't match internally inverted <?OtherAlphabetic>} );
ok(!( "\x[7B02]"  ~~ m/^<+<?OtherAlphabetic>>$/ ), q{Don't match unrelated <?OtherAlphabetic>} );
ok("\x[7B02]"  ~~ m/^<-<?OtherAlphabetic>>$/, q{Match unrelated externally inverted <?OtherAlphabetic>} );
ok("\x[7B02]"  ~~ m/^<+<-OtherAlphabetic>>$/, q{Match unrelated internally inverted <?OtherAlphabetic>} );
ok("\x[7B02]\c[COMBINING GREEK YPOGEGRAMMENI]" ~~ m/<+<?OtherAlphabetic>>/, q{Match unanchored <?OtherAlphabetic>} );

# OtherDefaultIgnorableCodePoint


ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<?OtherDefaultIgnorableCodePoint>>$/, q{Match <?OtherDefaultIgnorableCodePoint>} );
ok("\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]+<?OtherDefaultIgnorableCodePoint>>$/, q{Match compound <?OtherDefaultIgnorableCodePoint>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<-<?OtherDefaultIgnorableCodePoint>>$/ ), q{Don't match externally inverted <?OtherDefaultIgnorableCodePoint>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<[A]-<?OtherDefaultIgnorableCodePoint>>$/ ), q{Don't match compound inverted <?OtherDefaultIgnorableCodePoint>} );
ok(!( "\c[COMBINING GRAPHEME JOINER]" ~~ m/^<+<-OtherDefaultIgnorableCodePoint>>$/ ), q{Don't match internally inverted <?OtherDefaultIgnorableCodePoint>} );
ok(!( "\x[42DA]"  ~~ m/^<+<?OtherDefaultIgnorableCodePoint>>$/ ), q{Don't match unrelated <?OtherDefaultIgnorableCodePoint>} );
ok("\x[42DA]"  ~~ m/^<-<?OtherDefaultIgnorableCodePoint>>$/, q{Match unrelated externally inverted <?OtherDefaultIgnorableCodePoint>} );
ok("\x[42DA]"  ~~ m/^<+<-OtherDefaultIgnorableCodePoint>>$/, q{Match unrelated internally inverted <?OtherDefaultIgnorableCodePoint>} );
ok("\x[42DA]\c[COMBINING GRAPHEME JOINER]" ~~ m/<+<?OtherDefaultIgnorableCodePoint>>/, q{Match unanchored <?OtherDefaultIgnorableCodePoint>} );

# OtherGraphemeExtend


ok("\c[BENGALI VOWEL SIGN AA]" ~~ m/^<+<?OtherGraphemeExtend>>$/, q{Match <?OtherGraphemeExtend>} );
ok("\c[BENGALI VOWEL SIGN AA]" ~~ m/^<[A]+<?OtherGraphemeExtend>>$/, q{Match compound <?OtherGraphemeExtend>} );
ok(!( "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<-<?OtherGraphemeExtend>>$/ ), q{Don't match externally inverted <?OtherGraphemeExtend>} );
ok(!( "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<[A]-<?OtherGraphemeExtend>>$/ ), q{Don't match compound inverted <?OtherGraphemeExtend>} );
ok(!( "\c[BENGALI VOWEL SIGN AA]" ~~ m/^<+<-OtherGraphemeExtend>>$/ ), q{Don't match internally inverted <?OtherGraphemeExtend>} );
ok(!( "\c[TAI LE LETTER KHA]"  ~~ m/^<+<?OtherGraphemeExtend>>$/ ), q{Don't match unrelated <?OtherGraphemeExtend>} );
ok("\c[TAI LE LETTER KHA]"  ~~ m/^<-<?OtherGraphemeExtend>>$/, q{Match unrelated externally inverted <?OtherGraphemeExtend>} );
ok("\c[TAI LE LETTER KHA]"  ~~ m/^<+<-OtherGraphemeExtend>>$/, q{Match unrelated internally inverted <?OtherGraphemeExtend>} );
ok("\c[TAI LE LETTER KHA]\c[BENGALI VOWEL SIGN AA]" ~~ m/<+<?OtherGraphemeExtend>>/, q{Match unanchored <?OtherGraphemeExtend>} );

# OtherLowercase


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?OtherLowercase>>$/, q{Match <?OtherLowercase>} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?OtherLowercase>>$/, q{Match compound <?OtherLowercase>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?OtherLowercase>>$/ ), q{Don't match externally inverted <?OtherLowercase>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?OtherLowercase>>$/ ), q{Don't match compound inverted <?OtherLowercase>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-OtherLowercase>>$/ ), q{Don't match internally inverted <?OtherLowercase>} );
ok(!( "\x[8153]"  ~~ m/^<+<?OtherLowercase>>$/ ), q{Don't match unrelated <?OtherLowercase>} );
ok("\x[8153]"  ~~ m/^<-<?OtherLowercase>>$/, q{Match unrelated externally inverted <?OtherLowercase>} );
ok("\x[8153]"  ~~ m/^<+<-OtherLowercase>>$/, q{Match unrelated internally inverted <?OtherLowercase>} );
ok("\x[8153]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?OtherLowercase>>/, q{Match unanchored <?OtherLowercase>} );

# OtherMath


ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?OtherMath>>$/, q{Match <?OtherMath>} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?OtherMath>>$/, q{Match compound <?OtherMath>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?OtherMath>>$/ ), q{Don't match externally inverted <?OtherMath>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?OtherMath>>$/ ), q{Don't match compound inverted <?OtherMath>} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-OtherMath>>$/ ), q{Don't match internally inverted <?OtherMath>} );
ok(!( "\x[6D2F]"  ~~ m/^<+<?OtherMath>>$/ ), q{Don't match unrelated <?OtherMath>} );
ok("\x[6D2F]"  ~~ m/^<-<?OtherMath>>$/, q{Match unrelated externally inverted <?OtherMath>} );
ok("\x[6D2F]"  ~~ m/^<+<-OtherMath>>$/, q{Match unrelated internally inverted <?OtherMath>} );
ok("\x[6D2F]\c[LEFT PARENTHESIS]" ~~ m/<+<?OtherMath>>/, q{Match unanchored <?OtherMath>} );

# OtherUppercase


ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<+<?OtherUppercase>>$/, q{Match <?OtherUppercase>} );
ok("\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]+<?OtherUppercase>>$/, q{Match compound <?OtherUppercase>} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<-<?OtherUppercase>>$/ ), q{Don't match externally inverted <?OtherUppercase>} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<[A]-<?OtherUppercase>>$/ ), q{Don't match compound inverted <?OtherUppercase>} );
ok(!( "\c[ROMAN NUMERAL ONE]" ~~ m/^<+<-OtherUppercase>>$/ ), q{Don't match internally inverted <?OtherUppercase>} );
ok(!( "\x[A746]"  ~~ m/^<+<?OtherUppercase>>$/ ), q{Don't match unrelated <?OtherUppercase>} );
ok("\x[A746]"  ~~ m/^<-<?OtherUppercase>>$/, q{Match unrelated externally inverted <?OtherUppercase>} );
ok("\x[A746]"  ~~ m/^<+<-OtherUppercase>>$/, q{Match unrelated internally inverted <?OtherUppercase>} );
ok("\x[A746]\c[ROMAN NUMERAL ONE]" ~~ m/<+<?OtherUppercase>>/, q{Match unanchored <?OtherUppercase>} );

# QuotationMark


ok("\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<+<?QuotationMark>>$/, q{Match <?QuotationMark>} );
ok("\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<[A]+<?QuotationMark>>$/, q{Match compound <?QuotationMark>} );
ok(!( "\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<-<?QuotationMark>>$/ ), q{Don't match externally inverted <?QuotationMark>} );
ok(!( "\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<[A]-<?QuotationMark>>$/ ), q{Don't match compound inverted <?QuotationMark>} );
ok(!( "\c[LEFT SINGLE QUOTATION MARK]" ~~ m/^<+<-QuotationMark>>$/ ), q{Don't match internally inverted <?QuotationMark>} );
ok(!( "\c[GURMUKHI VOWEL SIGN AI]"  ~~ m/^<+<?QuotationMark>>$/ ), q{Don't match unrelated <?QuotationMark>} );
ok("\c[GURMUKHI VOWEL SIGN AI]"  ~~ m/^<-<?QuotationMark>>$/, q{Match unrelated externally inverted <?QuotationMark>} );
ok("\c[GURMUKHI VOWEL SIGN AI]"  ~~ m/^<+<-QuotationMark>>$/, q{Match unrelated internally inverted <?QuotationMark>} );
ok("\c[GURMUKHI VOWEL SIGN AI]\c[LEFT SINGLE QUOTATION MARK]" ~~ m/<+<?QuotationMark>>/, q{Match unanchored <?QuotationMark>} );

# Radical


ok("\c[CJK RADICAL REPEAT]" ~~ m/^<+<?Radical>>$/, q{Match <?Radical>} );
ok("\c[CJK RADICAL REPEAT]" ~~ m/^<[A]+<?Radical>>$/, q{Match compound <?Radical>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<-<?Radical>>$/ ), q{Don't match externally inverted <?Radical>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<[A]-<?Radical>>$/ ), q{Don't match compound inverted <?Radical>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<+<-Radical>>$/ ), q{Don't match internally inverted <?Radical>} );
ok(!( "\c[RUNIC LETTER ETH]"  ~~ m/^<+<?Radical>>$/ ), q{Don't match unrelated <?Radical>} );
ok("\c[RUNIC LETTER ETH]"  ~~ m/^<-<?Radical>>$/, q{Match unrelated externally inverted <?Radical>} );
ok("\c[RUNIC LETTER ETH]"  ~~ m/^<+<-Radical>>$/, q{Match unrelated internally inverted <?Radical>} );
ok("\c[RUNIC LETTER ETH]\c[CJK RADICAL REPEAT]" ~~ m/<+<?Radical>>/, q{Match unanchored <?Radical>} );

# SoftDotted


ok("\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<+<?SoftDotted>>$/, q{Match <?SoftDotted>} );
ok("\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<[A]+<?SoftDotted>>$/, q{Match compound <?SoftDotted>} );
ok(!( "\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<-<?SoftDotted>>$/ ), q{Don't match externally inverted <?SoftDotted>} );
ok(!( "\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<[A]-<?SoftDotted>>$/ ), q{Don't match compound inverted <?SoftDotted>} );
ok(!( "\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/^<+<-SoftDotted>>$/ ), q{Don't match internally inverted <?SoftDotted>} );
ok(!( "\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]"  ~~ m/^<+<?SoftDotted>>$/ ), q{Don't match unrelated <?SoftDotted>} );
ok("\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]"  ~~ m/^<-<?SoftDotted>>$/, q{Match unrelated externally inverted <?SoftDotted>} );
ok("\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]"  ~~ m/^<+<-SoftDotted>>$/, q{Match unrelated internally inverted <?SoftDotted>} );
ok(!( "\c[GREEK KORONIS]" ~~ m/^<+<?SoftDotted>>$/ ), q{Don't match related <?SoftDotted>} );
ok("\c[GREEK KORONIS]" ~~ m/^<+<-SoftDotted>>$/, q{Match related internally inverted <?SoftDotted>} );
ok("\c[GREEK KORONIS]" ~~ m/^<-<?SoftDotted>>$/, q{Match related externally inverted <?SoftDotted>} );
ok("\c[LATIN CAPITAL LETTER B WITH DOT ABOVE]\c[GREEK KORONIS]\c[LATIN SMALL LETTER I WITH TILDE BELOW]" ~~ m/<+<?SoftDotted>>/, q{Match unanchored <?SoftDotted>} );

# TerminalPunctuation


ok("\c[EXCLAMATION MARK]" ~~ m/^<+<?TerminalPunctuation>>$/, q{Match <?TerminalPunctuation>} );
ok("\c[EXCLAMATION MARK]" ~~ m/^<[A]+<?TerminalPunctuation>>$/, q{Match compound <?TerminalPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<-<?TerminalPunctuation>>$/ ), q{Don't match externally inverted <?TerminalPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<[A]-<?TerminalPunctuation>>$/ ), q{Don't match compound inverted <?TerminalPunctuation>} );
ok(!( "\c[EXCLAMATION MARK]" ~~ m/^<+<-TerminalPunctuation>>$/ ), q{Don't match internally inverted <?TerminalPunctuation>} );
ok(!( "\x[B43A]"  ~~ m/^<+<?TerminalPunctuation>>$/ ), q{Don't match unrelated <?TerminalPunctuation>} );
ok("\x[B43A]"  ~~ m/^<-<?TerminalPunctuation>>$/, q{Match unrelated externally inverted <?TerminalPunctuation>} );
ok("\x[B43A]"  ~~ m/^<+<-TerminalPunctuation>>$/, q{Match unrelated internally inverted <?TerminalPunctuation>} );
ok("\x[B43A]\c[EXCLAMATION MARK]" ~~ m/<+<?TerminalPunctuation>>/, q{Match unanchored <?TerminalPunctuation>} );

# UnifiedIdeograph


ok("\x[88D8]" ~~ m/^<+<?UnifiedIdeograph>>$/, q{Match <?UnifiedIdeograph>} );
ok("\x[88D8]" ~~ m/^<[A]+<?UnifiedIdeograph>>$/, q{Match compound <?UnifiedIdeograph>} );
ok(!( "\x[88D8]" ~~ m/^<-<?UnifiedIdeograph>>$/ ), q{Don't match externally inverted <?UnifiedIdeograph>} );
ok(!( "\x[88D8]" ~~ m/^<[A]-<?UnifiedIdeograph>>$/ ), q{Don't match compound inverted <?UnifiedIdeograph>} );
ok(!( "\x[88D8]" ~~ m/^<+<-UnifiedIdeograph>>$/ ), q{Don't match internally inverted <?UnifiedIdeograph>} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?UnifiedIdeograph>>$/ ), q{Don't match unrelated <?UnifiedIdeograph>} );
ok("\x[9FA6]"  ~~ m/^<-<?UnifiedIdeograph>>$/, q{Match unrelated externally inverted <?UnifiedIdeograph>} );
ok("\x[9FA6]"  ~~ m/^<+<-UnifiedIdeograph>>$/, q{Match unrelated internally inverted <?UnifiedIdeograph>} );
ok("\x[9FA6]\x[88D8]" ~~ m/<+<?UnifiedIdeograph>>/, q{Match unanchored <?UnifiedIdeograph>} );

# WhiteSpace


ok("\c[CHARACTER TABULATION]" ~~ m/^<+<?WhiteSpace>>$/, q{Match <?WhiteSpace>} );
ok("\c[CHARACTER TABULATION]" ~~ m/^<[A]+<?WhiteSpace>>$/, q{Match compound <?WhiteSpace>} );
ok(!( "\c[CHARACTER TABULATION]" ~~ m/^<-<?WhiteSpace>>$/ ), q{Don't match externally inverted <?WhiteSpace>} );
ok(!( "\c[CHARACTER TABULATION]" ~~ m/^<[A]-<?WhiteSpace>>$/ ), q{Don't match compound inverted <?WhiteSpace>} );
ok(!( "\c[CHARACTER TABULATION]" ~~ m/^<+<-WhiteSpace>>$/ ), q{Don't match internally inverted <?WhiteSpace>} );
ok(!( "\x[4345]"  ~~ m/^<+<?WhiteSpace>>$/ ), q{Don't match unrelated <?WhiteSpace>} );
ok("\x[4345]"  ~~ m/^<-<?WhiteSpace>>$/, q{Match unrelated externally inverted <?WhiteSpace>} );
ok("\x[4345]"  ~~ m/^<+<-WhiteSpace>>$/, q{Match unrelated internally inverted <?WhiteSpace>} );
ok("\x[4345]\c[CHARACTER TABULATION]" ~~ m/<+<?WhiteSpace>>/, q{Match unanchored <?WhiteSpace>} );

# Alphabetic      # Lu + Ll + Lt + Lm + Lo + OtherAlphabetic


ok("\x[3816]" ~~ m/^<+<?Alphabetic>>$/, q{Match (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[3816]" ~~ m/^<[A]+<?Alphabetic>>$/, q{Match compound (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[3816]" ~~ m/^<-<?Alphabetic>>$/ ), q{Don't match externally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[3816]" ~~ m/^<[A]-<?Alphabetic>>$/ ), q{Don't match compound inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[3816]" ~~ m/^<+<-Alphabetic>>$/ ), q{Don't match internally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok(!( "\x[4DB6]"  ~~ m/^<+<?Alphabetic>>$/ ), q{Don't match unrelated (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[4DB6]"  ~~ m/^<-<?Alphabetic>>$/, q{Match unrelated externally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[4DB6]"  ~~ m/^<+<-Alphabetic>>$/, q{Match unrelated internally inverted (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );
ok("\x[4DB6]\x[3816]" ~~ m/<+<?Alphabetic>>/, q{Match unanchored (Lu + Ll + Lt + Lm + Lo + OtherAlphabetic)} );

# Lowercase       # Ll + OtherLowercase


ok("\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<+<?Lowercase>>$/, q{Match (Ll + OtherLowercase)} );
ok("\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<[A]+<?Lowercase>>$/, q{Match compound (Ll + OtherLowercase)} );
ok(!( "\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<-<?Lowercase>>$/ ), q{Don't match externally inverted (Ll + OtherLowercase)} );
ok(!( "\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<[A]-<?Lowercase>>$/ ), q{Don't match compound inverted (Ll + OtherLowercase)} );
ok(!( "\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/^<+<-Lowercase>>$/ ), q{Don't match internally inverted (Ll + OtherLowercase)} );
ok(!( "\x[0D3D]"  ~~ m/^<+<?Lowercase>>$/ ), q{Don't match unrelated (Ll + OtherLowercase)} );
ok("\x[0D3D]"  ~~ m/^<-<?Lowercase>>$/, q{Match unrelated externally inverted (Ll + OtherLowercase)} );
ok("\x[0D3D]"  ~~ m/^<+<-Lowercase>>$/, q{Match unrelated internally inverted (Ll + OtherLowercase)} );
ok(!( "\c[MALAYALAM LETTER VOCALIC RR]" ~~ m/^<+<?Lowercase>>$/ ), q{Don't match related (Ll + OtherLowercase)} );
ok("\c[MALAYALAM LETTER VOCALIC RR]" ~~ m/^<+<-Lowercase>>$/, q{Match related internally inverted (Ll + OtherLowercase)} );
ok("\c[MALAYALAM LETTER VOCALIC RR]" ~~ m/^<-<?Lowercase>>$/, q{Match related externally inverted (Ll + OtherLowercase)} );
ok("\x[0D3D]\c[MALAYALAM LETTER VOCALIC RR]\c[LATIN LETTER SMALL CAPITAL A]" ~~ m/<+<?Lowercase>>/, q{Match unanchored (Ll + OtherLowercase)} );

# Uppercase       # Lu + OtherUppercase


ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<?Uppercase>>$/, q{Match (Lu + OtherUppercase)} );
ok("\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]+<?Uppercase>>$/, q{Match compound (Lu + OtherUppercase)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<-<?Uppercase>>$/ ), q{Don't match externally inverted (Lu + OtherUppercase)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<[A]-<?Uppercase>>$/ ), q{Don't match compound inverted (Lu + OtherUppercase)} );
ok(!( "\c[LATIN CAPITAL LETTER A]" ~~ m/^<+<-Uppercase>>$/ ), q{Don't match internally inverted (Lu + OtherUppercase)} );
ok(!( "\x[C107]"  ~~ m/^<+<?Uppercase>>$/ ), q{Don't match unrelated (Lu + OtherUppercase)} );
ok("\x[C107]"  ~~ m/^<-<?Uppercase>>$/, q{Match unrelated externally inverted (Lu + OtherUppercase)} );
ok("\x[C107]"  ~~ m/^<+<-Uppercase>>$/, q{Match unrelated internally inverted (Lu + OtherUppercase)} );
ok("\x[C107]\c[LATIN CAPITAL LETTER A]" ~~ m/<+<?Uppercase>>/, q{Match unanchored (Lu + OtherUppercase)} );

# Math            # Sm + OtherMath


ok("\c[LEFT PARENTHESIS]" ~~ m/^<+<?Math>>$/, q{Match (Sm + OtherMath)} );
ok("\c[LEFT PARENTHESIS]" ~~ m/^<[A]+<?Math>>$/, q{Match compound (Sm + OtherMath)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<-<?Math>>$/ ), q{Don't match externally inverted (Sm + OtherMath)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<[A]-<?Math>>$/ ), q{Don't match compound inverted (Sm + OtherMath)} );
ok(!( "\c[LEFT PARENTHESIS]" ~~ m/^<+<-Math>>$/ ), q{Don't match internally inverted (Sm + OtherMath)} );
ok(!( "\x[D73F]"  ~~ m/^<+<?Math>>$/ ), q{Don't match unrelated (Sm + OtherMath)} );
ok("\x[D73F]"  ~~ m/^<-<?Math>>$/, q{Match unrelated externally inverted (Sm + OtherMath)} );
ok("\x[D73F]"  ~~ m/^<+<-Math>>$/, q{Match unrelated internally inverted (Sm + OtherMath)} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?Math>>$/ ), q{Don't match related (Sm + OtherMath)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-Math>>$/, q{Match related internally inverted (Sm + OtherMath)} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?Math>>$/, q{Match related externally inverted (Sm + OtherMath)} );
ok("\x[D73F]\c[COMBINING GRAVE ACCENT]\c[LEFT PARENTHESIS]" ~~ m/<+<?Math>>/, q{Match unanchored (Sm + OtherMath)} );

# ID_Start        # Lu + Ll + Lt + Lm + Lo + Nl


ok("\x[4E5B]" ~~ m/^<+<?ID_Start>>$/, q{Match (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[4E5B]" ~~ m/^<[A]+<?ID_Start>>$/, q{Match compound (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[4E5B]" ~~ m/^<-<?ID_Start>>$/ ), q{Don't match externally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[4E5B]" ~~ m/^<[A]-<?ID_Start>>$/ ), q{Don't match compound inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[4E5B]" ~~ m/^<+<-ID_Start>>$/ ), q{Don't match internally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok(!( "\x[9FA6]"  ~~ m/^<+<?ID_Start>>$/ ), q{Don't match unrelated (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[9FA6]"  ~~ m/^<-<?ID_Start>>$/, q{Match unrelated externally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[9FA6]"  ~~ m/^<+<-ID_Start>>$/, q{Match unrelated internally inverted (Lu + Ll + Lt + Lm + Lo + Nl)} );
ok("\x[9FA6]\x[4E5B]" ~~ m/<+<?ID_Start>>/, q{Match unanchored (Lu + Ll + Lt + Lm + Lo + Nl)} );

# ID_Continue     # ID_Start + Mn + Mc + Nd + Pc


ok("\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<+<?ID_Continue>>$/, q{Match (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<[A]+<?ID_Continue>>$/, q{Match compound (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<-<?ID_Continue>>$/ ), q{Don't match externally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<[A]-<?ID_Continue>>$/ ), q{Don't match compound inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/^<+<-ID_Continue>>$/ ), q{Don't match internally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok(!( "\c[APL FUNCTIONAL SYMBOL UP TACK JOT]"  ~~ m/^<+<?ID_Continue>>$/ ), q{Don't match unrelated (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[APL FUNCTIONAL SYMBOL UP TACK JOT]"  ~~ m/^<-<?ID_Continue>>$/, q{Match unrelated externally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[APL FUNCTIONAL SYMBOL UP TACK JOT]"  ~~ m/^<+<-ID_Continue>>$/, q{Match unrelated internally inverted (ID_Start + Mn + Mc + Nd + Pc)} );
ok("\c[APL FUNCTIONAL SYMBOL UP TACK JOT]\c[IDEOGRAPHIC ITERATION MARK]" ~~ m/<+<?ID_Continue>>/, q{Match unanchored (ID_Start + Mn + Mc + Nd + Pc)} );

# Any             # Any character


ok("\c[SYRIAC LETTER TAW]" ~~ m/^<+<?Any>>$/, q{Match (Any character)} );
ok("\c[SYRIAC LETTER TAW]" ~~ m/^<[A]+<?Any>>$/, q{Match compound (Any character)} );
ok(!( "\c[SYRIAC LETTER TAW]" ~~ m/^<-<?Any>>$/ ), q{Don't match externally inverted (Any character)} );
ok(!( "\c[SYRIAC LETTER TAW]" ~~ m/^<[A]-<?Any>>$/ ), q{Don't match compound inverted (Any character)} );
ok(!( "\c[SYRIAC LETTER TAW]" ~~ m/^<+<-Any>>$/ ), q{Don't match internally inverted (Any character)} );
ok("\c[SYRIAC LETTER TAW]" ~~ m/<+<?Any>>/, q{Match unanchored (Any character)} );

# Assigned        # Any non-Cn character (i.e. synonym for \P{Cn})


ok("\x[AC00]" ~~ m/^<+<?Assigned>>$/, q{Match (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AC00]" ~~ m/^<[A]+<?Assigned>>$/, q{Match compound (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AC00]" ~~ m/^<-<?Assigned>>$/ ), q{Don't match externally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AC00]" ~~ m/^<[A]-<?Assigned>>$/ ), q{Don't match compound inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AC00]" ~~ m/^<+<-Assigned>>$/ ), q{Don't match internally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok(!( "\x[AB08]"  ~~ m/^<+<?Assigned>>$/ ), q{Don't match unrelated (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AB08]"  ~~ m/^<-<?Assigned>>$/, q{Match unrelated externally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AB08]"  ~~ m/^<+<-Assigned>>$/, q{Match unrelated internally inverted (Any non-Cn character (i.e. synonym for \P{Cn}))} );
ok("\x[AB08]\x[AC00]" ~~ m/<+<?Assigned>>/, q{Match unanchored (Any non-Cn character (i.e. synonym for \P{Cn}))} );

# Unassigned      # Synonym for \p{Cn}


ok("\x[1738]" ~~ m/^<+<?Unassigned>>$/, q{Match (Synonym for \p{Cn})} );
ok("\x[1738]" ~~ m/^<[A]+<?Unassigned>>$/, q{Match compound (Synonym for \p{Cn})} );
ok(!( "\x[1738]" ~~ m/^<-<?Unassigned>>$/ ), q{Don't match externally inverted (Synonym for \p{Cn})} );
ok(!( "\x[1738]" ~~ m/^<[A]-<?Unassigned>>$/ ), q{Don't match compound inverted (Synonym for \p{Cn})} );
ok(!( "\x[1738]" ~~ m/^<+<-Unassigned>>$/ ), q{Don't match internally inverted (Synonym for \p{Cn})} );
ok(!( "\c[BUHID LETTER A]"  ~~ m/^<+<?Unassigned>>$/ ), q{Don't match unrelated (Synonym for \p{Cn})} );
ok("\c[BUHID LETTER A]"  ~~ m/^<-<?Unassigned>>$/, q{Match unrelated externally inverted (Synonym for \p{Cn})} );
ok("\c[BUHID LETTER A]"  ~~ m/^<+<-Unassigned>>$/, q{Match unrelated internally inverted (Synonym for \p{Cn})} );
ok("\c[BUHID LETTER A]\x[1738]" ~~ m/<+<?Unassigned>>/, q{Match unanchored (Synonym for \p{Cn})} );

# Common          # Codepoint not explicitly assigned to a script


ok("\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<+<?Common>>$/, q{Match (Codepoint not explicitly assigned to a script)} );
ok("\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]+<?Common>>$/, q{Match compound (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<-<?Common>>$/ ), q{Don't match externally inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]-<?Common>>$/ ), q{Don't match compound inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/^<+<-Common>>$/ ), q{Don't match internally inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[CJK RADICAL REPEAT]"  ~~ m/^<+<?Common>>$/ ), q{Don't match unrelated (Codepoint not explicitly assigned to a script)} );
ok("\c[CJK RADICAL REPEAT]"  ~~ m/^<-<?Common>>$/, q{Match unrelated externally inverted (Codepoint not explicitly assigned to a script)} );
ok("\c[CJK RADICAL REPEAT]"  ~~ m/^<+<-Common>>$/, q{Match unrelated internally inverted (Codepoint not explicitly assigned to a script)} );
ok(!( "\c[ARABIC END OF AYAH]" ~~ m/^<+<?Common>>$/ ), q{Don't match related (Codepoint not explicitly assigned to a script)} );
ok("\c[ARABIC END OF AYAH]" ~~ m/^<+<-Common>>$/, q{Match related internally inverted (Codepoint not explicitly assigned to a script)} );
ok("\c[ARABIC END OF AYAH]" ~~ m/^<-<?Common>>$/, q{Match related externally inverted (Codepoint not explicitly assigned to a script)} );
ok("\c[CJK RADICAL REPEAT]\c[ARABIC END OF AYAH]\c[LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE]" ~~ m/<+<?Common>>/, q{Match unanchored (Codepoint not explicitly assigned to a script)} );

# InAlphabeticPresentationForms


ok(!( "\x[5A81]"  ~~ m/^<+<?InAlphabeticPresentationForms>>$/ ), q{Don't match unrelated <?InAlphabeticPresentationForms>} );
ok("\x[5A81]"  ~~ m/^<-<?InAlphabeticPresentationForms>>$/, q{Match unrelated externally inverted <?InAlphabeticPresentationForms>} );
ok("\x[5A81]"  ~~ m/^<+<-InAlphabeticPresentationForms>>$/, q{Match unrelated internally inverted <?InAlphabeticPresentationForms>} );

# InArabic


ok("\c[ARABIC NUMBER SIGN]" ~~ m/^<+<?InArabic>>$/, q{Match <?InArabic>} );
ok("\c[ARABIC NUMBER SIGN]" ~~ m/^<[A]+<?InArabic>>$/, q{Match compound <?InArabic>} );
ok(!( "\c[ARABIC NUMBER SIGN]" ~~ m/^<-<?InArabic>>$/ ), q{Don't match externally inverted <?InArabic>} );
ok(!( "\c[ARABIC NUMBER SIGN]" ~~ m/^<[A]-<?InArabic>>$/ ), q{Don't match compound inverted <?InArabic>} );
ok(!( "\c[ARABIC NUMBER SIGN]" ~~ m/^<+<-InArabic>>$/ ), q{Don't match internally inverted <?InArabic>} );
ok(!( "\x[8D16]"  ~~ m/^<+<?InArabic>>$/ ), q{Don't match unrelated <?InArabic>} );
ok("\x[8D16]"  ~~ m/^<-<?InArabic>>$/, q{Match unrelated externally inverted <?InArabic>} );
ok("\x[8D16]"  ~~ m/^<+<-InArabic>>$/, q{Match unrelated internally inverted <?InArabic>} );
ok("\x[8D16]\c[ARABIC NUMBER SIGN]" ~~ m/<+<?InArabic>>/, q{Match unanchored <?InArabic>} );

# InArabicPresentationFormsA


ok(!( "\x[C775]"  ~~ m/^<+<?InArabicPresentationFormsA>>$/ ), q{Don't match unrelated <?InArabicPresentationFormsA>} );
ok("\x[C775]"  ~~ m/^<-<?InArabicPresentationFormsA>>$/, q{Match unrelated externally inverted <?InArabicPresentationFormsA>} );
ok("\x[C775]"  ~~ m/^<+<-InArabicPresentationFormsA>>$/, q{Match unrelated internally inverted <?InArabicPresentationFormsA>} );

# InArabicPresentationFormsB


ok(!( "\x[B2EA]"  ~~ m/^<+<?InArabicPresentationFormsB>>$/ ), q{Don't match unrelated <?InArabicPresentationFormsB>} );
ok("\x[B2EA]"  ~~ m/^<-<?InArabicPresentationFormsB>>$/, q{Match unrelated externally inverted <?InArabicPresentationFormsB>} );
ok("\x[B2EA]"  ~~ m/^<+<-InArabicPresentationFormsB>>$/, q{Match unrelated internally inverted <?InArabicPresentationFormsB>} );

# InArmenian


ok("\x[0530]" ~~ m/^<+<?InArmenian>>$/, q{Match <?InArmenian>} );
ok("\x[0530]" ~~ m/^<[A]+<?InArmenian>>$/, q{Match compound <?InArmenian>} );
ok(!( "\x[0530]" ~~ m/^<-<?InArmenian>>$/ ), q{Don't match externally inverted <?InArmenian>} );
ok(!( "\x[0530]" ~~ m/^<[A]-<?InArmenian>>$/ ), q{Don't match compound inverted <?InArmenian>} );
ok(!( "\x[0530]" ~~ m/^<+<-InArmenian>>$/ ), q{Don't match internally inverted <?InArmenian>} );
ok(!( "\c[ETHIOPIC SYLLABLE QHWAA]"  ~~ m/^<+<?InArmenian>>$/ ), q{Don't match unrelated <?InArmenian>} );
ok("\c[ETHIOPIC SYLLABLE QHWAA]"  ~~ m/^<-<?InArmenian>>$/, q{Match unrelated externally inverted <?InArmenian>} );
ok("\c[ETHIOPIC SYLLABLE QHWAA]"  ~~ m/^<+<-InArmenian>>$/, q{Match unrelated internally inverted <?InArmenian>} );
ok("\c[ETHIOPIC SYLLABLE QHWAA]\x[0530]" ~~ m/<+<?InArmenian>>/, q{Match unanchored <?InArmenian>} );

# InArrows


ok("\c[LEFTWARDS ARROW]" ~~ m/^<+<?InArrows>>$/, q{Match <?InArrows>} );
ok("\c[LEFTWARDS ARROW]" ~~ m/^<[A]+<?InArrows>>$/, q{Match compound <?InArrows>} );
ok(!( "\c[LEFTWARDS ARROW]" ~~ m/^<-<?InArrows>>$/ ), q{Don't match externally inverted <?InArrows>} );
ok(!( "\c[LEFTWARDS ARROW]" ~~ m/^<[A]-<?InArrows>>$/ ), q{Don't match compound inverted <?InArrows>} );
ok(!( "\c[LEFTWARDS ARROW]" ~~ m/^<+<-InArrows>>$/ ), q{Don't match internally inverted <?InArrows>} );
ok(!( "\x[74FA]"  ~~ m/^<+<?InArrows>>$/ ), q{Don't match unrelated <?InArrows>} );
ok("\x[74FA]"  ~~ m/^<-<?InArrows>>$/, q{Match unrelated externally inverted <?InArrows>} );
ok("\x[74FA]"  ~~ m/^<+<-InArrows>>$/, q{Match unrelated internally inverted <?InArrows>} );
ok("\x[74FA]\c[LEFTWARDS ARROW]" ~~ m/<+<?InArrows>>/, q{Match unanchored <?InArrows>} );

# InBasicLatin


ok("\c[NULL]" ~~ m/^<+<?InBasicLatin>>$/, q{Match <?InBasicLatin>} );
ok("\c[NULL]" ~~ m/^<[A]+<?InBasicLatin>>$/, q{Match compound <?InBasicLatin>} );
ok(!( "\c[NULL]" ~~ m/^<-<?InBasicLatin>>$/ ), q{Don't match externally inverted <?InBasicLatin>} );
ok(!( "\c[NULL]" ~~ m/^<[A]-<?InBasicLatin>>$/ ), q{Don't match compound inverted <?InBasicLatin>} );
ok(!( "\c[NULL]" ~~ m/^<+<-InBasicLatin>>$/ ), q{Don't match internally inverted <?InBasicLatin>} );
ok(!( "\x[ADFF]"  ~~ m/^<+<?InBasicLatin>>$/ ), q{Don't match unrelated <?InBasicLatin>} );
ok("\x[ADFF]"  ~~ m/^<-<?InBasicLatin>>$/, q{Match unrelated externally inverted <?InBasicLatin>} );
ok("\x[ADFF]"  ~~ m/^<+<-InBasicLatin>>$/, q{Match unrelated internally inverted <?InBasicLatin>} );
ok("\x[ADFF]\c[NULL]" ~~ m/<+<?InBasicLatin>>/, q{Match unanchored <?InBasicLatin>} );

# InBengali


ok("\x[0980]" ~~ m/^<+<?InBengali>>$/, q{Match <?InBengali>} );
ok("\x[0980]" ~~ m/^<[A]+<?InBengali>>$/, q{Match compound <?InBengali>} );
ok(!( "\x[0980]" ~~ m/^<-<?InBengali>>$/ ), q{Don't match externally inverted <?InBengali>} );
ok(!( "\x[0980]" ~~ m/^<[A]-<?InBengali>>$/ ), q{Don't match compound inverted <?InBengali>} );
ok(!( "\x[0980]" ~~ m/^<+<-InBengali>>$/ ), q{Don't match internally inverted <?InBengali>} );
ok(!( "\x[3409]"  ~~ m/^<+<?InBengali>>$/ ), q{Don't match unrelated <?InBengali>} );
ok("\x[3409]"  ~~ m/^<-<?InBengali>>$/, q{Match unrelated externally inverted <?InBengali>} );
ok("\x[3409]"  ~~ m/^<+<-InBengali>>$/, q{Match unrelated internally inverted <?InBengali>} );
ok("\x[3409]\x[0980]" ~~ m/<+<?InBengali>>/, q{Match unanchored <?InBengali>} );

# InBlockElements


ok("\c[UPPER HALF BLOCK]" ~~ m/^<+<?InBlockElements>>$/, q{Match <?InBlockElements>} );
ok("\c[UPPER HALF BLOCK]" ~~ m/^<[A]+<?InBlockElements>>$/, q{Match compound <?InBlockElements>} );
ok(!( "\c[UPPER HALF BLOCK]" ~~ m/^<-<?InBlockElements>>$/ ), q{Don't match externally inverted <?InBlockElements>} );
ok(!( "\c[UPPER HALF BLOCK]" ~~ m/^<[A]-<?InBlockElements>>$/ ), q{Don't match compound inverted <?InBlockElements>} );
ok(!( "\c[UPPER HALF BLOCK]" ~~ m/^<+<-InBlockElements>>$/ ), q{Don't match internally inverted <?InBlockElements>} );
ok(!( "\x[77B1]"  ~~ m/^<+<?InBlockElements>>$/ ), q{Don't match unrelated <?InBlockElements>} );
ok("\x[77B1]"  ~~ m/^<-<?InBlockElements>>$/, q{Match unrelated externally inverted <?InBlockElements>} );
ok("\x[77B1]"  ~~ m/^<+<-InBlockElements>>$/, q{Match unrelated internally inverted <?InBlockElements>} );
ok("\x[77B1]\c[UPPER HALF BLOCK]" ~~ m/<+<?InBlockElements>>/, q{Match unanchored <?InBlockElements>} );

# InBopomofo


ok("\x[3100]" ~~ m/^<+<?InBopomofo>>$/, q{Match <?InBopomofo>} );
ok("\x[3100]" ~~ m/^<[A]+<?InBopomofo>>$/, q{Match compound <?InBopomofo>} );
ok(!( "\x[3100]" ~~ m/^<-<?InBopomofo>>$/ ), q{Don't match externally inverted <?InBopomofo>} );
ok(!( "\x[3100]" ~~ m/^<[A]-<?InBopomofo>>$/ ), q{Don't match compound inverted <?InBopomofo>} );
ok(!( "\x[3100]" ~~ m/^<+<-InBopomofo>>$/ ), q{Don't match internally inverted <?InBopomofo>} );
ok(!( "\x[701E]"  ~~ m/^<+<?InBopomofo>>$/ ), q{Don't match unrelated <?InBopomofo>} );
ok("\x[701E]"  ~~ m/^<-<?InBopomofo>>$/, q{Match unrelated externally inverted <?InBopomofo>} );
ok("\x[701E]"  ~~ m/^<+<-InBopomofo>>$/, q{Match unrelated internally inverted <?InBopomofo>} );
ok("\x[701E]\x[3100]" ~~ m/<+<?InBopomofo>>/, q{Match unanchored <?InBopomofo>} );

# InBopomofoExtended


ok("\c[BOPOMOFO LETTER BU]" ~~ m/^<+<?InBopomofoExtended>>$/, q{Match <?InBopomofoExtended>} );
ok("\c[BOPOMOFO LETTER BU]" ~~ m/^<[A]+<?InBopomofoExtended>>$/, q{Match compound <?InBopomofoExtended>} );
ok(!( "\c[BOPOMOFO LETTER BU]" ~~ m/^<-<?InBopomofoExtended>>$/ ), q{Don't match externally inverted <?InBopomofoExtended>} );
ok(!( "\c[BOPOMOFO LETTER BU]" ~~ m/^<[A]-<?InBopomofoExtended>>$/ ), q{Don't match compound inverted <?InBopomofoExtended>} );
ok(!( "\c[BOPOMOFO LETTER BU]" ~~ m/^<+<-InBopomofoExtended>>$/ ), q{Don't match internally inverted <?InBopomofoExtended>} );
ok(!( "\c[YI SYLLABLE TIE]"  ~~ m/^<+<?InBopomofoExtended>>$/ ), q{Don't match unrelated <?InBopomofoExtended>} );
ok("\c[YI SYLLABLE TIE]"  ~~ m/^<-<?InBopomofoExtended>>$/, q{Match unrelated externally inverted <?InBopomofoExtended>} );
ok("\c[YI SYLLABLE TIE]"  ~~ m/^<+<-InBopomofoExtended>>$/, q{Match unrelated internally inverted <?InBopomofoExtended>} );
ok("\c[YI SYLLABLE TIE]\c[BOPOMOFO LETTER BU]" ~~ m/<+<?InBopomofoExtended>>/, q{Match unanchored <?InBopomofoExtended>} );

# InBoxDrawing


ok("\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<+<?InBoxDrawing>>$/, q{Match <?InBoxDrawing>} );
ok("\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<[A]+<?InBoxDrawing>>$/, q{Match compound <?InBoxDrawing>} );
ok(!( "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<-<?InBoxDrawing>>$/ ), q{Don't match externally inverted <?InBoxDrawing>} );
ok(!( "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<[A]-<?InBoxDrawing>>$/ ), q{Don't match compound inverted <?InBoxDrawing>} );
ok(!( "\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/^<+<-InBoxDrawing>>$/ ), q{Don't match internally inverted <?InBoxDrawing>} );
ok(!( "\x[D2FB]"  ~~ m/^<+<?InBoxDrawing>>$/ ), q{Don't match unrelated <?InBoxDrawing>} );
ok("\x[D2FB]"  ~~ m/^<-<?InBoxDrawing>>$/, q{Match unrelated externally inverted <?InBoxDrawing>} );
ok("\x[D2FB]"  ~~ m/^<+<-InBoxDrawing>>$/, q{Match unrelated internally inverted <?InBoxDrawing>} );
ok("\x[D2FB]\c[BOX DRAWINGS LIGHT HORIZONTAL]" ~~ m/<+<?InBoxDrawing>>/, q{Match unanchored <?InBoxDrawing>} );

# InBraillePatterns


ok("\c[BRAILLE PATTERN BLANK]" ~~ m/^<+<?InBraillePatterns>>$/, q{Match <?InBraillePatterns>} );
ok("\c[BRAILLE PATTERN BLANK]" ~~ m/^<[A]+<?InBraillePatterns>>$/, q{Match compound <?InBraillePatterns>} );
ok(!( "\c[BRAILLE PATTERN BLANK]" ~~ m/^<-<?InBraillePatterns>>$/ ), q{Don't match externally inverted <?InBraillePatterns>} );
ok(!( "\c[BRAILLE PATTERN BLANK]" ~~ m/^<[A]-<?InBraillePatterns>>$/ ), q{Don't match compound inverted <?InBraillePatterns>} );
ok(!( "\c[BRAILLE PATTERN BLANK]" ~~ m/^<+<-InBraillePatterns>>$/ ), q{Don't match internally inverted <?InBraillePatterns>} );
ok(!( "\x[4FE4]"  ~~ m/^<+<?InBraillePatterns>>$/ ), q{Don't match unrelated <?InBraillePatterns>} );
ok("\x[4FE4]"  ~~ m/^<-<?InBraillePatterns>>$/, q{Match unrelated externally inverted <?InBraillePatterns>} );
ok("\x[4FE4]"  ~~ m/^<+<-InBraillePatterns>>$/, q{Match unrelated internally inverted <?InBraillePatterns>} );
ok("\x[4FE4]\c[BRAILLE PATTERN BLANK]" ~~ m/<+<?InBraillePatterns>>/, q{Match unanchored <?InBraillePatterns>} );

# InBuhid


ok("\c[BUHID LETTER A]" ~~ m/^<+<?InBuhid>>$/, q{Match <?InBuhid>} );
ok("\c[BUHID LETTER A]" ~~ m/^<[A]+<?InBuhid>>$/, q{Match compound <?InBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<-<?InBuhid>>$/ ), q{Don't match externally inverted <?InBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<[A]-<?InBuhid>>$/ ), q{Don't match compound inverted <?InBuhid>} );
ok(!( "\c[BUHID LETTER A]" ~~ m/^<+<-InBuhid>>$/ ), q{Don't match internally inverted <?InBuhid>} );
ok(!( "\x[996F]"  ~~ m/^<+<?InBuhid>>$/ ), q{Don't match unrelated <?InBuhid>} );
ok("\x[996F]"  ~~ m/^<-<?InBuhid>>$/, q{Match unrelated externally inverted <?InBuhid>} );
ok("\x[996F]"  ~~ m/^<+<-InBuhid>>$/, q{Match unrelated internally inverted <?InBuhid>} );
ok("\x[996F]\c[BUHID LETTER A]" ~~ m/<+<?InBuhid>>/, q{Match unanchored <?InBuhid>} );

# InByzantineMusicalSymbols


ok(!( "\x[802A]"  ~~ m/^<+<?InByzantineMusicalSymbols>>$/ ), q{Don't match unrelated <?InByzantineMusicalSymbols>} );
ok("\x[802A]"  ~~ m/^<-<?InByzantineMusicalSymbols>>$/, q{Match unrelated externally inverted <?InByzantineMusicalSymbols>} );
ok("\x[802A]"  ~~ m/^<+<-InByzantineMusicalSymbols>>$/, q{Match unrelated internally inverted <?InByzantineMusicalSymbols>} );

# InCJKCompatibility


ok("\c[SQUARE APAATO]" ~~ m/^<+<?InCJKCompatibility>>$/, q{Match <?InCJKCompatibility>} );
ok("\c[SQUARE APAATO]" ~~ m/^<[A]+<?InCJKCompatibility>>$/, q{Match compound <?InCJKCompatibility>} );
ok(!( "\c[SQUARE APAATO]" ~~ m/^<-<?InCJKCompatibility>>$/ ), q{Don't match externally inverted <?InCJKCompatibility>} );
ok(!( "\c[SQUARE APAATO]" ~~ m/^<[A]-<?InCJKCompatibility>>$/ ), q{Don't match compound inverted <?InCJKCompatibility>} );
ok(!( "\c[SQUARE APAATO]" ~~ m/^<+<-InCJKCompatibility>>$/ ), q{Don't match internally inverted <?InCJKCompatibility>} );
ok(!( "\x[2B99]"  ~~ m/^<+<?InCJKCompatibility>>$/ ), q{Don't match unrelated <?InCJKCompatibility>} );
ok("\x[2B99]"  ~~ m/^<-<?InCJKCompatibility>>$/, q{Match unrelated externally inverted <?InCJKCompatibility>} );
ok("\x[2B99]"  ~~ m/^<+<-InCJKCompatibility>>$/, q{Match unrelated internally inverted <?InCJKCompatibility>} );
ok("\x[2B99]\c[SQUARE APAATO]" ~~ m/<+<?InCJKCompatibility>>/, q{Match unanchored <?InCJKCompatibility>} );

# InCJKCompatibilityForms


ok(!( "\x[342B]"  ~~ m/^<+<?InCJKCompatibilityForms>>$/ ), q{Don't match unrelated <?InCJKCompatibilityForms>} );
ok("\x[342B]"  ~~ m/^<-<?InCJKCompatibilityForms>>$/, q{Match unrelated externally inverted <?InCJKCompatibilityForms>} );
ok("\x[342B]"  ~~ m/^<+<-InCJKCompatibilityForms>>$/, q{Match unrelated internally inverted <?InCJKCompatibilityForms>} );

# InCJKCompatibilityIdeographs


ok(!( "\c[BLACK SQUARE]"  ~~ m/^<+<?InCJKCompatibilityIdeographs>>$/ ), q{Don't match unrelated <?InCJKCompatibilityIdeographs>} );
ok("\c[BLACK SQUARE]"  ~~ m/^<-<?InCJKCompatibilityIdeographs>>$/, q{Match unrelated externally inverted <?InCJKCompatibilityIdeographs>} );
ok("\c[BLACK SQUARE]"  ~~ m/^<+<-InCJKCompatibilityIdeographs>>$/, q{Match unrelated internally inverted <?InCJKCompatibilityIdeographs>} );

# InCJKCompatibilityIdeographsSupplement


ok(!( "\x[A90E]"  ~~ m/^<+<?InCJKCompatibilityIdeographsSupplement>>$/ ), q{Don't match unrelated <?InCJKCompatibilityIdeographsSupplement>} );
ok("\x[A90E]"  ~~ m/^<-<?InCJKCompatibilityIdeographsSupplement>>$/, q{Match unrelated externally inverted <?InCJKCompatibilityIdeographsSupplement>} );
ok("\x[A90E]"  ~~ m/^<+<-InCJKCompatibilityIdeographsSupplement>>$/, q{Match unrelated internally inverted <?InCJKCompatibilityIdeographsSupplement>} );

# InCJKRadicalsSupplement


ok("\c[CJK RADICAL REPEAT]" ~~ m/^<+<?InCJKRadicalsSupplement>>$/, q{Match <?InCJKRadicalsSupplement>} );
ok("\c[CJK RADICAL REPEAT]" ~~ m/^<[A]+<?InCJKRadicalsSupplement>>$/, q{Match compound <?InCJKRadicalsSupplement>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<-<?InCJKRadicalsSupplement>>$/ ), q{Don't match externally inverted <?InCJKRadicalsSupplement>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<[A]-<?InCJKRadicalsSupplement>>$/ ), q{Don't match compound inverted <?InCJKRadicalsSupplement>} );
ok(!( "\c[CJK RADICAL REPEAT]" ~~ m/^<+<-InCJKRadicalsSupplement>>$/ ), q{Don't match internally inverted <?InCJKRadicalsSupplement>} );
ok(!( "\x[563B]"  ~~ m/^<+<?InCJKRadicalsSupplement>>$/ ), q{Don't match unrelated <?InCJKRadicalsSupplement>} );
ok("\x[563B]"  ~~ m/^<-<?InCJKRadicalsSupplement>>$/, q{Match unrelated externally inverted <?InCJKRadicalsSupplement>} );
ok("\x[563B]"  ~~ m/^<+<-InCJKRadicalsSupplement>>$/, q{Match unrelated internally inverted <?InCJKRadicalsSupplement>} );
ok("\x[563B]\c[CJK RADICAL REPEAT]" ~~ m/<+<?InCJKRadicalsSupplement>>/, q{Match unanchored <?InCJKRadicalsSupplement>} );

# InCJKSymbolsAndPunctuation


ok("\c[IDEOGRAPHIC SPACE]" ~~ m/^<+<?InCJKSymbolsAndPunctuation>>$/, q{Match <?InCJKSymbolsAndPunctuation>} );
ok("\c[IDEOGRAPHIC SPACE]" ~~ m/^<[A]+<?InCJKSymbolsAndPunctuation>>$/, q{Match compound <?InCJKSymbolsAndPunctuation>} );
ok(!( "\c[IDEOGRAPHIC SPACE]" ~~ m/^<-<?InCJKSymbolsAndPunctuation>>$/ ), q{Don't match externally inverted <?InCJKSymbolsAndPunctuation>} );
ok(!( "\c[IDEOGRAPHIC SPACE]" ~~ m/^<[A]-<?InCJKSymbolsAndPunctuation>>$/ ), q{Don't match compound inverted <?InCJKSymbolsAndPunctuation>} );
ok(!( "\c[IDEOGRAPHIC SPACE]" ~~ m/^<+<-InCJKSymbolsAndPunctuation>>$/ ), q{Don't match internally inverted <?InCJKSymbolsAndPunctuation>} );
ok(!( "\x[3BE6]"  ~~ m/^<+<?InCJKSymbolsAndPunctuation>>$/ ), q{Don't match unrelated <?InCJKSymbolsAndPunctuation>} );
ok("\x[3BE6]"  ~~ m/^<-<?InCJKSymbolsAndPunctuation>>$/, q{Match unrelated externally inverted <?InCJKSymbolsAndPunctuation>} );
ok("\x[3BE6]"  ~~ m/^<+<-InCJKSymbolsAndPunctuation>>$/, q{Match unrelated internally inverted <?InCJKSymbolsAndPunctuation>} );
ok("\x[3BE6]\c[IDEOGRAPHIC SPACE]" ~~ m/<+<?InCJKSymbolsAndPunctuation>>/, q{Match unanchored <?InCJKSymbolsAndPunctuation>} );

# InCJKUnifiedIdeographs


ok("\x[4E00]" ~~ m/^<+<?InCJKUnifiedIdeographs>>$/, q{Match <?InCJKUnifiedIdeographs>} );
ok("\x[4E00]" ~~ m/^<[A]+<?InCJKUnifiedIdeographs>>$/, q{Match compound <?InCJKUnifiedIdeographs>} );
ok(!( "\x[4E00]" ~~ m/^<-<?InCJKUnifiedIdeographs>>$/ ), q{Don't match externally inverted <?InCJKUnifiedIdeographs>} );
ok(!( "\x[4E00]" ~~ m/^<[A]-<?InCJKUnifiedIdeographs>>$/ ), q{Don't match compound inverted <?InCJKUnifiedIdeographs>} );
ok(!( "\x[4E00]" ~~ m/^<+<-InCJKUnifiedIdeographs>>$/ ), q{Don't match internally inverted <?InCJKUnifiedIdeographs>} );
ok(!( "\x[436E]"  ~~ m/^<+<?InCJKUnifiedIdeographs>>$/ ), q{Don't match unrelated <?InCJKUnifiedIdeographs>} );
ok("\x[436E]"  ~~ m/^<-<?InCJKUnifiedIdeographs>>$/, q{Match unrelated externally inverted <?InCJKUnifiedIdeographs>} );
ok("\x[436E]"  ~~ m/^<+<-InCJKUnifiedIdeographs>>$/, q{Match unrelated internally inverted <?InCJKUnifiedIdeographs>} );
ok("\x[436E]\x[4E00]" ~~ m/<+<?InCJKUnifiedIdeographs>>/, q{Match unanchored <?InCJKUnifiedIdeographs>} );

# InCJKUnifiedIdeographsExtensionA


ok("\x[4993]" ~~ m/^<+<?InCJKUnifiedIdeographsExtensionA>>$/, q{Match <?InCJKUnifiedIdeographsExtensionA>} );
ok("\x[4993]" ~~ m/^<[A]+<?InCJKUnifiedIdeographsExtensionA>>$/, q{Match compound <?InCJKUnifiedIdeographsExtensionA>} );
ok(!( "\x[4993]" ~~ m/^<-<?InCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match externally inverted <?InCJKUnifiedIdeographsExtensionA>} );
ok(!( "\x[4993]" ~~ m/^<[A]-<?InCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match compound inverted <?InCJKUnifiedIdeographsExtensionA>} );
ok(!( "\x[4993]" ~~ m/^<+<-InCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match internally inverted <?InCJKUnifiedIdeographsExtensionA>} );
ok(!( "\c[HEXAGRAM FOR THE CREATIVE HEAVEN]"  ~~ m/^<+<?InCJKUnifiedIdeographsExtensionA>>$/ ), q{Don't match unrelated <?InCJKUnifiedIdeographsExtensionA>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]"  ~~ m/^<-<?InCJKUnifiedIdeographsExtensionA>>$/, q{Match unrelated externally inverted <?InCJKUnifiedIdeographsExtensionA>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]"  ~~ m/^<+<-InCJKUnifiedIdeographsExtensionA>>$/, q{Match unrelated internally inverted <?InCJKUnifiedIdeographsExtensionA>} );
ok("\c[HEXAGRAM FOR THE CREATIVE HEAVEN]\x[4993]" ~~ m/<+<?InCJKUnifiedIdeographsExtensionA>>/, q{Match unanchored <?InCJKUnifiedIdeographsExtensionA>} );

# InCJKUnifiedIdeographsExtensionB


ok(!( "\x[3E5A]"  ~~ m/^<+<?InCJKUnifiedIdeographsExtensionB>>$/ ), q{Don't match unrelated <?InCJKUnifiedIdeographsExtensionB>} );
ok("\x[3E5A]"  ~~ m/^<-<?InCJKUnifiedIdeographsExtensionB>>$/, q{Match unrelated externally inverted <?InCJKUnifiedIdeographsExtensionB>} );
ok("\x[3E5A]"  ~~ m/^<+<-InCJKUnifiedIdeographsExtensionB>>$/, q{Match unrelated internally inverted <?InCJKUnifiedIdeographsExtensionB>} );

# InCherokee


ok("\c[CHEROKEE LETTER A]" ~~ m/^<+<?InCherokee>>$/, q{Match <?InCherokee>} );
ok("\c[CHEROKEE LETTER A]" ~~ m/^<[A]+<?InCherokee>>$/, q{Match compound <?InCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<-<?InCherokee>>$/ ), q{Don't match externally inverted <?InCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<[A]-<?InCherokee>>$/ ), q{Don't match compound inverted <?InCherokee>} );
ok(!( "\c[CHEROKEE LETTER A]" ~~ m/^<+<-InCherokee>>$/ ), q{Don't match internally inverted <?InCherokee>} );
ok(!( "\x[B311]"  ~~ m/^<+<?InCherokee>>$/ ), q{Don't match unrelated <?InCherokee>} );
ok("\x[B311]"  ~~ m/^<-<?InCherokee>>$/, q{Match unrelated externally inverted <?InCherokee>} );
ok("\x[B311]"  ~~ m/^<+<-InCherokee>>$/, q{Match unrelated internally inverted <?InCherokee>} );
ok("\x[B311]\c[CHEROKEE LETTER A]" ~~ m/<+<?InCherokee>>/, q{Match unanchored <?InCherokee>} );

# InCombiningDiacriticalMarks


ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<?InCombiningDiacriticalMarks>>$/, q{Match <?InCombiningDiacriticalMarks>} );
ok("\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]+<?InCombiningDiacriticalMarks>>$/, q{Match compound <?InCombiningDiacriticalMarks>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<-<?InCombiningDiacriticalMarks>>$/ ), q{Don't match externally inverted <?InCombiningDiacriticalMarks>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<[A]-<?InCombiningDiacriticalMarks>>$/ ), q{Don't match compound inverted <?InCombiningDiacriticalMarks>} );
ok(!( "\c[COMBINING GRAVE ACCENT]" ~~ m/^<+<-InCombiningDiacriticalMarks>>$/ ), q{Don't match internally inverted <?InCombiningDiacriticalMarks>} );
ok(!( "\x[81B8]"  ~~ m/^<+<?InCombiningDiacriticalMarks>>$/ ), q{Don't match unrelated <?InCombiningDiacriticalMarks>} );
ok("\x[81B8]"  ~~ m/^<-<?InCombiningDiacriticalMarks>>$/, q{Match unrelated externally inverted <?InCombiningDiacriticalMarks>} );
ok("\x[81B8]"  ~~ m/^<+<-InCombiningDiacriticalMarks>>$/, q{Match unrelated internally inverted <?InCombiningDiacriticalMarks>} );
ok("\x[81B8]\c[COMBINING GRAVE ACCENT]" ~~ m/<+<?InCombiningDiacriticalMarks>>/, q{Match unanchored <?InCombiningDiacriticalMarks>} );

# InCombiningDiacriticalMarksforSymbols


ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<?InCombiningDiacriticalMarksforSymbols>>$/, q{Match <?InCombiningDiacriticalMarksforSymbols>} );
ok("\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<[A]+<?InCombiningDiacriticalMarksforSymbols>>$/, q{Match compound <?InCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<-<?InCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match externally inverted <?InCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<[A]-<?InCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match compound inverted <?InCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/^<+<-InCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match internally inverted <?InCombiningDiacriticalMarksforSymbols>} );
ok(!( "\c[YI SYLLABLE NZOX]"  ~~ m/^<+<?InCombiningDiacriticalMarksforSymbols>>$/ ), q{Don't match unrelated <?InCombiningDiacriticalMarksforSymbols>} );
ok("\c[YI SYLLABLE NZOX]"  ~~ m/^<-<?InCombiningDiacriticalMarksforSymbols>>$/, q{Match unrelated externally inverted <?InCombiningDiacriticalMarksforSymbols>} );
ok("\c[YI SYLLABLE NZOX]"  ~~ m/^<+<-InCombiningDiacriticalMarksforSymbols>>$/, q{Match unrelated internally inverted <?InCombiningDiacriticalMarksforSymbols>} );
ok("\c[YI SYLLABLE NZOX]\c[COMBINING LEFT HARPOON ABOVE]" ~~ m/<+<?InCombiningDiacriticalMarksforSymbols>>/, q{Match unanchored <?InCombiningDiacriticalMarksforSymbols>} );

# InCombiningHalfMarks


ok(!( "\x[7140]"  ~~ m/^<+<?InCombiningHalfMarks>>$/ ), q{Don't match unrelated <?InCombiningHalfMarks>} );
ok("\x[7140]"  ~~ m/^<-<?InCombiningHalfMarks>>$/, q{Match unrelated externally inverted <?InCombiningHalfMarks>} );
ok("\x[7140]"  ~~ m/^<+<-InCombiningHalfMarks>>$/, q{Match unrelated internally inverted <?InCombiningHalfMarks>} );

# InControlPictures


ok("\c[SYMBOL FOR NULL]" ~~ m/^<+<?InControlPictures>>$/, q{Match <?InControlPictures>} );
ok("\c[SYMBOL FOR NULL]" ~~ m/^<[A]+<?InControlPictures>>$/, q{Match compound <?InControlPictures>} );
ok(!( "\c[SYMBOL FOR NULL]" ~~ m/^<-<?InControlPictures>>$/ ), q{Don't match externally inverted <?InControlPictures>} );
ok(!( "\c[SYMBOL FOR NULL]" ~~ m/^<[A]-<?InControlPictures>>$/ ), q{Don't match compound inverted <?InControlPictures>} );
ok(!( "\c[SYMBOL FOR NULL]" ~~ m/^<+<-InControlPictures>>$/ ), q{Don't match internally inverted <?InControlPictures>} );
ok(!( "\x[CBBF]"  ~~ m/^<+<?InControlPictures>>$/ ), q{Don't match unrelated <?InControlPictures>} );
ok("\x[CBBF]"  ~~ m/^<-<?InControlPictures>>$/, q{Match unrelated externally inverted <?InControlPictures>} );
ok("\x[CBBF]"  ~~ m/^<+<-InControlPictures>>$/, q{Match unrelated internally inverted <?InControlPictures>} );
ok("\x[CBBF]\c[SYMBOL FOR NULL]" ~~ m/<+<?InControlPictures>>/, q{Match unanchored <?InControlPictures>} );

# InCurrencySymbols


ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<+<?InCurrencySymbols>>$/, q{Match <?InCurrencySymbols>} );
ok("\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]+<?InCurrencySymbols>>$/, q{Match compound <?InCurrencySymbols>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<-<?InCurrencySymbols>>$/ ), q{Don't match externally inverted <?InCurrencySymbols>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<[A]-<?InCurrencySymbols>>$/ ), q{Don't match compound inverted <?InCurrencySymbols>} );
ok(!( "\c[EURO-CURRENCY SIGN]" ~~ m/^<+<-InCurrencySymbols>>$/ ), q{Don't match internally inverted <?InCurrencySymbols>} );
ok(!( "\x[D040]"  ~~ m/^<+<?InCurrencySymbols>>$/ ), q{Don't match unrelated <?InCurrencySymbols>} );
ok("\x[D040]"  ~~ m/^<-<?InCurrencySymbols>>$/, q{Match unrelated externally inverted <?InCurrencySymbols>} );
ok("\x[D040]"  ~~ m/^<+<-InCurrencySymbols>>$/, q{Match unrelated internally inverted <?InCurrencySymbols>} );
ok("\x[D040]\c[EURO-CURRENCY SIGN]" ~~ m/<+<?InCurrencySymbols>>/, q{Match unanchored <?InCurrencySymbols>} );

# InCyrillic


ok("\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<+<?InCyrillic>>$/, q{Match <?InCyrillic>} );
ok("\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<[A]+<?InCyrillic>>$/, q{Match compound <?InCyrillic>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<-<?InCyrillic>>$/ ), q{Don't match externally inverted <?InCyrillic>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<[A]-<?InCyrillic>>$/ ), q{Don't match compound inverted <?InCyrillic>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/^<+<-InCyrillic>>$/ ), q{Don't match internally inverted <?InCyrillic>} );
ok(!( "\x[9C58]"  ~~ m/^<+<?InCyrillic>>$/ ), q{Don't match unrelated <?InCyrillic>} );
ok("\x[9C58]"  ~~ m/^<-<?InCyrillic>>$/, q{Match unrelated externally inverted <?InCyrillic>} );
ok("\x[9C58]"  ~~ m/^<+<-InCyrillic>>$/, q{Match unrelated internally inverted <?InCyrillic>} );
ok("\x[9C58]\c[CYRILLIC CAPITAL LETTER IE WITH GRAVE]" ~~ m/<+<?InCyrillic>>/, q{Match unanchored <?InCyrillic>} );

# InCyrillicSupplementary


ok("\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<+<?InCyrillicSupplementary>>$/, q{Match <?InCyrillicSupplementary>} );
ok("\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<[A]+<?InCyrillicSupplementary>>$/, q{Match compound <?InCyrillicSupplementary>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<-<?InCyrillicSupplementary>>$/ ), q{Don't match externally inverted <?InCyrillicSupplementary>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<[A]-<?InCyrillicSupplementary>>$/ ), q{Don't match compound inverted <?InCyrillicSupplementary>} );
ok(!( "\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/^<+<-InCyrillicSupplementary>>$/ ), q{Don't match internally inverted <?InCyrillicSupplementary>} );
ok(!( "\x[857A]"  ~~ m/^<+<?InCyrillicSupplementary>>$/ ), q{Don't match unrelated <?InCyrillicSupplementary>} );
ok("\x[857A]"  ~~ m/^<-<?InCyrillicSupplementary>>$/, q{Match unrelated externally inverted <?InCyrillicSupplementary>} );
ok("\x[857A]"  ~~ m/^<+<-InCyrillicSupplementary>>$/, q{Match unrelated internally inverted <?InCyrillicSupplementary>} );
ok("\x[857A]\c[CYRILLIC CAPITAL LETTER KOMI DE]" ~~ m/<+<?InCyrillicSupplementary>>/, q{Match unanchored <?InCyrillicSupplementary>} );

# InDeseret


ok(!( "\c[LATIN SMALL LETTER I WITH DOUBLE GRAVE]"  ~~ m/^<+<?InDeseret>>$/ ), q{Don't match unrelated <?InDeseret>} );
ok("\c[LATIN SMALL LETTER I WITH DOUBLE GRAVE]"  ~~ m/^<-<?InDeseret>>$/, q{Match unrelated externally inverted <?InDeseret>} );
ok("\c[LATIN SMALL LETTER I WITH DOUBLE GRAVE]"  ~~ m/^<+<-InDeseret>>$/, q{Match unrelated internally inverted <?InDeseret>} );

# InDevanagari


ok("\x[0900]" ~~ m/^<+<?InDevanagari>>$/, q{Match <?InDevanagari>} );
ok("\x[0900]" ~~ m/^<[A]+<?InDevanagari>>$/, q{Match compound <?InDevanagari>} );
ok(!( "\x[0900]" ~~ m/^<-<?InDevanagari>>$/ ), q{Don't match externally inverted <?InDevanagari>} );
ok(!( "\x[0900]" ~~ m/^<[A]-<?InDevanagari>>$/ ), q{Don't match compound inverted <?InDevanagari>} );
ok(!( "\x[0900]" ~~ m/^<+<-InDevanagari>>$/ ), q{Don't match internally inverted <?InDevanagari>} );
ok(!( "\x[3837]"  ~~ m/^<+<?InDevanagari>>$/ ), q{Don't match unrelated <?InDevanagari>} );
ok("\x[3837]"  ~~ m/^<-<?InDevanagari>>$/, q{Match unrelated externally inverted <?InDevanagari>} );
ok("\x[3837]"  ~~ m/^<+<-InDevanagari>>$/, q{Match unrelated internally inverted <?InDevanagari>} );
ok("\x[3837]\x[0900]" ~~ m/<+<?InDevanagari>>/, q{Match unanchored <?InDevanagari>} );

# InDingbats


ok("\x[2700]" ~~ m/^<+<?InDingbats>>$/, q{Match <?InDingbats>} );
ok("\x[2700]" ~~ m/^<[A]+<?InDingbats>>$/, q{Match compound <?InDingbats>} );
ok(!( "\x[2700]" ~~ m/^<-<?InDingbats>>$/ ), q{Don't match externally inverted <?InDingbats>} );
ok(!( "\x[2700]" ~~ m/^<[A]-<?InDingbats>>$/ ), q{Don't match compound inverted <?InDingbats>} );
ok(!( "\x[2700]" ~~ m/^<+<-InDingbats>>$/ ), q{Don't match internally inverted <?InDingbats>} );
ok(!( "\x[C9CC]"  ~~ m/^<+<?InDingbats>>$/ ), q{Don't match unrelated <?InDingbats>} );
ok("\x[C9CC]"  ~~ m/^<-<?InDingbats>>$/, q{Match unrelated externally inverted <?InDingbats>} );
ok("\x[C9CC]"  ~~ m/^<+<-InDingbats>>$/, q{Match unrelated internally inverted <?InDingbats>} );
ok("\x[C9CC]\x[2700]" ~~ m/<+<?InDingbats>>/, q{Match unanchored <?InDingbats>} );

# InEnclosedAlphanumerics


ok("\c[CIRCLED DIGIT ONE]" ~~ m/^<+<?InEnclosedAlphanumerics>>$/, q{Match <?InEnclosedAlphanumerics>} );
ok("\c[CIRCLED DIGIT ONE]" ~~ m/^<[A]+<?InEnclosedAlphanumerics>>$/, q{Match compound <?InEnclosedAlphanumerics>} );
ok(!( "\c[CIRCLED DIGIT ONE]" ~~ m/^<-<?InEnclosedAlphanumerics>>$/ ), q{Don't match externally inverted <?InEnclosedAlphanumerics>} );
ok(!( "\c[CIRCLED DIGIT ONE]" ~~ m/^<[A]-<?InEnclosedAlphanumerics>>$/ ), q{Don't match compound inverted <?InEnclosedAlphanumerics>} );
ok(!( "\c[CIRCLED DIGIT ONE]" ~~ m/^<+<-InEnclosedAlphanumerics>>$/ ), q{Don't match internally inverted <?InEnclosedAlphanumerics>} );
ok(!( "\x[CCB8]"  ~~ m/^<+<?InEnclosedAlphanumerics>>$/ ), q{Don't match unrelated <?InEnclosedAlphanumerics>} );
ok("\x[CCB8]"  ~~ m/^<-<?InEnclosedAlphanumerics>>$/, q{Match unrelated externally inverted <?InEnclosedAlphanumerics>} );
ok("\x[CCB8]"  ~~ m/^<+<-InEnclosedAlphanumerics>>$/, q{Match unrelated internally inverted <?InEnclosedAlphanumerics>} );
ok("\x[CCB8]\c[CIRCLED DIGIT ONE]" ~~ m/<+<?InEnclosedAlphanumerics>>/, q{Match unanchored <?InEnclosedAlphanumerics>} );

# InEnclosedCJKLettersAndMonths


ok("\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<+<?InEnclosedCJKLettersAndMonths>>$/, q{Match <?InEnclosedCJKLettersAndMonths>} );
ok("\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<[A]+<?InEnclosedCJKLettersAndMonths>>$/, q{Match compound <?InEnclosedCJKLettersAndMonths>} );
ok(!( "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<-<?InEnclosedCJKLettersAndMonths>>$/ ), q{Don't match externally inverted <?InEnclosedCJKLettersAndMonths>} );
ok(!( "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<[A]-<?InEnclosedCJKLettersAndMonths>>$/ ), q{Don't match compound inverted <?InEnclosedCJKLettersAndMonths>} );
ok(!( "\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/^<+<-InEnclosedCJKLettersAndMonths>>$/ ), q{Don't match internally inverted <?InEnclosedCJKLettersAndMonths>} );
ok(!( "\x[8883]"  ~~ m/^<+<?InEnclosedCJKLettersAndMonths>>$/ ), q{Don't match unrelated <?InEnclosedCJKLettersAndMonths>} );
ok("\x[8883]"  ~~ m/^<-<?InEnclosedCJKLettersAndMonths>>$/, q{Match unrelated externally inverted <?InEnclosedCJKLettersAndMonths>} );
ok("\x[8883]"  ~~ m/^<+<-InEnclosedCJKLettersAndMonths>>$/, q{Match unrelated internally inverted <?InEnclosedCJKLettersAndMonths>} );
ok("\x[8883]\c[PARENTHESIZED HANGUL KIYEOK]" ~~ m/<+<?InEnclosedCJKLettersAndMonths>>/, q{Match unanchored <?InEnclosedCJKLettersAndMonths>} );

# InEthiopic


ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<?InEthiopic>>$/, q{Match <?InEthiopic>} );
ok("\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]+<?InEthiopic>>$/, q{Match compound <?InEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<-<?InEthiopic>>$/ ), q{Don't match externally inverted <?InEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<[A]-<?InEthiopic>>$/ ), q{Don't match compound inverted <?InEthiopic>} );
ok(!( "\c[ETHIOPIC SYLLABLE HA]" ~~ m/^<+<-InEthiopic>>$/ ), q{Don't match internally inverted <?InEthiopic>} );
ok(!( "\c[MALAYALAM DIGIT NINE]"  ~~ m/^<+<?InEthiopic>>$/ ), q{Don't match unrelated <?InEthiopic>} );
ok("\c[MALAYALAM DIGIT NINE]"  ~~ m/^<-<?InEthiopic>>$/, q{Match unrelated externally inverted <?InEthiopic>} );
ok("\c[MALAYALAM DIGIT NINE]"  ~~ m/^<+<-InEthiopic>>$/, q{Match unrelated internally inverted <?InEthiopic>} );
ok("\c[MALAYALAM DIGIT NINE]\c[ETHIOPIC SYLLABLE HA]" ~~ m/<+<?InEthiopic>>/, q{Match unanchored <?InEthiopic>} );

# InGeneralPunctuation


ok("\c[EN QUAD]" ~~ m/^<+<?InGeneralPunctuation>>$/, q{Match <?InGeneralPunctuation>} );
ok("\c[EN QUAD]" ~~ m/^<[A]+<?InGeneralPunctuation>>$/, q{Match compound <?InGeneralPunctuation>} );
ok(!( "\c[EN QUAD]" ~~ m/^<-<?InGeneralPunctuation>>$/ ), q{Don't match externally inverted <?InGeneralPunctuation>} );
ok(!( "\c[EN QUAD]" ~~ m/^<[A]-<?InGeneralPunctuation>>$/ ), q{Don't match compound inverted <?InGeneralPunctuation>} );
ok(!( "\c[EN QUAD]" ~~ m/^<+<-InGeneralPunctuation>>$/ ), q{Don't match internally inverted <?InGeneralPunctuation>} );
ok(!( "\x[BBC9]"  ~~ m/^<+<?InGeneralPunctuation>>$/ ), q{Don't match unrelated <?InGeneralPunctuation>} );
ok("\x[BBC9]"  ~~ m/^<-<?InGeneralPunctuation>>$/, q{Match unrelated externally inverted <?InGeneralPunctuation>} );
ok("\x[BBC9]"  ~~ m/^<+<-InGeneralPunctuation>>$/, q{Match unrelated internally inverted <?InGeneralPunctuation>} );
ok("\x[BBC9]\c[EN QUAD]" ~~ m/<+<?InGeneralPunctuation>>/, q{Match unanchored <?InGeneralPunctuation>} );

# InGeometricShapes


ok("\c[BLACK SQUARE]" ~~ m/^<+<?InGeometricShapes>>$/, q{Match <?InGeometricShapes>} );
ok("\c[BLACK SQUARE]" ~~ m/^<[A]+<?InGeometricShapes>>$/, q{Match compound <?InGeometricShapes>} );
ok(!( "\c[BLACK SQUARE]" ~~ m/^<-<?InGeometricShapes>>$/ ), q{Don't match externally inverted <?InGeometricShapes>} );
ok(!( "\c[BLACK SQUARE]" ~~ m/^<[A]-<?InGeometricShapes>>$/ ), q{Don't match compound inverted <?InGeometricShapes>} );
ok(!( "\c[BLACK SQUARE]" ~~ m/^<+<-InGeometricShapes>>$/ ), q{Don't match internally inverted <?InGeometricShapes>} );
ok(!( "\x[C58A]"  ~~ m/^<+<?InGeometricShapes>>$/ ), q{Don't match unrelated <?InGeometricShapes>} );
ok("\x[C58A]"  ~~ m/^<-<?InGeometricShapes>>$/, q{Match unrelated externally inverted <?InGeometricShapes>} );
ok("\x[C58A]"  ~~ m/^<+<-InGeometricShapes>>$/, q{Match unrelated internally inverted <?InGeometricShapes>} );
ok("\x[C58A]\c[BLACK SQUARE]" ~~ m/<+<?InGeometricShapes>>/, q{Match unanchored <?InGeometricShapes>} );

# InGeorgian


ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<?InGeorgian>>$/, q{Match <?InGeorgian>} );
ok("\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]+<?InGeorgian>>$/, q{Match compound <?InGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<-<?InGeorgian>>$/ ), q{Don't match externally inverted <?InGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<[A]-<?InGeorgian>>$/ ), q{Don't match compound inverted <?InGeorgian>} );
ok(!( "\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/^<+<-InGeorgian>>$/ ), q{Don't match internally inverted <?InGeorgian>} );
ok(!( "\c[ARMENIAN CAPITAL LETTER BEN]"  ~~ m/^<+<?InGeorgian>>$/ ), q{Don't match unrelated <?InGeorgian>} );
ok("\c[ARMENIAN CAPITAL LETTER BEN]"  ~~ m/^<-<?InGeorgian>>$/, q{Match unrelated externally inverted <?InGeorgian>} );
ok("\c[ARMENIAN CAPITAL LETTER BEN]"  ~~ m/^<+<-InGeorgian>>$/, q{Match unrelated internally inverted <?InGeorgian>} );
ok("\c[ARMENIAN CAPITAL LETTER BEN]\c[GEORGIAN CAPITAL LETTER AN]" ~~ m/<+<?InGeorgian>>/, q{Match unanchored <?InGeorgian>} );

# InGothic


ok(!( "\x[1A5A]"  ~~ m/^<+<?InGothic>>$/ ), q{Don't match unrelated <?InGothic>} );
ok("\x[1A5A]"  ~~ m/^<-<?InGothic>>$/, q{Match unrelated externally inverted <?InGothic>} );
ok("\x[1A5A]"  ~~ m/^<+<-InGothic>>$/, q{Match unrelated internally inverted <?InGothic>} );

# InGreekExtended


ok("\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<+<?InGreekExtended>>$/, q{Match <?InGreekExtended>} );
ok("\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<[A]+<?InGreekExtended>>$/, q{Match compound <?InGreekExtended>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<-<?InGreekExtended>>$/ ), q{Don't match externally inverted <?InGreekExtended>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<[A]-<?InGreekExtended>>$/ ), q{Don't match compound inverted <?InGreekExtended>} );
ok(!( "\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/^<+<-InGreekExtended>>$/ ), q{Don't match internally inverted <?InGreekExtended>} );
ok(!( "\x[39F3]"  ~~ m/^<+<?InGreekExtended>>$/ ), q{Don't match unrelated <?InGreekExtended>} );
ok("\x[39F3]"  ~~ m/^<-<?InGreekExtended>>$/, q{Match unrelated externally inverted <?InGreekExtended>} );
ok("\x[39F3]"  ~~ m/^<+<-InGreekExtended>>$/, q{Match unrelated internally inverted <?InGreekExtended>} );
ok("\x[39F3]\c[GREEK SMALL LETTER ALPHA WITH PSILI]" ~~ m/<+<?InGreekExtended>>/, q{Match unanchored <?InGreekExtended>} );

# InGreekAndCoptic


ok("\x[0370]" ~~ m/^<+<?InGreekAndCoptic>>$/, q{Match <?InGreekAndCoptic>} );
ok("\x[0370]" ~~ m/^<[A]+<?InGreekAndCoptic>>$/, q{Match compound <?InGreekAndCoptic>} );
ok(!( "\x[0370]" ~~ m/^<-<?InGreekAndCoptic>>$/ ), q{Don't match externally inverted <?InGreekAndCoptic>} );
ok(!( "\x[0370]" ~~ m/^<[A]-<?InGreekAndCoptic>>$/ ), q{Don't match compound inverted <?InGreekAndCoptic>} );
ok(!( "\x[0370]" ~~ m/^<+<-InGreekAndCoptic>>$/ ), q{Don't match internally inverted <?InGreekAndCoptic>} );
ok(!( "\x[8CFE]"  ~~ m/^<+<?InGreekAndCoptic>>$/ ), q{Don't match unrelated <?InGreekAndCoptic>} );
ok("\x[8CFE]"  ~~ m/^<-<?InGreekAndCoptic>>$/, q{Match unrelated externally inverted <?InGreekAndCoptic>} );
ok("\x[8CFE]"  ~~ m/^<+<-InGreekAndCoptic>>$/, q{Match unrelated internally inverted <?InGreekAndCoptic>} );
ok("\x[8CFE]\x[0370]" ~~ m/<+<?InGreekAndCoptic>>/, q{Match unanchored <?InGreekAndCoptic>} );

# InGujarati


ok("\x[0A80]" ~~ m/^<+<?InGujarati>>$/, q{Match <?InGujarati>} );
ok("\x[0A80]" ~~ m/^<[A]+<?InGujarati>>$/, q{Match compound <?InGujarati>} );
ok(!( "\x[0A80]" ~~ m/^<-<?InGujarati>>$/ ), q{Don't match externally inverted <?InGujarati>} );
ok(!( "\x[0A80]" ~~ m/^<[A]-<?InGujarati>>$/ ), q{Don't match compound inverted <?InGujarati>} );
ok(!( "\x[0A80]" ~~ m/^<+<-InGujarati>>$/ ), q{Don't match internally inverted <?InGujarati>} );
ok(!( "\x[B022]"  ~~ m/^<+<?InGujarati>>$/ ), q{Don't match unrelated <?InGujarati>} );
ok("\x[B022]"  ~~ m/^<-<?InGujarati>>$/, q{Match unrelated externally inverted <?InGujarati>} );
ok("\x[B022]"  ~~ m/^<+<-InGujarati>>$/, q{Match unrelated internally inverted <?InGujarati>} );
ok("\x[B022]\x[0A80]" ~~ m/<+<?InGujarati>>/, q{Match unanchored <?InGujarati>} );

# InGurmukhi


ok("\x[0A00]" ~~ m/^<+<?InGurmukhi>>$/, q{Match <?InGurmukhi>} );
ok("\x[0A00]" ~~ m/^<[A]+<?InGurmukhi>>$/, q{Match compound <?InGurmukhi>} );
ok(!( "\x[0A00]" ~~ m/^<-<?InGurmukhi>>$/ ), q{Don't match externally inverted <?InGurmukhi>} );
ok(!( "\x[0A00]" ~~ m/^<[A]-<?InGurmukhi>>$/ ), q{Don't match compound inverted <?InGurmukhi>} );
ok(!( "\x[0A00]" ~~ m/^<+<-InGurmukhi>>$/ ), q{Don't match internally inverted <?InGurmukhi>} );
ok(!( "\x[8FC3]"  ~~ m/^<+<?InGurmukhi>>$/ ), q{Don't match unrelated <?InGurmukhi>} );
ok("\x[8FC3]"  ~~ m/^<-<?InGurmukhi>>$/, q{Match unrelated externally inverted <?InGurmukhi>} );
ok("\x[8FC3]"  ~~ m/^<+<-InGurmukhi>>$/, q{Match unrelated internally inverted <?InGurmukhi>} );
ok("\x[8FC3]\x[0A00]" ~~ m/<+<?InGurmukhi>>/, q{Match unanchored <?InGurmukhi>} );

# InHalfwidthAndFullwidthForms


ok(!( "\x[36A3]"  ~~ m/^<+<?InHalfwidthAndFullwidthForms>>$/ ), q{Don't match unrelated <?InHalfwidthAndFullwidthForms>} );
ok("\x[36A3]"  ~~ m/^<-<?InHalfwidthAndFullwidthForms>>$/, q{Match unrelated externally inverted <?InHalfwidthAndFullwidthForms>} );
ok("\x[36A3]"  ~~ m/^<+<-InHalfwidthAndFullwidthForms>>$/, q{Match unrelated internally inverted <?InHalfwidthAndFullwidthForms>} );

# InHangulCompatibilityJamo


ok("\x[3130]" ~~ m/^<+<?InHangulCompatibilityJamo>>$/, q{Match <?InHangulCompatibilityJamo>} );
ok("\x[3130]" ~~ m/^<[A]+<?InHangulCompatibilityJamo>>$/, q{Match compound <?InHangulCompatibilityJamo>} );
ok(!( "\x[3130]" ~~ m/^<-<?InHangulCompatibilityJamo>>$/ ), q{Don't match externally inverted <?InHangulCompatibilityJamo>} );
ok(!( "\x[3130]" ~~ m/^<[A]-<?InHangulCompatibilityJamo>>$/ ), q{Don't match compound inverted <?InHangulCompatibilityJamo>} );
ok(!( "\x[3130]" ~~ m/^<+<-InHangulCompatibilityJamo>>$/ ), q{Don't match internally inverted <?InHangulCompatibilityJamo>} );
ok(!( "\x[BAF0]"  ~~ m/^<+<?InHangulCompatibilityJamo>>$/ ), q{Don't match unrelated <?InHangulCompatibilityJamo>} );
ok("\x[BAF0]"  ~~ m/^<-<?InHangulCompatibilityJamo>>$/, q{Match unrelated externally inverted <?InHangulCompatibilityJamo>} );
ok("\x[BAF0]"  ~~ m/^<+<-InHangulCompatibilityJamo>>$/, q{Match unrelated internally inverted <?InHangulCompatibilityJamo>} );
ok("\x[BAF0]\x[3130]" ~~ m/<+<?InHangulCompatibilityJamo>>/, q{Match unanchored <?InHangulCompatibilityJamo>} );

# InHangulJamo


ok("\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<+<?InHangulJamo>>$/, q{Match <?InHangulJamo>} );
ok("\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<[A]+<?InHangulJamo>>$/, q{Match compound <?InHangulJamo>} );
ok(!( "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<-<?InHangulJamo>>$/ ), q{Don't match externally inverted <?InHangulJamo>} );
ok(!( "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<[A]-<?InHangulJamo>>$/ ), q{Don't match compound inverted <?InHangulJamo>} );
ok(!( "\c[HANGUL CHOSEONG KIYEOK]" ~~ m/^<+<-InHangulJamo>>$/ ), q{Don't match internally inverted <?InHangulJamo>} );
ok(!( "\x[4EB3]"  ~~ m/^<+<?InHangulJamo>>$/ ), q{Don't match unrelated <?InHangulJamo>} );
ok("\x[4EB3]"  ~~ m/^<-<?InHangulJamo>>$/, q{Match unrelated externally inverted <?InHangulJamo>} );
ok("\x[4EB3]"  ~~ m/^<+<-InHangulJamo>>$/, q{Match unrelated internally inverted <?InHangulJamo>} );
ok("\x[4EB3]\c[HANGUL CHOSEONG KIYEOK]" ~~ m/<+<?InHangulJamo>>/, q{Match unanchored <?InHangulJamo>} );

# InHangulSyllables


ok("\x[AC00]" ~~ m/^<+<?InHangulSyllables>>$/, q{Match <?InHangulSyllables>} );
ok("\x[AC00]" ~~ m/^<[A]+<?InHangulSyllables>>$/, q{Match compound <?InHangulSyllables>} );
ok(!( "\x[AC00]" ~~ m/^<-<?InHangulSyllables>>$/ ), q{Don't match externally inverted <?InHangulSyllables>} );
ok(!( "\x[AC00]" ~~ m/^<[A]-<?InHangulSyllables>>$/ ), q{Don't match compound inverted <?InHangulSyllables>} );
ok(!( "\x[AC00]" ~~ m/^<+<-InHangulSyllables>>$/ ), q{Don't match internally inverted <?InHangulSyllables>} );
ok(!( "\x[7D7E]"  ~~ m/^<+<?InHangulSyllables>>$/ ), q{Don't match unrelated <?InHangulSyllables>} );
ok("\x[7D7E]"  ~~ m/^<-<?InHangulSyllables>>$/, q{Match unrelated externally inverted <?InHangulSyllables>} );
ok("\x[7D7E]"  ~~ m/^<+<-InHangulSyllables>>$/, q{Match unrelated internally inverted <?InHangulSyllables>} );
ok("\x[7D7E]\x[AC00]" ~~ m/<+<?InHangulSyllables>>/, q{Match unanchored <?InHangulSyllables>} );

# InHanunoo


ok("\c[HANUNOO LETTER A]" ~~ m/^<+<?InHanunoo>>$/, q{Match <?InHanunoo>} );
ok("\c[HANUNOO LETTER A]" ~~ m/^<[A]+<?InHanunoo>>$/, q{Match compound <?InHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<-<?InHanunoo>>$/ ), q{Don't match externally inverted <?InHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<[A]-<?InHanunoo>>$/ ), q{Don't match compound inverted <?InHanunoo>} );
ok(!( "\c[HANUNOO LETTER A]" ~~ m/^<+<-InHanunoo>>$/ ), q{Don't match internally inverted <?InHanunoo>} );
ok(!( "\x[BD8A]"  ~~ m/^<+<?InHanunoo>>$/ ), q{Don't match unrelated <?InHanunoo>} );
ok("\x[BD8A]"  ~~ m/^<-<?InHanunoo>>$/, q{Match unrelated externally inverted <?InHanunoo>} );
ok("\x[BD8A]"  ~~ m/^<+<-InHanunoo>>$/, q{Match unrelated internally inverted <?InHanunoo>} );
ok("\x[BD8A]\c[HANUNOO LETTER A]" ~~ m/<+<?InHanunoo>>/, q{Match unanchored <?InHanunoo>} );

# InHebrew


ok("\x[0590]" ~~ m/^<+<?InHebrew>>$/, q{Match <?InHebrew>} );
ok("\x[0590]" ~~ m/^<[A]+<?InHebrew>>$/, q{Match compound <?InHebrew>} );
ok(!( "\x[0590]" ~~ m/^<-<?InHebrew>>$/ ), q{Don't match externally inverted <?InHebrew>} );
ok(!( "\x[0590]" ~~ m/^<[A]-<?InHebrew>>$/ ), q{Don't match compound inverted <?InHebrew>} );
ok(!( "\x[0590]" ~~ m/^<+<-InHebrew>>$/ ), q{Don't match internally inverted <?InHebrew>} );
ok(!( "\x[7EB3]"  ~~ m/^<+<?InHebrew>>$/ ), q{Don't match unrelated <?InHebrew>} );
ok("\x[7EB3]"  ~~ m/^<-<?InHebrew>>$/, q{Match unrelated externally inverted <?InHebrew>} );
ok("\x[7EB3]"  ~~ m/^<+<-InHebrew>>$/, q{Match unrelated internally inverted <?InHebrew>} );
ok("\x[7EB3]\x[0590]" ~~ m/<+<?InHebrew>>/, q{Match unanchored <?InHebrew>} );

# InHighPrivateUseSurrogates


ok(!( "\x[9EC6]"  ~~ m/^<+<?InHighPrivateUseSurrogates>>$/ ), q{Don't match unrelated <?InHighPrivateUseSurrogates>} );
ok("\x[9EC6]"  ~~ m/^<-<?InHighPrivateUseSurrogates>>$/, q{Match unrelated externally inverted <?InHighPrivateUseSurrogates>} );
ok("\x[9EC6]"  ~~ m/^<+<-InHighPrivateUseSurrogates>>$/, q{Match unrelated internally inverted <?InHighPrivateUseSurrogates>} );

# InHighSurrogates


ok(!( "\x[BC8F]"  ~~ m/^<+<?InHighSurrogates>>$/ ), q{Don't match unrelated <?InHighSurrogates>} );
ok("\x[BC8F]"  ~~ m/^<-<?InHighSurrogates>>$/, q{Match unrelated externally inverted <?InHighSurrogates>} );
ok("\x[BC8F]"  ~~ m/^<+<-InHighSurrogates>>$/, q{Match unrelated internally inverted <?InHighSurrogates>} );

# InHiragana


ok("\x[3040]" ~~ m/^<+<?InHiragana>>$/, q{Match <?InHiragana>} );
ok("\x[3040]" ~~ m/^<[A]+<?InHiragana>>$/, q{Match compound <?InHiragana>} );
ok(!( "\x[3040]" ~~ m/^<-<?InHiragana>>$/ ), q{Don't match externally inverted <?InHiragana>} );
ok(!( "\x[3040]" ~~ m/^<[A]-<?InHiragana>>$/ ), q{Don't match compound inverted <?InHiragana>} );
ok(!( "\x[3040]" ~~ m/^<+<-InHiragana>>$/ ), q{Don't match internally inverted <?InHiragana>} );
ok(!( "\c[ARABIC SIGN ALAYHE ASSALLAM]"  ~~ m/^<+<?InHiragana>>$/ ), q{Don't match unrelated <?InHiragana>} );
ok("\c[ARABIC SIGN ALAYHE ASSALLAM]"  ~~ m/^<-<?InHiragana>>$/, q{Match unrelated externally inverted <?InHiragana>} );
ok("\c[ARABIC SIGN ALAYHE ASSALLAM]"  ~~ m/^<+<-InHiragana>>$/, q{Match unrelated internally inverted <?InHiragana>} );
ok("\c[ARABIC SIGN ALAYHE ASSALLAM]\x[3040]" ~~ m/<+<?InHiragana>>/, q{Match unanchored <?InHiragana>} );

# InIPAExtensions


ok("\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<+<?InIPAExtensions>>$/, q{Match <?InIPAExtensions>} );
ok("\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<[A]+<?InIPAExtensions>>$/, q{Match compound <?InIPAExtensions>} );
ok(!( "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<-<?InIPAExtensions>>$/ ), q{Don't match externally inverted <?InIPAExtensions>} );
ok(!( "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<[A]-<?InIPAExtensions>>$/ ), q{Don't match compound inverted <?InIPAExtensions>} );
ok(!( "\c[LATIN SMALL LETTER TURNED A]" ~~ m/^<+<-InIPAExtensions>>$/ ), q{Don't match internally inverted <?InIPAExtensions>} );
ok(!( "\x[0DFC]"  ~~ m/^<+<?InIPAExtensions>>$/ ), q{Don't match unrelated <?InIPAExtensions>} );
ok("\x[0DFC]"  ~~ m/^<-<?InIPAExtensions>>$/, q{Match unrelated externally inverted <?InIPAExtensions>} );
ok("\x[0DFC]"  ~~ m/^<+<-InIPAExtensions>>$/, q{Match unrelated internally inverted <?InIPAExtensions>} );
ok("\x[0DFC]\c[LATIN SMALL LETTER TURNED A]" ~~ m/<+<?InIPAExtensions>>/, q{Match unanchored <?InIPAExtensions>} );

# InIdeographicDescriptionCharacters


ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<?InIdeographicDescriptionCharacters>>$/, q{Match <?InIdeographicDescriptionCharacters>} );
ok("\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]+<?InIdeographicDescriptionCharacters>>$/, q{Match compound <?InIdeographicDescriptionCharacters>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<-<?InIdeographicDescriptionCharacters>>$/ ), q{Don't match externally inverted <?InIdeographicDescriptionCharacters>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<[A]-<?InIdeographicDescriptionCharacters>>$/ ), q{Don't match compound inverted <?InIdeographicDescriptionCharacters>} );
ok(!( "\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/^<+<-InIdeographicDescriptionCharacters>>$/ ), q{Don't match internally inverted <?InIdeographicDescriptionCharacters>} );
ok(!( "\c[LATIN CAPITAL LETTER AE]"  ~~ m/^<+<?InIdeographicDescriptionCharacters>>$/ ), q{Don't match unrelated <?InIdeographicDescriptionCharacters>} );
ok("\c[LATIN CAPITAL LETTER AE]"  ~~ m/^<-<?InIdeographicDescriptionCharacters>>$/, q{Match unrelated externally inverted <?InIdeographicDescriptionCharacters>} );
ok("\c[LATIN CAPITAL LETTER AE]"  ~~ m/^<+<-InIdeographicDescriptionCharacters>>$/, q{Match unrelated internally inverted <?InIdeographicDescriptionCharacters>} );
ok("\c[LATIN CAPITAL LETTER AE]\c[IDEOGRAPHIC DESCRIPTION CHARACTER LEFT TO RIGHT]" ~~ m/<+<?InIdeographicDescriptionCharacters>>/, q{Match unanchored <?InIdeographicDescriptionCharacters>} );

# InKanbun


ok("\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<+<?InKanbun>>$/, q{Match <?InKanbun>} );
ok("\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<[A]+<?InKanbun>>$/, q{Match compound <?InKanbun>} );
ok(!( "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<-<?InKanbun>>$/ ), q{Don't match externally inverted <?InKanbun>} );
ok(!( "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<[A]-<?InKanbun>>$/ ), q{Don't match compound inverted <?InKanbun>} );
ok(!( "\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/^<+<-InKanbun>>$/ ), q{Don't match internally inverted <?InKanbun>} );
ok(!( "\x[93AB]"  ~~ m/^<+<?InKanbun>>$/ ), q{Don't match unrelated <?InKanbun>} );
ok("\x[93AB]"  ~~ m/^<-<?InKanbun>>$/, q{Match unrelated externally inverted <?InKanbun>} );
ok("\x[93AB]"  ~~ m/^<+<-InKanbun>>$/, q{Match unrelated internally inverted <?InKanbun>} );
ok("\x[93AB]\c[IDEOGRAPHIC ANNOTATION LINKING MARK]" ~~ m/<+<?InKanbun>>/, q{Match unanchored <?InKanbun>} );

# InKangxiRadicals


ok("\c[KANGXI RADICAL ONE]" ~~ m/^<+<?InKangxiRadicals>>$/, q{Match <?InKangxiRadicals>} );
ok("\c[KANGXI RADICAL ONE]" ~~ m/^<[A]+<?InKangxiRadicals>>$/, q{Match compound <?InKangxiRadicals>} );
ok(!( "\c[KANGXI RADICAL ONE]" ~~ m/^<-<?InKangxiRadicals>>$/ ), q{Don't match externally inverted <?InKangxiRadicals>} );
ok(!( "\c[KANGXI RADICAL ONE]" ~~ m/^<[A]-<?InKangxiRadicals>>$/ ), q{Don't match compound inverted <?InKangxiRadicals>} );
ok(!( "\c[KANGXI RADICAL ONE]" ~~ m/^<+<-InKangxiRadicals>>$/ ), q{Don't match internally inverted <?InKangxiRadicals>} );
ok(!( "\x[363D]"  ~~ m/^<+<?InKangxiRadicals>>$/ ), q{Don't match unrelated <?InKangxiRadicals>} );
ok("\x[363D]"  ~~ m/^<-<?InKangxiRadicals>>$/, q{Match unrelated externally inverted <?InKangxiRadicals>} );
ok("\x[363D]"  ~~ m/^<+<-InKangxiRadicals>>$/, q{Match unrelated internally inverted <?InKangxiRadicals>} );
ok("\x[363D]\c[KANGXI RADICAL ONE]" ~~ m/<+<?InKangxiRadicals>>/, q{Match unanchored <?InKangxiRadicals>} );

# InKannada


ok("\x[0C80]" ~~ m/^<+<?InKannada>>$/, q{Match <?InKannada>} );
ok("\x[0C80]" ~~ m/^<[A]+<?InKannada>>$/, q{Match compound <?InKannada>} );
ok(!( "\x[0C80]" ~~ m/^<-<?InKannada>>$/ ), q{Don't match externally inverted <?InKannada>} );
ok(!( "\x[0C80]" ~~ m/^<[A]-<?InKannada>>$/ ), q{Don't match compound inverted <?InKannada>} );
ok(!( "\x[0C80]" ~~ m/^<+<-InKannada>>$/ ), q{Don't match internally inverted <?InKannada>} );
ok(!( "\x[9093]"  ~~ m/^<+<?InKannada>>$/ ), q{Don't match unrelated <?InKannada>} );
ok("\x[9093]"  ~~ m/^<-<?InKannada>>$/, q{Match unrelated externally inverted <?InKannada>} );
ok("\x[9093]"  ~~ m/^<+<-InKannada>>$/, q{Match unrelated internally inverted <?InKannada>} );
ok("\x[9093]\x[0C80]" ~~ m/<+<?InKannada>>/, q{Match unanchored <?InKannada>} );

# InKatakana


ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<?InKatakana>>$/, q{Match <?InKatakana>} );
ok("\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]+<?InKatakana>>$/, q{Match compound <?InKatakana>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<-<?InKatakana>>$/ ), q{Don't match externally inverted <?InKatakana>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<[A]-<?InKatakana>>$/ ), q{Don't match compound inverted <?InKatakana>} );
ok(!( "\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/^<+<-InKatakana>>$/ ), q{Don't match internally inverted <?InKatakana>} );
ok(!( "\x[75DA]"  ~~ m/^<+<?InKatakana>>$/ ), q{Don't match unrelated <?InKatakana>} );
ok("\x[75DA]"  ~~ m/^<-<?InKatakana>>$/, q{Match unrelated externally inverted <?InKatakana>} );
ok("\x[75DA]"  ~~ m/^<+<-InKatakana>>$/, q{Match unrelated internally inverted <?InKatakana>} );
ok("\x[75DA]\c[KATAKANA-HIRAGANA DOUBLE HYPHEN]" ~~ m/<+<?InKatakana>>/, q{Match unanchored <?InKatakana>} );

# InKatakanaPhoneticExtensions


ok("\c[KATAKANA LETTER SMALL KU]" ~~ m/^<+<?InKatakanaPhoneticExtensions>>$/, q{Match <?InKatakanaPhoneticExtensions>} );
ok("\c[KATAKANA LETTER SMALL KU]" ~~ m/^<[A]+<?InKatakanaPhoneticExtensions>>$/, q{Match compound <?InKatakanaPhoneticExtensions>} );
ok(!( "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<-<?InKatakanaPhoneticExtensions>>$/ ), q{Don't match externally inverted <?InKatakanaPhoneticExtensions>} );
ok(!( "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<[A]-<?InKatakanaPhoneticExtensions>>$/ ), q{Don't match compound inverted <?InKatakanaPhoneticExtensions>} );
ok(!( "\c[KATAKANA LETTER SMALL KU]" ~~ m/^<+<-InKatakanaPhoneticExtensions>>$/ ), q{Don't match internally inverted <?InKatakanaPhoneticExtensions>} );
ok(!( "\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]"  ~~ m/^<+<?InKatakanaPhoneticExtensions>>$/ ), q{Don't match unrelated <?InKatakanaPhoneticExtensions>} );
ok("\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]"  ~~ m/^<-<?InKatakanaPhoneticExtensions>>$/, q{Match unrelated externally inverted <?InKatakanaPhoneticExtensions>} );
ok("\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]"  ~~ m/^<+<-InKatakanaPhoneticExtensions>>$/, q{Match unrelated internally inverted <?InKatakanaPhoneticExtensions>} );
ok("\c[GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA]\c[KATAKANA LETTER SMALL KU]" ~~ m/<+<?InKatakanaPhoneticExtensions>>/, q{Match unanchored <?InKatakanaPhoneticExtensions>} );

# InKhmer


ok("\c[KHMER LETTER KA]" ~~ m/^<+<?InKhmer>>$/, q{Match <?InKhmer>} );
ok("\c[KHMER LETTER KA]" ~~ m/^<[A]+<?InKhmer>>$/, q{Match compound <?InKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<-<?InKhmer>>$/ ), q{Don't match externally inverted <?InKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<[A]-<?InKhmer>>$/ ), q{Don't match compound inverted <?InKhmer>} );
ok(!( "\c[KHMER LETTER KA]" ~~ m/^<+<-InKhmer>>$/ ), q{Don't match internally inverted <?InKhmer>} );
ok(!( "\x[508C]"  ~~ m/^<+<?InKhmer>>$/ ), q{Don't match unrelated <?InKhmer>} );
ok("\x[508C]"  ~~ m/^<-<?InKhmer>>$/, q{Match unrelated externally inverted <?InKhmer>} );
ok("\x[508C]"  ~~ m/^<+<-InKhmer>>$/, q{Match unrelated internally inverted <?InKhmer>} );
ok("\x[508C]\c[KHMER LETTER KA]" ~~ m/<+<?InKhmer>>/, q{Match unanchored <?InKhmer>} );

# InLao


ok("\x[0E80]" ~~ m/^<+<?InLao>>$/, q{Match <?InLao>} );
ok("\x[0E80]" ~~ m/^<[A]+<?InLao>>$/, q{Match compound <?InLao>} );
ok(!( "\x[0E80]" ~~ m/^<-<?InLao>>$/ ), q{Don't match externally inverted <?InLao>} );
ok(!( "\x[0E80]" ~~ m/^<[A]-<?InLao>>$/ ), q{Don't match compound inverted <?InLao>} );
ok(!( "\x[0E80]" ~~ m/^<+<-InLao>>$/ ), q{Don't match internally inverted <?InLao>} );
ok(!( "\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]"  ~~ m/^<+<?InLao>>$/ ), q{Don't match unrelated <?InLao>} );
ok("\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]"  ~~ m/^<-<?InLao>>$/, q{Match unrelated externally inverted <?InLao>} );
ok("\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]"  ~~ m/^<+<-InLao>>$/, q{Match unrelated internally inverted <?InLao>} );
ok("\c[PARENTHESIZED IDEOGRAPH CONGRATULATION]\x[0E80]" ~~ m/<+<?InLao>>/, q{Match unanchored <?InLao>} );

# InLatin1Supplement


ok("\x[0080]" ~~ m/^<+<?InLatin1Supplement>>$/, q{Match <?InLatin1Supplement>} );
ok("\x[0080]" ~~ m/^<[A]+<?InLatin1Supplement>>$/, q{Match compound <?InLatin1Supplement>} );
ok(!( "\x[0080]" ~~ m/^<-<?InLatin1Supplement>>$/ ), q{Don't match externally inverted <?InLatin1Supplement>} );
ok(!( "\x[0080]" ~~ m/^<[A]-<?InLatin1Supplement>>$/ ), q{Don't match compound inverted <?InLatin1Supplement>} );
ok(!( "\x[0080]" ~~ m/^<+<-InLatin1Supplement>>$/ ), q{Don't match internally inverted <?InLatin1Supplement>} );
ok(!( "\x[3A43]"  ~~ m/^<+<?InLatin1Supplement>>$/ ), q{Don't match unrelated <?InLatin1Supplement>} );
ok("\x[3A43]"  ~~ m/^<-<?InLatin1Supplement>>$/, q{Match unrelated externally inverted <?InLatin1Supplement>} );
ok("\x[3A43]"  ~~ m/^<+<-InLatin1Supplement>>$/, q{Match unrelated internally inverted <?InLatin1Supplement>} );
ok("\x[3A43]\x[0080]" ~~ m/<+<?InLatin1Supplement>>/, q{Match unanchored <?InLatin1Supplement>} );

# InLatinExtendedA


ok("\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<+<?InLatinExtendedA>>$/, q{Match <?InLatinExtendedA>} );
ok("\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<[A]+<?InLatinExtendedA>>$/, q{Match compound <?InLatinExtendedA>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<-<?InLatinExtendedA>>$/ ), q{Don't match externally inverted <?InLatinExtendedA>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<[A]-<?InLatinExtendedA>>$/ ), q{Don't match compound inverted <?InLatinExtendedA>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/^<+<-InLatinExtendedA>>$/ ), q{Don't match internally inverted <?InLatinExtendedA>} );
ok(!( "\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]"  ~~ m/^<+<?InLatinExtendedA>>$/ ), q{Don't match unrelated <?InLatinExtendedA>} );
ok("\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]"  ~~ m/^<-<?InLatinExtendedA>>$/, q{Match unrelated externally inverted <?InLatinExtendedA>} );
ok("\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]"  ~~ m/^<+<-InLatinExtendedA>>$/, q{Match unrelated internally inverted <?InLatinExtendedA>} );
ok("\c[ARROW POINTING RIGHTWARDS THEN CURVING UPWARDS]\c[LATIN CAPITAL LETTER A WITH MACRON]" ~~ m/<+<?InLatinExtendedA>>/, q{Match unanchored <?InLatinExtendedA>} );

# InLatinExtendedAdditional


ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<?InLatinExtendedAdditional>>$/, q{Match <?InLatinExtendedAdditional>} );
ok("\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]+<?InLatinExtendedAdditional>>$/, q{Match compound <?InLatinExtendedAdditional>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<-<?InLatinExtendedAdditional>>$/ ), q{Don't match externally inverted <?InLatinExtendedAdditional>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<[A]-<?InLatinExtendedAdditional>>$/ ), q{Don't match compound inverted <?InLatinExtendedAdditional>} );
ok(!( "\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/^<+<-InLatinExtendedAdditional>>$/ ), q{Don't match internally inverted <?InLatinExtendedAdditional>} );
ok(!( "\x[36E7]"  ~~ m/^<+<?InLatinExtendedAdditional>>$/ ), q{Don't match unrelated <?InLatinExtendedAdditional>} );
ok("\x[36E7]"  ~~ m/^<-<?InLatinExtendedAdditional>>$/, q{Match unrelated externally inverted <?InLatinExtendedAdditional>} );
ok("\x[36E7]"  ~~ m/^<+<-InLatinExtendedAdditional>>$/, q{Match unrelated internally inverted <?InLatinExtendedAdditional>} );
ok("\x[36E7]\c[LATIN CAPITAL LETTER A WITH RING BELOW]" ~~ m/<+<?InLatinExtendedAdditional>>/, q{Match unanchored <?InLatinExtendedAdditional>} );

# InLatinExtendedB


ok("\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<+<?InLatinExtendedB>>$/, q{Match <?InLatinExtendedB>} );
ok("\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<[A]+<?InLatinExtendedB>>$/, q{Match compound <?InLatinExtendedB>} );
ok(!( "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<-<?InLatinExtendedB>>$/ ), q{Don't match externally inverted <?InLatinExtendedB>} );
ok(!( "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<[A]-<?InLatinExtendedB>>$/ ), q{Don't match compound inverted <?InLatinExtendedB>} );
ok(!( "\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/^<+<-InLatinExtendedB>>$/ ), q{Don't match internally inverted <?InLatinExtendedB>} );
ok(!( "\x[877E]"  ~~ m/^<+<?InLatinExtendedB>>$/ ), q{Don't match unrelated <?InLatinExtendedB>} );
ok("\x[877E]"  ~~ m/^<-<?InLatinExtendedB>>$/, q{Match unrelated externally inverted <?InLatinExtendedB>} );
ok("\x[877E]"  ~~ m/^<+<-InLatinExtendedB>>$/, q{Match unrelated internally inverted <?InLatinExtendedB>} );
ok("\x[877E]\c[LATIN SMALL LETTER B WITH STROKE]" ~~ m/<+<?InLatinExtendedB>>/, q{Match unanchored <?InLatinExtendedB>} );

# InLetterlikeSymbols


ok("\c[ACCOUNT OF]" ~~ m/^<+<?InLetterlikeSymbols>>$/, q{Match <?InLetterlikeSymbols>} );
ok("\c[ACCOUNT OF]" ~~ m/^<[A]+<?InLetterlikeSymbols>>$/, q{Match compound <?InLetterlikeSymbols>} );
ok(!( "\c[ACCOUNT OF]" ~~ m/^<-<?InLetterlikeSymbols>>$/ ), q{Don't match externally inverted <?InLetterlikeSymbols>} );
ok(!( "\c[ACCOUNT OF]" ~~ m/^<[A]-<?InLetterlikeSymbols>>$/ ), q{Don't match compound inverted <?InLetterlikeSymbols>} );
ok(!( "\c[ACCOUNT OF]" ~~ m/^<+<-InLetterlikeSymbols>>$/ ), q{Don't match internally inverted <?InLetterlikeSymbols>} );
ok(!( "\c[CIRCLED IDEOGRAPH TWO]"  ~~ m/^<+<?InLetterlikeSymbols>>$/ ), q{Don't match unrelated <?InLetterlikeSymbols>} );
ok("\c[CIRCLED IDEOGRAPH TWO]"  ~~ m/^<-<?InLetterlikeSymbols>>$/, q{Match unrelated externally inverted <?InLetterlikeSymbols>} );
ok("\c[CIRCLED IDEOGRAPH TWO]"  ~~ m/^<+<-InLetterlikeSymbols>>$/, q{Match unrelated internally inverted <?InLetterlikeSymbols>} );
ok("\c[CIRCLED IDEOGRAPH TWO]\c[ACCOUNT OF]" ~~ m/<+<?InLetterlikeSymbols>>/, q{Match unanchored <?InLetterlikeSymbols>} );

# InLowSurrogates


ok(!( "\x[B611]"  ~~ m/^<+<?InLowSurrogates>>$/ ), q{Don't match unrelated <?InLowSurrogates>} );
ok("\x[B611]"  ~~ m/^<-<?InLowSurrogates>>$/, q{Match unrelated externally inverted <?InLowSurrogates>} );
ok("\x[B611]"  ~~ m/^<+<-InLowSurrogates>>$/, q{Match unrelated internally inverted <?InLowSurrogates>} );

# InMalayalam


ok("\x[0D00]" ~~ m/^<+<?InMalayalam>>$/, q{Match <?InMalayalam>} );
ok("\x[0D00]" ~~ m/^<[A]+<?InMalayalam>>$/, q{Match compound <?InMalayalam>} );
ok(!( "\x[0D00]" ~~ m/^<-<?InMalayalam>>$/ ), q{Don't match externally inverted <?InMalayalam>} );
ok(!( "\x[0D00]" ~~ m/^<[A]-<?InMalayalam>>$/ ), q{Don't match compound inverted <?InMalayalam>} );
ok(!( "\x[0D00]" ~~ m/^<+<-InMalayalam>>$/ ), q{Don't match internally inverted <?InMalayalam>} );
ok(!( "\x[C011]"  ~~ m/^<+<?InMalayalam>>$/ ), q{Don't match unrelated <?InMalayalam>} );
ok("\x[C011]"  ~~ m/^<-<?InMalayalam>>$/, q{Match unrelated externally inverted <?InMalayalam>} );
ok("\x[C011]"  ~~ m/^<+<-InMalayalam>>$/, q{Match unrelated internally inverted <?InMalayalam>} );
ok("\x[C011]\x[0D00]" ~~ m/<+<?InMalayalam>>/, q{Match unanchored <?InMalayalam>} );

# InMathematicalAlphanumericSymbols


ok(!( "\x[73FA]"  ~~ m/^<+<?InMathematicalAlphanumericSymbols>>$/ ), q{Don't match unrelated <?InMathematicalAlphanumericSymbols>} );
ok("\x[73FA]"  ~~ m/^<-<?InMathematicalAlphanumericSymbols>>$/, q{Match unrelated externally inverted <?InMathematicalAlphanumericSymbols>} );
ok("\x[73FA]"  ~~ m/^<+<-InMathematicalAlphanumericSymbols>>$/, q{Match unrelated internally inverted <?InMathematicalAlphanumericSymbols>} );

# InMathematicalOperators


ok("\c[FOR ALL]" ~~ m/^<+<?InMathematicalOperators>>$/, q{Match <?InMathematicalOperators>} );
ok("\c[FOR ALL]" ~~ m/^<[A]+<?InMathematicalOperators>>$/, q{Match compound <?InMathematicalOperators>} );
ok(!( "\c[FOR ALL]" ~~ m/^<-<?InMathematicalOperators>>$/ ), q{Don't match externally inverted <?InMathematicalOperators>} );
ok(!( "\c[FOR ALL]" ~~ m/^<[A]-<?InMathematicalOperators>>$/ ), q{Don't match compound inverted <?InMathematicalOperators>} );
ok(!( "\c[FOR ALL]" ~~ m/^<+<-InMathematicalOperators>>$/ ), q{Don't match internally inverted <?InMathematicalOperators>} );
ok(!( "\x[B389]"  ~~ m/^<+<?InMathematicalOperators>>$/ ), q{Don't match unrelated <?InMathematicalOperators>} );
ok("\x[B389]"  ~~ m/^<-<?InMathematicalOperators>>$/, q{Match unrelated externally inverted <?InMathematicalOperators>} );
ok("\x[B389]"  ~~ m/^<+<-InMathematicalOperators>>$/, q{Match unrelated internally inverted <?InMathematicalOperators>} );
ok("\x[B389]\c[FOR ALL]" ~~ m/<+<?InMathematicalOperators>>/, q{Match unanchored <?InMathematicalOperators>} );

# InMiscellaneousMathematicalSymbolsA


ok("\x[27C0]" ~~ m/^<+<?InMiscellaneousMathematicalSymbolsA>>$/, q{Match <?InMiscellaneousMathematicalSymbolsA>} );
ok("\x[27C0]" ~~ m/^<[A]+<?InMiscellaneousMathematicalSymbolsA>>$/, q{Match compound <?InMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[27C0]" ~~ m/^<-<?InMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match externally inverted <?InMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[27C0]" ~~ m/^<[A]-<?InMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match compound inverted <?InMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[27C0]" ~~ m/^<+<-InMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match internally inverted <?InMiscellaneousMathematicalSymbolsA>} );
ok(!( "\x[AAB2]"  ~~ m/^<+<?InMiscellaneousMathematicalSymbolsA>>$/ ), q{Don't match unrelated <?InMiscellaneousMathematicalSymbolsA>} );
ok("\x[AAB2]"  ~~ m/^<-<?InMiscellaneousMathematicalSymbolsA>>$/, q{Match unrelated externally inverted <?InMiscellaneousMathematicalSymbolsA>} );
ok("\x[AAB2]"  ~~ m/^<+<-InMiscellaneousMathematicalSymbolsA>>$/, q{Match unrelated internally inverted <?InMiscellaneousMathematicalSymbolsA>} );
ok("\x[AAB2]\x[27C0]" ~~ m/<+<?InMiscellaneousMathematicalSymbolsA>>/, q{Match unanchored <?InMiscellaneousMathematicalSymbolsA>} );

# InMiscellaneousMathematicalSymbolsB


ok("\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<+<?InMiscellaneousMathematicalSymbolsB>>$/, q{Match <?InMiscellaneousMathematicalSymbolsB>} );
ok("\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<[A]+<?InMiscellaneousMathematicalSymbolsB>>$/, q{Match compound <?InMiscellaneousMathematicalSymbolsB>} );
ok(!( "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<-<?InMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match externally inverted <?InMiscellaneousMathematicalSymbolsB>} );
ok(!( "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<[A]-<?InMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match compound inverted <?InMiscellaneousMathematicalSymbolsB>} );
ok(!( "\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/^<+<-InMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match internally inverted <?InMiscellaneousMathematicalSymbolsB>} );
ok(!( "\x[5793]"  ~~ m/^<+<?InMiscellaneousMathematicalSymbolsB>>$/ ), q{Don't match unrelated <?InMiscellaneousMathematicalSymbolsB>} );
ok("\x[5793]"  ~~ m/^<-<?InMiscellaneousMathematicalSymbolsB>>$/, q{Match unrelated externally inverted <?InMiscellaneousMathematicalSymbolsB>} );
ok("\x[5793]"  ~~ m/^<+<-InMiscellaneousMathematicalSymbolsB>>$/, q{Match unrelated internally inverted <?InMiscellaneousMathematicalSymbolsB>} );
ok("\x[5793]\c[TRIPLE VERTICAL BAR DELIMITER]" ~~ m/<+<?InMiscellaneousMathematicalSymbolsB>>/, q{Match unanchored <?InMiscellaneousMathematicalSymbolsB>} );

# InMiscellaneousSymbols


ok("\c[BLACK SUN WITH RAYS]" ~~ m/^<+<?InMiscellaneousSymbols>>$/, q{Match <?InMiscellaneousSymbols>} );
ok("\c[BLACK SUN WITH RAYS]" ~~ m/^<[A]+<?InMiscellaneousSymbols>>$/, q{Match compound <?InMiscellaneousSymbols>} );
ok(!( "\c[BLACK SUN WITH RAYS]" ~~ m/^<-<?InMiscellaneousSymbols>>$/ ), q{Don't match externally inverted <?InMiscellaneousSymbols>} );
ok(!( "\c[BLACK SUN WITH RAYS]" ~~ m/^<[A]-<?InMiscellaneousSymbols>>$/ ), q{Don't match compound inverted <?InMiscellaneousSymbols>} );
ok(!( "\c[BLACK SUN WITH RAYS]" ~~ m/^<+<-InMiscellaneousSymbols>>$/ ), q{Don't match internally inverted <?InMiscellaneousSymbols>} );
ok(!( "\x[39D9]"  ~~ m/^<+<?InMiscellaneousSymbols>>$/ ), q{Don't match unrelated <?InMiscellaneousSymbols>} );
ok("\x[39D9]"  ~~ m/^<-<?InMiscellaneousSymbols>>$/, q{Match unrelated externally inverted <?InMiscellaneousSymbols>} );
ok("\x[39D9]"  ~~ m/^<+<-InMiscellaneousSymbols>>$/, q{Match unrelated internally inverted <?InMiscellaneousSymbols>} );
ok("\x[39D9]\c[BLACK SUN WITH RAYS]" ~~ m/<+<?InMiscellaneousSymbols>>/, q{Match unanchored <?InMiscellaneousSymbols>} );

# InMiscellaneousTechnical


ok("\c[DIAMETER SIGN]" ~~ m/^<+<?InMiscellaneousTechnical>>$/, q{Match <?InMiscellaneousTechnical>} );
ok("\c[DIAMETER SIGN]" ~~ m/^<[A]+<?InMiscellaneousTechnical>>$/, q{Match compound <?InMiscellaneousTechnical>} );
ok(!( "\c[DIAMETER SIGN]" ~~ m/^<-<?InMiscellaneousTechnical>>$/ ), q{Don't match externally inverted <?InMiscellaneousTechnical>} );
ok(!( "\c[DIAMETER SIGN]" ~~ m/^<[A]-<?InMiscellaneousTechnical>>$/ ), q{Don't match compound inverted <?InMiscellaneousTechnical>} );
ok(!( "\c[DIAMETER SIGN]" ~~ m/^<+<-InMiscellaneousTechnical>>$/ ), q{Don't match internally inverted <?InMiscellaneousTechnical>} );
ok(!( "\x[528A]"  ~~ m/^<+<?InMiscellaneousTechnical>>$/ ), q{Don't match unrelated <?InMiscellaneousTechnical>} );
ok("\x[528A]"  ~~ m/^<-<?InMiscellaneousTechnical>>$/, q{Match unrelated externally inverted <?InMiscellaneousTechnical>} );
ok("\x[528A]"  ~~ m/^<+<-InMiscellaneousTechnical>>$/, q{Match unrelated internally inverted <?InMiscellaneousTechnical>} );
ok("\x[528A]\c[DIAMETER SIGN]" ~~ m/<+<?InMiscellaneousTechnical>>/, q{Match unanchored <?InMiscellaneousTechnical>} );

# InMongolian


ok("\c[MONGOLIAN BIRGA]" ~~ m/^<+<?InMongolian>>$/, q{Match <?InMongolian>} );
ok("\c[MONGOLIAN BIRGA]" ~~ m/^<[A]+<?InMongolian>>$/, q{Match compound <?InMongolian>} );
ok(!( "\c[MONGOLIAN BIRGA]" ~~ m/^<-<?InMongolian>>$/ ), q{Don't match externally inverted <?InMongolian>} );
ok(!( "\c[MONGOLIAN BIRGA]" ~~ m/^<[A]-<?InMongolian>>$/ ), q{Don't match compound inverted <?InMongolian>} );
ok(!( "\c[MONGOLIAN BIRGA]" ~~ m/^<+<-InMongolian>>$/ ), q{Don't match internally inverted <?InMongolian>} );
ok(!( "\x[3985]"  ~~ m/^<+<?InMongolian>>$/ ), q{Don't match unrelated <?InMongolian>} );
ok("\x[3985]"  ~~ m/^<-<?InMongolian>>$/, q{Match unrelated externally inverted <?InMongolian>} );
ok("\x[3985]"  ~~ m/^<+<-InMongolian>>$/, q{Match unrelated internally inverted <?InMongolian>} );
ok("\x[3985]\c[MONGOLIAN BIRGA]" ~~ m/<+<?InMongolian>>/, q{Match unanchored <?InMongolian>} );

# InMusicalSymbols


ok(!( "\x[7A59]"  ~~ m/^<+<?InMusicalSymbols>>$/ ), q{Don't match unrelated <?InMusicalSymbols>} );
ok("\x[7A59]"  ~~ m/^<-<?InMusicalSymbols>>$/, q{Match unrelated externally inverted <?InMusicalSymbols>} );
ok("\x[7A59]"  ~~ m/^<+<-InMusicalSymbols>>$/, q{Match unrelated internally inverted <?InMusicalSymbols>} );

# InMyanmar


ok("\c[MYANMAR LETTER KA]" ~~ m/^<+<?InMyanmar>>$/, q{Match <?InMyanmar>} );
ok("\c[MYANMAR LETTER KA]" ~~ m/^<[A]+<?InMyanmar>>$/, q{Match compound <?InMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<-<?InMyanmar>>$/ ), q{Don't match externally inverted <?InMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<[A]-<?InMyanmar>>$/ ), q{Don't match compound inverted <?InMyanmar>} );
ok(!( "\c[MYANMAR LETTER KA]" ~~ m/^<+<-InMyanmar>>$/ ), q{Don't match internally inverted <?InMyanmar>} );
ok(!( "\x[5698]"  ~~ m/^<+<?InMyanmar>>$/ ), q{Don't match unrelated <?InMyanmar>} );
ok("\x[5698]"  ~~ m/^<-<?InMyanmar>>$/, q{Match unrelated externally inverted <?InMyanmar>} );
ok("\x[5698]"  ~~ m/^<+<-InMyanmar>>$/, q{Match unrelated internally inverted <?InMyanmar>} );
ok("\x[5698]\c[MYANMAR LETTER KA]" ~~ m/<+<?InMyanmar>>/, q{Match unanchored <?InMyanmar>} );

# InNumberForms


ok("\x[2150]" ~~ m/^<+<?InNumberForms>>$/, q{Match <?InNumberForms>} );
ok("\x[2150]" ~~ m/^<[A]+<?InNumberForms>>$/, q{Match compound <?InNumberForms>} );
ok(!( "\x[2150]" ~~ m/^<-<?InNumberForms>>$/ ), q{Don't match externally inverted <?InNumberForms>} );
ok(!( "\x[2150]" ~~ m/^<[A]-<?InNumberForms>>$/ ), q{Don't match compound inverted <?InNumberForms>} );
ok(!( "\x[2150]" ~~ m/^<+<-InNumberForms>>$/ ), q{Don't match internally inverted <?InNumberForms>} );
ok(!( "\x[C41B]"  ~~ m/^<+<?InNumberForms>>$/ ), q{Don't match unrelated <?InNumberForms>} );
ok("\x[C41B]"  ~~ m/^<-<?InNumberForms>>$/, q{Match unrelated externally inverted <?InNumberForms>} );
ok("\x[C41B]"  ~~ m/^<+<-InNumberForms>>$/, q{Match unrelated internally inverted <?InNumberForms>} );
ok("\x[C41B]\x[2150]" ~~ m/<+<?InNumberForms>>/, q{Match unanchored <?InNumberForms>} );

# InOgham


ok("\c[OGHAM SPACE MARK]" ~~ m/^<+<?InOgham>>$/, q{Match <?InOgham>} );
ok("\c[OGHAM SPACE MARK]" ~~ m/^<[A]+<?InOgham>>$/, q{Match compound <?InOgham>} );
ok(!( "\c[OGHAM SPACE MARK]" ~~ m/^<-<?InOgham>>$/ ), q{Don't match externally inverted <?InOgham>} );
ok(!( "\c[OGHAM SPACE MARK]" ~~ m/^<[A]-<?InOgham>>$/ ), q{Don't match compound inverted <?InOgham>} );
ok(!( "\c[OGHAM SPACE MARK]" ~~ m/^<+<-InOgham>>$/ ), q{Don't match internally inverted <?InOgham>} );
ok(!( "\x[2C8C]"  ~~ m/^<+<?InOgham>>$/ ), q{Don't match unrelated <?InOgham>} );
ok("\x[2C8C]"  ~~ m/^<-<?InOgham>>$/, q{Match unrelated externally inverted <?InOgham>} );
ok("\x[2C8C]"  ~~ m/^<+<-InOgham>>$/, q{Match unrelated internally inverted <?InOgham>} );
ok("\x[2C8C]\c[OGHAM SPACE MARK]" ~~ m/<+<?InOgham>>/, q{Match unanchored <?InOgham>} );

# InOldItalic


ok(!( "\c[YI SYLLABLE MGAT]"  ~~ m/^<+<?InOldItalic>>$/ ), q{Don't match unrelated <?InOldItalic>} );
ok("\c[YI SYLLABLE MGAT]"  ~~ m/^<-<?InOldItalic>>$/, q{Match unrelated externally inverted <?InOldItalic>} );
ok("\c[YI SYLLABLE MGAT]"  ~~ m/^<+<-InOldItalic>>$/, q{Match unrelated internally inverted <?InOldItalic>} );

# InOpticalCharacterRecognition


ok("\c[OCR HOOK]" ~~ m/^<+<?InOpticalCharacterRecognition>>$/, q{Match <?InOpticalCharacterRecognition>} );
ok("\c[OCR HOOK]" ~~ m/^<[A]+<?InOpticalCharacterRecognition>>$/, q{Match compound <?InOpticalCharacterRecognition>} );
ok(!( "\c[OCR HOOK]" ~~ m/^<-<?InOpticalCharacterRecognition>>$/ ), q{Don't match externally inverted <?InOpticalCharacterRecognition>} );
ok(!( "\c[OCR HOOK]" ~~ m/^<[A]-<?InOpticalCharacterRecognition>>$/ ), q{Don't match compound inverted <?InOpticalCharacterRecognition>} );
ok(!( "\c[OCR HOOK]" ~~ m/^<+<-InOpticalCharacterRecognition>>$/ ), q{Don't match internally inverted <?InOpticalCharacterRecognition>} );
ok(!( "\x[CB83]"  ~~ m/^<+<?InOpticalCharacterRecognition>>$/ ), q{Don't match unrelated <?InOpticalCharacterRecognition>} );
ok("\x[CB83]"  ~~ m/^<-<?InOpticalCharacterRecognition>>$/, q{Match unrelated externally inverted <?InOpticalCharacterRecognition>} );
ok("\x[CB83]"  ~~ m/^<+<-InOpticalCharacterRecognition>>$/, q{Match unrelated internally inverted <?InOpticalCharacterRecognition>} );
ok("\x[CB83]\c[OCR HOOK]" ~~ m/<+<?InOpticalCharacterRecognition>>/, q{Match unanchored <?InOpticalCharacterRecognition>} );

# InOriya


ok("\x[0B00]" ~~ m/^<+<?InOriya>>$/, q{Match <?InOriya>} );
ok("\x[0B00]" ~~ m/^<[A]+<?InOriya>>$/, q{Match compound <?InOriya>} );
ok(!( "\x[0B00]" ~~ m/^<-<?InOriya>>$/ ), q{Don't match externally inverted <?InOriya>} );
ok(!( "\x[0B00]" ~~ m/^<[A]-<?InOriya>>$/ ), q{Don't match compound inverted <?InOriya>} );
ok(!( "\x[0B00]" ~~ m/^<+<-InOriya>>$/ ), q{Don't match internally inverted <?InOriya>} );
ok(!( "\x[6CE7]"  ~~ m/^<+<?InOriya>>$/ ), q{Don't match unrelated <?InOriya>} );
ok("\x[6CE7]"  ~~ m/^<-<?InOriya>>$/, q{Match unrelated externally inverted <?InOriya>} );
ok("\x[6CE7]"  ~~ m/^<+<-InOriya>>$/, q{Match unrelated internally inverted <?InOriya>} );
ok("\x[6CE7]\x[0B00]" ~~ m/<+<?InOriya>>/, q{Match unanchored <?InOriya>} );

# InPrivateUseArea


ok(!( "\x[7662]"  ~~ m/^<+<?InPrivateUseArea>>$/ ), q{Don't match unrelated <?InPrivateUseArea>} );
ok("\x[7662]"  ~~ m/^<-<?InPrivateUseArea>>$/, q{Match unrelated externally inverted <?InPrivateUseArea>} );
ok("\x[7662]"  ~~ m/^<+<-InPrivateUseArea>>$/, q{Match unrelated internally inverted <?InPrivateUseArea>} );

# InRunic


ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<?InRunic>>$/, q{Match <?InRunic>} );
ok("\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]+<?InRunic>>$/, q{Match compound <?InRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<-<?InRunic>>$/ ), q{Don't match externally inverted <?InRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<[A]-<?InRunic>>$/ ), q{Don't match compound inverted <?InRunic>} );
ok(!( "\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/^<+<-InRunic>>$/ ), q{Don't match internally inverted <?InRunic>} );
ok(!( "\x[47A1]"  ~~ m/^<+<?InRunic>>$/ ), q{Don't match unrelated <?InRunic>} );
ok("\x[47A1]"  ~~ m/^<-<?InRunic>>$/, q{Match unrelated externally inverted <?InRunic>} );
ok("\x[47A1]"  ~~ m/^<+<-InRunic>>$/, q{Match unrelated internally inverted <?InRunic>} );
ok("\x[47A1]\c[RUNIC LETTER FEHU FEOH FE F]" ~~ m/<+<?InRunic>>/, q{Match unanchored <?InRunic>} );

# InSinhala


ok("\x[0D80]" ~~ m/^<+<?InSinhala>>$/, q{Match <?InSinhala>} );
ok("\x[0D80]" ~~ m/^<[A]+<?InSinhala>>$/, q{Match compound <?InSinhala>} );
ok(!( "\x[0D80]" ~~ m/^<-<?InSinhala>>$/ ), q{Don't match externally inverted <?InSinhala>} );
ok(!( "\x[0D80]" ~~ m/^<[A]-<?InSinhala>>$/ ), q{Don't match compound inverted <?InSinhala>} );
ok(!( "\x[0D80]" ~~ m/^<+<-InSinhala>>$/ ), q{Don't match internally inverted <?InSinhala>} );
ok(!( "\x[1C39]"  ~~ m/^<+<?InSinhala>>$/ ), q{Don't match unrelated <?InSinhala>} );
ok("\x[1C39]"  ~~ m/^<-<?InSinhala>>$/, q{Match unrelated externally inverted <?InSinhala>} );
ok("\x[1C39]"  ~~ m/^<+<-InSinhala>>$/, q{Match unrelated internally inverted <?InSinhala>} );
ok("\x[1C39]\x[0D80]" ~~ m/<+<?InSinhala>>/, q{Match unanchored <?InSinhala>} );

# InSmallFormVariants


ok(!( "\c[YI SYLLABLE FAP]"  ~~ m/^<+<?InSmallFormVariants>>$/ ), q{Don't match unrelated <?InSmallFormVariants>} );
ok("\c[YI SYLLABLE FAP]"  ~~ m/^<-<?InSmallFormVariants>>$/, q{Match unrelated externally inverted <?InSmallFormVariants>} );
ok("\c[YI SYLLABLE FAP]"  ~~ m/^<+<-InSmallFormVariants>>$/, q{Match unrelated internally inverted <?InSmallFormVariants>} );

# InSpacingModifierLetters


ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<?InSpacingModifierLetters>>$/, q{Match <?InSpacingModifierLetters>} );
ok("\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]+<?InSpacingModifierLetters>>$/, q{Match compound <?InSpacingModifierLetters>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<-<?InSpacingModifierLetters>>$/ ), q{Don't match externally inverted <?InSpacingModifierLetters>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<[A]-<?InSpacingModifierLetters>>$/ ), q{Don't match compound inverted <?InSpacingModifierLetters>} );
ok(!( "\c[MODIFIER LETTER SMALL H]" ~~ m/^<+<-InSpacingModifierLetters>>$/ ), q{Don't match internally inverted <?InSpacingModifierLetters>} );
ok(!( "\x[08E8]"  ~~ m/^<+<?InSpacingModifierLetters>>$/ ), q{Don't match unrelated <?InSpacingModifierLetters>} );
ok("\x[08E8]"  ~~ m/^<-<?InSpacingModifierLetters>>$/, q{Match unrelated externally inverted <?InSpacingModifierLetters>} );
ok("\x[08E8]"  ~~ m/^<+<-InSpacingModifierLetters>>$/, q{Match unrelated internally inverted <?InSpacingModifierLetters>} );
ok("\x[08E8]\c[MODIFIER LETTER SMALL H]" ~~ m/<+<?InSpacingModifierLetters>>/, q{Match unanchored <?InSpacingModifierLetters>} );

# InSpecials


ok(!( "\x[0C7E]"  ~~ m/^<+<?InSpecials>>$/ ), q{Don't match unrelated <?InSpecials>} );
ok("\x[0C7E]"  ~~ m/^<-<?InSpecials>>$/, q{Match unrelated externally inverted <?InSpecials>} );
ok("\x[0C7E]"  ~~ m/^<+<-InSpecials>>$/, q{Match unrelated internally inverted <?InSpecials>} );

# InSuperscriptsAndSubscripts


ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<+<?InSuperscriptsAndSubscripts>>$/, q{Match <?InSuperscriptsAndSubscripts>} );
ok("\c[SUPERSCRIPT ZERO]" ~~ m/^<[A]+<?InSuperscriptsAndSubscripts>>$/, q{Match compound <?InSuperscriptsAndSubscripts>} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<-<?InSuperscriptsAndSubscripts>>$/ ), q{Don't match externally inverted <?InSuperscriptsAndSubscripts>} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<[A]-<?InSuperscriptsAndSubscripts>>$/ ), q{Don't match compound inverted <?InSuperscriptsAndSubscripts>} );
ok(!( "\c[SUPERSCRIPT ZERO]" ~~ m/^<+<-InSuperscriptsAndSubscripts>>$/ ), q{Don't match internally inverted <?InSuperscriptsAndSubscripts>} );
ok(!( "\x[D378]"  ~~ m/^<+<?InSuperscriptsAndSubscripts>>$/ ), q{Don't match unrelated <?InSuperscriptsAndSubscripts>} );
ok("\x[D378]"  ~~ m/^<-<?InSuperscriptsAndSubscripts>>$/, q{Match unrelated externally inverted <?InSuperscriptsAndSubscripts>} );
ok("\x[D378]"  ~~ m/^<+<-InSuperscriptsAndSubscripts>>$/, q{Match unrelated internally inverted <?InSuperscriptsAndSubscripts>} );
ok("\x[D378]\c[SUPERSCRIPT ZERO]" ~~ m/<+<?InSuperscriptsAndSubscripts>>/, q{Match unanchored <?InSuperscriptsAndSubscripts>} );

# InSupplementalArrowsA


ok("\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<+<?InSupplementalArrowsA>>$/, q{Match <?InSupplementalArrowsA>} );
ok("\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<[A]+<?InSupplementalArrowsA>>$/, q{Match compound <?InSupplementalArrowsA>} );
ok(!( "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<-<?InSupplementalArrowsA>>$/ ), q{Don't match externally inverted <?InSupplementalArrowsA>} );
ok(!( "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<[A]-<?InSupplementalArrowsA>>$/ ), q{Don't match compound inverted <?InSupplementalArrowsA>} );
ok(!( "\c[UPWARDS QUADRUPLE ARROW]" ~~ m/^<+<-InSupplementalArrowsA>>$/ ), q{Don't match internally inverted <?InSupplementalArrowsA>} );
ok(!( "\c[LIMBU DIGIT SEVEN]"  ~~ m/^<+<?InSupplementalArrowsA>>$/ ), q{Don't match unrelated <?InSupplementalArrowsA>} );
ok("\c[LIMBU DIGIT SEVEN]"  ~~ m/^<-<?InSupplementalArrowsA>>$/, q{Match unrelated externally inverted <?InSupplementalArrowsA>} );
ok("\c[LIMBU DIGIT SEVEN]"  ~~ m/^<+<-InSupplementalArrowsA>>$/, q{Match unrelated internally inverted <?InSupplementalArrowsA>} );
ok("\c[LIMBU DIGIT SEVEN]\c[UPWARDS QUADRUPLE ARROW]" ~~ m/<+<?InSupplementalArrowsA>>/, q{Match unanchored <?InSupplementalArrowsA>} );

# InSupplementalArrowsB


ok("\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<+<?InSupplementalArrowsB>>$/, q{Match <?InSupplementalArrowsB>} );
ok("\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]+<?InSupplementalArrowsB>>$/, q{Match compound <?InSupplementalArrowsB>} );
ok(!( "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<-<?InSupplementalArrowsB>>$/ ), q{Don't match externally inverted <?InSupplementalArrowsB>} );
ok(!( "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<[A]-<?InSupplementalArrowsB>>$/ ), q{Don't match compound inverted <?InSupplementalArrowsB>} );
ok(!( "\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/^<+<-InSupplementalArrowsB>>$/ ), q{Don't match internally inverted <?InSupplementalArrowsB>} );
ok(!( "\x[1D7D]"  ~~ m/^<+<?InSupplementalArrowsB>>$/ ), q{Don't match unrelated <?InSupplementalArrowsB>} );
ok("\x[1D7D]"  ~~ m/^<-<?InSupplementalArrowsB>>$/, q{Match unrelated externally inverted <?InSupplementalArrowsB>} );
ok("\x[1D7D]"  ~~ m/^<+<-InSupplementalArrowsB>>$/, q{Match unrelated internally inverted <?InSupplementalArrowsB>} );
ok("\x[1D7D]\c[RIGHTWARDS TWO-HEADED ARROW WITH VERTICAL STROKE]" ~~ m/<+<?InSupplementalArrowsB>>/, q{Match unanchored <?InSupplementalArrowsB>} );

# InSupplementalMathematicalOperators


ok("\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<+<?InSupplementalMathematicalOperators>>$/, q{Match <?InSupplementalMathematicalOperators>} );
ok("\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<[A]+<?InSupplementalMathematicalOperators>>$/, q{Match compound <?InSupplementalMathematicalOperators>} );
ok(!( "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<-<?InSupplementalMathematicalOperators>>$/ ), q{Don't match externally inverted <?InSupplementalMathematicalOperators>} );
ok(!( "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<[A]-<?InSupplementalMathematicalOperators>>$/ ), q{Don't match compound inverted <?InSupplementalMathematicalOperators>} );
ok(!( "\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/^<+<-InSupplementalMathematicalOperators>>$/ ), q{Don't match internally inverted <?InSupplementalMathematicalOperators>} );
ok(!( "\c[YI SYLLABLE TAX]"  ~~ m/^<+<?InSupplementalMathematicalOperators>>$/ ), q{Don't match unrelated <?InSupplementalMathematicalOperators>} );
ok("\c[YI SYLLABLE TAX]"  ~~ m/^<-<?InSupplementalMathematicalOperators>>$/, q{Match unrelated externally inverted <?InSupplementalMathematicalOperators>} );
ok("\c[YI SYLLABLE TAX]"  ~~ m/^<+<-InSupplementalMathematicalOperators>>$/, q{Match unrelated internally inverted <?InSupplementalMathematicalOperators>} );
ok("\c[YI SYLLABLE TAX]\c[N-ARY CIRCLED DOT OPERATOR]" ~~ m/<+<?InSupplementalMathematicalOperators>>/, q{Match unanchored <?InSupplementalMathematicalOperators>} );

# InSupplementaryPrivateUseAreaA


ok(!( "\c[TIBETAN LETTER PHA]"  ~~ m/^<+<?InSupplementaryPrivateUseAreaA>>$/ ), q{Don't match unrelated <?InSupplementaryPrivateUseAreaA>} );
ok("\c[TIBETAN LETTER PHA]"  ~~ m/^<-<?InSupplementaryPrivateUseAreaA>>$/, q{Match unrelated externally inverted <?InSupplementaryPrivateUseAreaA>} );
ok("\c[TIBETAN LETTER PHA]"  ~~ m/^<+<-InSupplementaryPrivateUseAreaA>>$/, q{Match unrelated internally inverted <?InSupplementaryPrivateUseAreaA>} );

# InSupplementaryPrivateUseAreaB


ok(!( "\x[7E65]"  ~~ m/^<+<?InSupplementaryPrivateUseAreaB>>$/ ), q{Don't match unrelated <?InSupplementaryPrivateUseAreaB>} );
ok("\x[7E65]"  ~~ m/^<-<?InSupplementaryPrivateUseAreaB>>$/, q{Match unrelated externally inverted <?InSupplementaryPrivateUseAreaB>} );
ok("\x[7E65]"  ~~ m/^<+<-InSupplementaryPrivateUseAreaB>>$/, q{Match unrelated internally inverted <?InSupplementaryPrivateUseAreaB>} );

# InSyriac


ok("\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<+<?InSyriac>>$/, q{Match <?InSyriac>} );
ok("\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<[A]+<?InSyriac>>$/, q{Match compound <?InSyriac>} );
ok(!( "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<-<?InSyriac>>$/ ), q{Don't match externally inverted <?InSyriac>} );
ok(!( "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<[A]-<?InSyriac>>$/ ), q{Don't match compound inverted <?InSyriac>} );
ok(!( "\c[SYRIAC END OF PARAGRAPH]" ~~ m/^<+<-InSyriac>>$/ ), q{Don't match internally inverted <?InSyriac>} );
ok(!( "\x[CA1C]"  ~~ m/^<+<?InSyriac>>$/ ), q{Don't match unrelated <?InSyriac>} );
ok("\x[CA1C]"  ~~ m/^<-<?InSyriac>>$/, q{Match unrelated externally inverted <?InSyriac>} );
ok("\x[CA1C]"  ~~ m/^<+<-InSyriac>>$/, q{Match unrelated internally inverted <?InSyriac>} );
ok("\x[CA1C]\c[SYRIAC END OF PARAGRAPH]" ~~ m/<+<?InSyriac>>/, q{Match unanchored <?InSyriac>} );

# InTagalog


ok("\c[TAGALOG LETTER A]" ~~ m/^<+<?InTagalog>>$/, q{Match <?InTagalog>} );
ok("\c[TAGALOG LETTER A]" ~~ m/^<[A]+<?InTagalog>>$/, q{Match compound <?InTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<-<?InTagalog>>$/ ), q{Don't match externally inverted <?InTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<[A]-<?InTagalog>>$/ ), q{Don't match compound inverted <?InTagalog>} );
ok(!( "\c[TAGALOG LETTER A]" ~~ m/^<+<-InTagalog>>$/ ), q{Don't match internally inverted <?InTagalog>} );
ok(!( "\x[D49E]"  ~~ m/^<+<?InTagalog>>$/ ), q{Don't match unrelated <?InTagalog>} );
ok("\x[D49E]"  ~~ m/^<-<?InTagalog>>$/, q{Match unrelated externally inverted <?InTagalog>} );
ok("\x[D49E]"  ~~ m/^<+<-InTagalog>>$/, q{Match unrelated internally inverted <?InTagalog>} );
ok("\x[D49E]\c[TAGALOG LETTER A]" ~~ m/<+<?InTagalog>>/, q{Match unanchored <?InTagalog>} );

# InTagbanwa


ok("\c[TAGBANWA LETTER A]" ~~ m/^<+<?InTagbanwa>>$/, q{Match <?InTagbanwa>} );
ok("\c[TAGBANWA LETTER A]" ~~ m/^<[A]+<?InTagbanwa>>$/, q{Match compound <?InTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<-<?InTagbanwa>>$/ ), q{Don't match externally inverted <?InTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<[A]-<?InTagbanwa>>$/ ), q{Don't match compound inverted <?InTagbanwa>} );
ok(!( "\c[TAGBANWA LETTER A]" ~~ m/^<+<-InTagbanwa>>$/ ), q{Don't match internally inverted <?InTagbanwa>} );
ok(!( "\x[AFAA]"  ~~ m/^<+<?InTagbanwa>>$/ ), q{Don't match unrelated <?InTagbanwa>} );
ok("\x[AFAA]"  ~~ m/^<-<?InTagbanwa>>$/, q{Match unrelated externally inverted <?InTagbanwa>} );
ok("\x[AFAA]"  ~~ m/^<+<-InTagbanwa>>$/, q{Match unrelated internally inverted <?InTagbanwa>} );
ok("\x[AFAA]\c[TAGBANWA LETTER A]" ~~ m/<+<?InTagbanwa>>/, q{Match unanchored <?InTagbanwa>} );

# InTags


ok(!( "\x[CA38]"  ~~ m/^<+<?InTags>>$/ ), q{Don't match unrelated <?InTags>} );
ok("\x[CA38]"  ~~ m/^<-<?InTags>>$/, q{Match unrelated externally inverted <?InTags>} );
ok("\x[CA38]"  ~~ m/^<+<-InTags>>$/, q{Match unrelated internally inverted <?InTags>} );

# InTamil


ok("\x[0B80]" ~~ m/^<+<?InTamil>>$/, q{Match <?InTamil>} );
ok("\x[0B80]" ~~ m/^<[A]+<?InTamil>>$/, q{Match compound <?InTamil>} );
ok(!( "\x[0B80]" ~~ m/^<-<?InTamil>>$/ ), q{Don't match externally inverted <?InTamil>} );
ok(!( "\x[0B80]" ~~ m/^<[A]-<?InTamil>>$/ ), q{Don't match compound inverted <?InTamil>} );
ok(!( "\x[0B80]" ~~ m/^<+<-InTamil>>$/ ), q{Don't match internally inverted <?InTamil>} );
ok(!( "\x[D44B]"  ~~ m/^<+<?InTamil>>$/ ), q{Don't match unrelated <?InTamil>} );
ok("\x[D44B]"  ~~ m/^<-<?InTamil>>$/, q{Match unrelated externally inverted <?InTamil>} );
ok("\x[D44B]"  ~~ m/^<+<-InTamil>>$/, q{Match unrelated internally inverted <?InTamil>} );
ok("\x[D44B]\x[0B80]" ~~ m/<+<?InTamil>>/, q{Match unanchored <?InTamil>} );

# InTelugu


ok("\x[0C00]" ~~ m/^<+<?InTelugu>>$/, q{Match <?InTelugu>} );
ok("\x[0C00]" ~~ m/^<[A]+<?InTelugu>>$/, q{Match compound <?InTelugu>} );
ok(!( "\x[0C00]" ~~ m/^<-<?InTelugu>>$/ ), q{Don't match externally inverted <?InTelugu>} );
ok(!( "\x[0C00]" ~~ m/^<[A]-<?InTelugu>>$/ ), q{Don't match compound inverted <?InTelugu>} );
ok(!( "\x[0C00]" ~~ m/^<+<-InTelugu>>$/ ), q{Don't match internally inverted <?InTelugu>} );
ok(!( "\x[D3E7]"  ~~ m/^<+<?InTelugu>>$/ ), q{Don't match unrelated <?InTelugu>} );
ok("\x[D3E7]"  ~~ m/^<-<?InTelugu>>$/, q{Match unrelated externally inverted <?InTelugu>} );
ok("\x[D3E7]"  ~~ m/^<+<-InTelugu>>$/, q{Match unrelated internally inverted <?InTelugu>} );
ok("\x[D3E7]\x[0C00]" ~~ m/<+<?InTelugu>>/, q{Match unanchored <?InTelugu>} );

# InThaana


ok("\c[THAANA LETTER HAA]" ~~ m/^<+<?InThaana>>$/, q{Match <?InThaana>} );
ok("\c[THAANA LETTER HAA]" ~~ m/^<[A]+<?InThaana>>$/, q{Match compound <?InThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<-<?InThaana>>$/ ), q{Don't match externally inverted <?InThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<[A]-<?InThaana>>$/ ), q{Don't match compound inverted <?InThaana>} );
ok(!( "\c[THAANA LETTER HAA]" ~~ m/^<+<-InThaana>>$/ ), q{Don't match internally inverted <?InThaana>} );
ok(!( "\c[YI SYLLABLE QIT]"  ~~ m/^<+<?InThaana>>$/ ), q{Don't match unrelated <?InThaana>} );
ok("\c[YI SYLLABLE QIT]"  ~~ m/^<-<?InThaana>>$/, q{Match unrelated externally inverted <?InThaana>} );
ok("\c[YI SYLLABLE QIT]"  ~~ m/^<+<-InThaana>>$/, q{Match unrelated internally inverted <?InThaana>} );
ok("\c[YI SYLLABLE QIT]\c[THAANA LETTER HAA]" ~~ m/<+<?InThaana>>/, q{Match unanchored <?InThaana>} );

# InThai


ok("\x[0E00]" ~~ m/^<+<?InThai>>$/, q{Match <?InThai>} );
ok("\x[0E00]" ~~ m/^<[A]+<?InThai>>$/, q{Match compound <?InThai>} );
ok(!( "\x[0E00]" ~~ m/^<-<?InThai>>$/ ), q{Don't match externally inverted <?InThai>} );
ok(!( "\x[0E00]" ~~ m/^<[A]-<?InThai>>$/ ), q{Don't match compound inverted <?InThai>} );
ok(!( "\x[0E00]" ~~ m/^<+<-InThai>>$/ ), q{Don't match internally inverted <?InThai>} );
ok(!( "\x[BCED]"  ~~ m/^<+<?InThai>>$/ ), q{Don't match unrelated <?InThai>} );
ok("\x[BCED]"  ~~ m/^<-<?InThai>>$/, q{Match unrelated externally inverted <?InThai>} );
ok("\x[BCED]"  ~~ m/^<+<-InThai>>$/, q{Match unrelated internally inverted <?InThai>} );
ok("\x[BCED]\x[0E00]" ~~ m/<+<?InThai>>/, q{Match unanchored <?InThai>} );

# InTibetan


ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<?InTibetan>>$/, q{Match <?InTibetan>} );
ok("\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]+<?InTibetan>>$/, q{Match compound <?InTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<-<?InTibetan>>$/ ), q{Don't match externally inverted <?InTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<[A]-<?InTibetan>>$/ ), q{Don't match compound inverted <?InTibetan>} );
ok(!( "\c[TIBETAN SYLLABLE OM]" ~~ m/^<+<-InTibetan>>$/ ), q{Don't match internally inverted <?InTibetan>} );
ok(!( "\c[ARABIC SIGN SINDHI AMPERSAND]"  ~~ m/^<+<?InTibetan>>$/ ), q{Don't match unrelated <?InTibetan>} );
ok("\c[ARABIC SIGN SINDHI AMPERSAND]"  ~~ m/^<-<?InTibetan>>$/, q{Match unrelated externally inverted <?InTibetan>} );
ok("\c[ARABIC SIGN SINDHI AMPERSAND]"  ~~ m/^<+<-InTibetan>>$/, q{Match unrelated internally inverted <?InTibetan>} );
ok("\c[ARABIC SIGN SINDHI AMPERSAND]\c[TIBETAN SYLLABLE OM]" ~~ m/<+<?InTibetan>>/, q{Match unanchored <?InTibetan>} );

# InUnifiedCanadianAboriginalSyllabics


ok("\x[1400]" ~~ m/^<+<?InUnifiedCanadianAboriginalSyllabics>>$/, q{Match <?InUnifiedCanadianAboriginalSyllabics>} );
ok("\x[1400]" ~~ m/^<[A]+<?InUnifiedCanadianAboriginalSyllabics>>$/, q{Match compound <?InUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[1400]" ~~ m/^<-<?InUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match externally inverted <?InUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[1400]" ~~ m/^<[A]-<?InUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match compound inverted <?InUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[1400]" ~~ m/^<+<-InUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match internally inverted <?InUnifiedCanadianAboriginalSyllabics>} );
ok(!( "\x[49D8]"  ~~ m/^<+<?InUnifiedCanadianAboriginalSyllabics>>$/ ), q{Don't match unrelated <?InUnifiedCanadianAboriginalSyllabics>} );
ok("\x[49D8]"  ~~ m/^<-<?InUnifiedCanadianAboriginalSyllabics>>$/, q{Match unrelated externally inverted <?InUnifiedCanadianAboriginalSyllabics>} );
ok("\x[49D8]"  ~~ m/^<+<-InUnifiedCanadianAboriginalSyllabics>>$/, q{Match unrelated internally inverted <?InUnifiedCanadianAboriginalSyllabics>} );
ok("\x[49D8]\x[1400]" ~~ m/<+<?InUnifiedCanadianAboriginalSyllabics>>/, q{Match unanchored <?InUnifiedCanadianAboriginalSyllabics>} );

# InVariationSelectors


ok(!( "\x[5307]"  ~~ m/^<+<?InVariationSelectors>>$/ ), q{Don't match unrelated <?InVariationSelectors>} );
ok("\x[5307]"  ~~ m/^<-<?InVariationSelectors>>$/, q{Match unrelated externally inverted <?InVariationSelectors>} );
ok("\x[5307]"  ~~ m/^<+<-InVariationSelectors>>$/, q{Match unrelated internally inverted <?InVariationSelectors>} );

# InYiRadicals


ok("\c[YI RADICAL QOT]" ~~ m/^<+<?InYiRadicals>>$/, q{Match <?InYiRadicals>} );
ok("\c[YI RADICAL QOT]" ~~ m/^<[A]+<?InYiRadicals>>$/, q{Match compound <?InYiRadicals>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<-<?InYiRadicals>>$/ ), q{Don't match externally inverted <?InYiRadicals>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<[A]-<?InYiRadicals>>$/ ), q{Don't match compound inverted <?InYiRadicals>} );
ok(!( "\c[YI RADICAL QOT]" ~~ m/^<+<-InYiRadicals>>$/ ), q{Don't match internally inverted <?InYiRadicals>} );
ok(!( "\x[7CAD]"  ~~ m/^<+<?InYiRadicals>>$/ ), q{Don't match unrelated <?InYiRadicals>} );
ok("\x[7CAD]"  ~~ m/^<-<?InYiRadicals>>$/, q{Match unrelated externally inverted <?InYiRadicals>} );
ok("\x[7CAD]"  ~~ m/^<+<-InYiRadicals>>$/, q{Match unrelated internally inverted <?InYiRadicals>} );
ok("\x[7CAD]\c[YI RADICAL QOT]" ~~ m/<+<?InYiRadicals>>/, q{Match unanchored <?InYiRadicals>} );

# InYiSyllables


ok("\c[YI SYLLABLE IT]" ~~ m/^<+<?InYiSyllables>>$/, q{Match <?InYiSyllables>} );
ok("\c[YI SYLLABLE IT]" ~~ m/^<[A]+<?InYiSyllables>>$/, q{Match compound <?InYiSyllables>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<-<?InYiSyllables>>$/ ), q{Don't match externally inverted <?InYiSyllables>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<[A]-<?InYiSyllables>>$/ ), q{Don't match compound inverted <?InYiSyllables>} );
ok(!( "\c[YI SYLLABLE IT]" ~~ m/^<+<-InYiSyllables>>$/ ), q{Don't match internally inverted <?InYiSyllables>} );
ok(!( "\c[BRAILLE PATTERN DOTS-1578]"  ~~ m/^<+<?InYiSyllables>>$/ ), q{Don't match unrelated <?InYiSyllables>} );
ok("\c[BRAILLE PATTERN DOTS-1578]"  ~~ m/^<-<?InYiSyllables>>$/, q{Match unrelated externally inverted <?InYiSyllables>} );
ok("\c[BRAILLE PATTERN DOTS-1578]"  ~~ m/^<+<-InYiSyllables>>$/, q{Match unrelated internally inverted <?InYiSyllables>} );
ok("\c[BRAILLE PATTERN DOTS-1578]\c[YI SYLLABLE IT]" ~~ m/<+<?InYiSyllables>>/, q{Match unanchored <?InYiSyllables>} );
