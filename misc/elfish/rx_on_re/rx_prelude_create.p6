# This is a p6 file, suitable for any elf.

sub create_rx_prelude {
  my $basics = '
  regex alnum {:P5 [[:alnum:]]}
  regex alpha {:P5 [[:alpha:]]}
  regex ascii {:P5 [[:ascii:]]}
  regex blank {:P5 [[:blank:]]}
  regex cntrl {:P5 [[:cntrl:]]}
  regex digit {:P5 [[:digit:]]}
  regex graph {:P5 [[:graph:]]}
  regex lower {:P5 [[:lower:]]}
#  regex print {:P5 [[:print:]]} #X Until rx dont stomp on subs...
  regex punct {:P5 [[:punct:]]}
  regex space {:P5 [[:space:]]}
  regex upper {:P5 [[:upper:]]}
  regex word  {:P5 [[:word:]]}
  regex xdigit {:P5 [[:xdigit:]]}

  regex null {:P5}
  regex sp {:P5 [ ]}
  regex lt {:P5 [<]}
  regex gt {:P5 [>]}
  regex dot {:P5 \\.}
  regex wb {:P5 \\b}
#  regex fail {:P5 (?!)} #X STD.pm/gimme5 is not parsing.
  regex fail {112c19a111d323ab3091f62bfc7fb0de}

  # before - implemented as a primitive.
  # after - implemented as a primitive.

  regex commit {:::}

  #regex ident {:P5 (?:_|[[:alpha:]])\w*} #X STD.pm/gimme5 is not parsing.
  regex ident {<fail>} 
  regex name {<fail>}

  '~"\n";
  my @unicode_classes = (
  #perl-5.9.4/pod/perlunicode.pod
  #=item General Category
  'L Letter LC CasedLetter Lu UppercaseLetter Ll LowercaseLetter Lt TitlecaseLetter Lm ModifierLetter Lo OtherLetter M Mark Mn NonspacingMark Mc SpacingMark Me EnclosingMark N Number Nd DecimalNumber Nl LetterNumber No OtherNumber P Punctuation Pc ConnectorPunctuation Pd DashPunctuation Ps OpenPunctuation Pe ClosePunctuation Pi InitialPunctuation Pf FinalPunctuation Po OtherPunctuation S Symbol Sm MathSymbol Sc CurrencySymbol Sk ModifierSymbol So OtherSymbol Z Separator Zs SpaceSeparator Zl LineSeparator Zp ParagraphSeparator C Other Cc Control Cf Format Cs Surrogate Co PrivateUse Cn' ~' '~
  #=item Bidirectional Character Types
  # separate
  #=item Scripts
  'Arabic Armenian Bengali Bopomofo Buhid CanadianAboriginal Cherokee Cyrillic Deseret Devanagari Ethiopic Georgian Gothic Greek Gujarati Gurmukhi Han Hangul Hanunoo Hebrew Hiragana Inherited Kannada Katakana Khmer Lao Latin Malayalam Mongolian Myanmar Ogham OldItalic Oriya Runic Sinhala Syriac Tagalog Tagbanwa Tamil Telugu Thaana Thai Tibetan Yi' ~' '~
  #=item Extended property classes
  'ASCIIHexDigit BidiControl Dash Deprecated Diacritic Extender GraphemeLink HexDigit Hyphen Ideographic IDSBinaryOperator IDSTrinaryOperator JoinControl LogicalOrderException NoncharacterCodePoint OtherAlphabetic OtherDefaultIgnorableCodePoint OtherGraphemeExtend OtherLowercase OtherMath OtherUppercase QuotationMark Radical SoftDotted TerminalPunctuation UnifiedIdeograph WhiteSpace' ~' '~
  # and there are further derived properties:
  'Alphabetic Lowercase Uppercase Math ID_Start ID_Continue Any Assigned Common' ~' '~
  #=item Blocks
  'InAlphabeticPresentationForms InArabic InArabicPresentationFormsA InArabicPresentationFormsB InArmenian InArrows InBasicLatin InBengali InBlockElements InBopomofo InBopomofoExtended InBoxDrawing InBraillePatterns InBuhid InByzantineMusicalSymbols InCJKCompatibility InCJKCompatibilityForms InCJKCompatibilityIdeographs InCJKCompatibilityIdeographsSupplement InCJKRadicalsSupplement InCJKSymbolsAndPunctuation InCJKUnifiedIdeographs InCJKUnifiedIdeographsExtensionA InCJKUnifiedIdeographsExtensionB InCherokee InCombiningDiacriticalMarks InCombiningDiacriticalMarksforSymbols InCombiningHalfMarks InControlPictures InCurrencySymbols InCyrillic InCyrillicSupplementary InDeseret InDevanagari InDingbats InEnclosedAlphanumerics InEnclosedCJKLettersAndMonths InEthiopic InGeneralPunctuation InGeometricShapes InGeorgian InGothic InGreekExtended InGreekAndCoptic InGujarati InGurmukhi InHalfwidthAndFullwidthForms InHangulCompatibilityJamo InHangulJamo InHangulSyllables InHanunoo InHebrew InHighPrivateUseSurrogates InHighSurrogates InHiragana InIPAExtensions InIdeographicDescriptionCharacters InKanbun InKangxiRadicals InKannada InKatakana InKatakanaPhoneticExtensions InKhmer InLao InLatin1Supplement InLatinExtendedA InLatinExtendedAdditional InLatinExtendedB InLetterlikeSymbols InLowSurrogates InMalayalam InMathematicalAlphanumericSymbols InMathematicalOperators InMiscellaneousMathematicalSymbolsA InMiscellaneousMathematicalSymbolsB InMiscellaneousSymbols InMiscellaneousTechnical InMongolian InMusicalSymbols InMyanmar InNumberForms InOgham InOldItalic InOpticalCharacterRecognition InOriya InPrivateUseArea InRunic InSinhala InSmallFormVariants InSpacingModifierLetters InSpecials InSuperscriptsAndSubscripts InSupplementalArrowsA InSupplementalArrowsB InSupplementalMathematicalOperators InSupplementaryPrivateUseAreaA InSupplementaryPrivateUseAreaB InSyriac InTagalog InTagbanwa InTags InTamil InTelugu InThaana InThai InTibetan InUnifiedCanadianAboriginalSyllabics InVariationSelectors InYiRadicals InYiSyllables').split('\s+');
  my @unicode_bidi_classes = (
    'L LRE LRO R AL RLE RLO PDF EN ES ET AN CS NSM BN B S WS ON').split('\s+');

  my $code0 = $basics;
  my $code1 = @unicode_classes.map(sub ($class){
    my $name = "is"~$class;
    "  regex "~$name~" {:P5 \\p\{"~$class~"}}\n";
  }).join("");
  my $code2 = @unicode_bidi_classes.map(sub ($class){
    my $name = "isBidi"~$class;
    "  regex "~$name~" {:P5 \\p\{BidiClass:"~$class~"}}\n";
  }).join("");
  #X Lr - it's defined in propcharset.t, but its not in perlunicode.
  $code2 = $code2 ~  #XXX AST->IR for | .
      "  regex isLr {:P5 \\p\{Ll}|\\p\{Lu}|\\p\{Lt}}\n";

  my $code = "module Any {\n"~$code0~$code1~$code2~"\n}\n";
  say "# Generated by "~$?FILE~"\n";
  say($code);
}
create_rx_prelude;
