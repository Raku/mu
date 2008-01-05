module Unicode;

use Utable;

constant Int $unicode_max = 0x10ffff;

# UnicodeData.txt
# 0 codepoint
# 1 name
#     it would be a shame if all p6 programs had to carry around a huge name->codepoint table
#     I figure \c doesn't need to be efficient, so we can skip it and just grep UnicodeData.txt
# 2 General_Category
my Utable %is;
for <Lu Ll Lt Lm Lo LC L>, <Mn Mc Me M>, <Nd Nl No N>,
    <Pc Pd Ps Pe Pi Pf Po P>, <Sm Sc Sk So S>,
    <Zs Zl Zp Z>, <Cc Cf Cs Co Cn C>,
    # Proplist.txt
    <ASCII_Hex_Digit Bidi_Control Dash Deprecated Diacritic Extender Grapheme_Link>,
    <Hex_Digit Hyphen Ideographic IDS_Binary_Operator IDS_Trinary_Operator Join_Control>,
    <Logical_Order_Exception Noncharacter_Code_Point Other_Alphabetic>,
    <Other_Default_Ignorable_Code_Point Other_Grapheme_Extend Other_ID_Continue>,
    <Other_ID_Start Other_Lowercase Other_Math Other_Uppercase Pattern_Syntax>,
    <Pattern_White_Space Quotation_Mark Radical Soft_Dotted STerm Terminal_Punctuation>,
    <Unified_Ideograph Variation_Selector White_Space>,
    # Perlish stuff
    <alnum alpha ascii blank cntrl digit graph lower print>,
    <punct space title upper xdigit word vspace hspace>
    -> my Str $cat { %is{$cat} = Utable.new; }
# STD needs this
my Str %open2close;
# 3 Canonical_Combining_Class
my Int %ccc;
# 4 Bidi_Class
my Utable $bidi_class;
# 5 Decomposition_Mapping
my Str %compat_decomp;
my Str %compat_decomp_type;
my Str %canon_decomp;
# 6 decimal digit
my Int %dec_digit;
# 7 digit
my Int %digit;
# 8 numeric
my Num %numeric;
# 9 Bidi_Mirrored
my Utable $bidi_mirrored;
# 10 Unicode_1_Name
#     is this needed for anything other than name lookups?
# 11 ISO_Comment
#     needed for anything?
# 12 Simple_Uppercase_Mapping
my Str %upper;
# 13 Simple_Lowercase_Mapping
my Str %lower;
# 14 Simple_Titlecase_Mapping
my Str %title;

# ArabicShaping.txt
#     post-6.0?

# BidiMirroring.txt
# 0 code
# 1 mirrored code
my Str %bidi_mirror;

# Blocks.txt
# 0 code range
# 1 block name
my Utable $blockname;

# CompositionExclusions.txt
# 0 code
my Bool %compex;

# CaseFolding.txt
# 0 code
# 1 status
# 2 mapping
my Str %casefold_stat;
my Str %casefold_map;

# DerivedAge.txt
#     post-6.0?

# EastAsianWidth.txt
#     post-6.0?

# HangulSyllableType.txt
#     post-6.0?

# Jamo.txt
#     post-6.0?

# LineBreak.txt
#     post-6.0?

# NameAliases.txt
# 0 code
# 1 name
#     see note about names in UnicodeData.txt

# NormalizationCorrections.txt
# 0 code
# 1 Original (erroneous) decomposition
# 2 Corrected decomposition
# 3 version corrected

# PropertyAliases.txt
# 0 abbrev
# 1 full name
# 2... more aliases
my Str %propalias;

# PropertyValueAliases.txt
# 0 prop
# 1 abbrev
# 2 full name
# -- for ccc ---
# 0 ccc
# 1 ccc num
# 2 abbrev
# 3 full name
my Hash of Str %pva;
...;

# Scripts.txt
# 0 code range
# 1 script name
my Utable $script;

# SpecialCasing.txt
# 0 code
# 1 lower
# 2 title
# 3 upper
# 4 conditionals
my Str %upper_cond;
my Str %lower_cond;
my Str %title_cond;
my Str %case_cond;

# Unihan.txt
#     post-6.0?

# DerivedCoreProperties.txt
# DerivedNormalizationProps.txt

# Proplist.txt
# 0 code range
# 1 prop name

# GraphemeBreakProperty.txt
#     post-6.0?

# SentenceBreakProperty.txt
#     post-6.0?

# WordBreakProperty.txt
#     post-6.0?

# From S29

class Str is also {
    our Str multi method lc ( Str $string: ) is export { ... }
    our Str multi method lcfirst ( Str $string: ) is export { ... }
    our Str multi method uc ( Str $string: ) is export { ... }
    our Str multi method ucfirst ( Str $string: ) is export { ... }
    our Str multi method capitalize ( Str $string: ) is export { ... }
    our Str multi method normalize ( Str $string: Bool :$canonical = Bool::True, Bool :$recompose = Bool::False ) is export { ... }
    our Str multi method nfd ( Str $string: ) is export { $string.normalize(:cononical, :!recompose); }
    our Str multi method nfc ( Str $string: ) is export { $string.normalize(:canonical, :recompose); }
    our Str multi method nfkd ( Str $string: ) is export { $string.normalize(:!canonical, :!recompose); }
    our Str multi method nfkc ( Str $string: ) is export { $string.normalize(:!canonical, :recompose); }
    our multi method ord( Str $string: ) is export { ... }
}
our class AnyChar is Str { ... }
our class Uni is AnyChar is Int { ... }
our class Codepoint is Uni { }
our class Grapheme is AnyChar { ... }
our class Byte is AnyChar is Int { ... }
our class CharLingua is AnyChar { ... }

our Uni multi chr( Uni $codepoint ) { $codepoint }
our Uni multi ord( Uni $character ) { $character }
