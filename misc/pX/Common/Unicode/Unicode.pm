module Unicode;

constant Int $unicode_max = 0x10ffff;

# UnicodeData.txt
# 0 codepoint
# 1 name
#     it would be a shame if all p6 programs had to carry around a huge name->codepoint table
#     I figure \c doesn't need to be efficient, so we can skip it and just grep UnicodeData.txt
# 2 General_Category
our Utable %is;
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
our Str %open2close;
# 3 Canonical_Combining_Class
our Int %ccc;
# 4 Bidi_Class
our Utable $bidi_class;
# 5 Decomposition_Mapping
our Str %compat_decomp;
our Str %compat_decomp_type;
our Str %canon_decomp;
# 6 decimal digit
our Int %dec_digit;
# 7 digit
our Int %digit;
# 8 numeric
our Num %numeric;
# 9 Bidi_Mirrored
our Utable $bidi_mirrored;
# 10 Unicode_1_Name
#     is this needed for anything other than name lookups?
# 11 ISO_Comment
#     needed for anything?
# 12 Simple_Uppercase_Mapping
our Str %upper;
# 13 Simple_Lowercase_Mapping
our Str %lower;
# 14 Simple_Titlecase_Mapping
our Str %title;

# ArabicShaping.txt
#     post-6.0?

# BidiMirroring.txt
# 0 code
# 1 mirrored code
our Str %bidi_mirror;

# Blocks.txt
# 0 code range
# 1 block name
our Utable $blockname;

# CompositionExclusions.txt
# 0 code
our Bool %compex;

# CaseFolding.txt
# 0 code
# 1 status
# 2 mapping
our Str %casefold_stat;
our Str %casefold_map;

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
our Str %propalias;

# PropertyValueAliases.txt
# 0 prop
# 1 abbrev
# 2 full name
# -- for ccc ---
# 0 ccc
# 1 ccc num
# 2 abbrev
# 3 full name
our Hash of Str %pva;
...;

# Scripts.txt
# 0 code range
# 1 script name
our Utable $script;

# SpecialCasing.txt
# 0 code
# 1 lower
# 2 title
# 3 upper
# 4 conditionals
our Str %upper_cond;
our Str %lower_cond;
our Str %title_cond;
our Str %case_cond;

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


class Utable {
    # An efficient data structure for unicode property data
    # The basic data structure is an array of Range objects
    # e.g. <alpha> would use ( 0x41..0x5a ; 0x61..0x7a ; ... )
    # lookup is O(log n), where n is the number of ranges, not total codepoints
    # insertion is O(n) in general, but appending a non-contuguous range is O(1)
    has Range @@.table;
    # Values to attach to ranges
    has Any @.val;

    multi submethod BUILD(Range @@r, Any :@val) {
        @@.table = @@r;
        @.val = @val;
        $.preen;
    }
    multi submethod BUILD(Str $str) {
        # parse the output of $.print
        for $str.split(';') {
            if mm/^ ( <xdigit>+ ) [ ':' ( \S+ ) ]? $/ {
                $.add(hex $0, :val($1), :!preen);
            } elsif mm/^ ( <xdigit>+ ) '..' ( <xdigit>+ ) [ ':' ( \S+ ) ]? $/ {
                $.add(hex($0) .. hex($1), :val($2), :!preen);
            } else {
                die "Utable::BUILD: can't parse '$_'";
            }
        }
        $.preen;
    }

    method print(-->) {
        # not needed if @@.table.perl DTRT...
        loop my Int $i = 0; $i < @@.table.elems; $i++ {
            my Range $r := @@.table[$i];
            my Any $v := @.val[$i];
            print ';' if my Int $n++;
            if $r.min == $r.max {
                printf '%x', $r.min;
            } else {
                printf '%x..%x', $r.min, $r.max;
            }
            print ":$v" if $v.chars;
        }
    }

    method say(-->) { $.print; say; }

    method contains(Int $x --> Bool) {
        return False if !+@@.table;
        return False if $x < @@.table[0].min;
        return False if $x > @@.table[*-1].max;
        my Int $min = 0;
        my Int $max = @@.table.elems-1;
        while $min <= $max {
            my Int $mid = ($max + $min) / 2;
            return True if $x ~~ @@.table[$mid];
            if $x < @@.table[$mid].min  {
                $max = $mid - 1;
            } else {
                $min = $mid + 1;
            }
        }
        return False;
    }

    method get(Int $x --> Any) {
        return undef if !+@@.table;
        return undef if $x < @@.table[0].min;
        return undef if $x > @@.table[*-1].max;
        my Int $min = 0;
        my Int $max = @@.table.elems-1;
        while $min <= $max {
            my Int $mid = ($max + $min) / 2;
            return @.val[$mid] if $x ~~ @@.table[$mid];
            if $x < @@.table[$mid].min  {
                $max = $mid - 1;
            } else {
                $min = $mid + 1;
            }
        }
        return undef;
    }

    method inverse(--> Utable) {
        my Utable $u.=new;
        if !+@@.table {
            $u.add(0 .. $unicode_max);
            return $u;
        }
        $u.add(0 ..^ @@.table[0].min);
        # $i < @@.table.elems-1 is intended
        loop my Int $i = 0; $i < @@.table.elems-1; $i++ {
            $u.add(@@.table[$i].max ^..^ @@.table[$i+1].min, :!preen);
        }
        $u.add(@@.table[*-1].max ^.. $unicode_max);
        return $u;
    }

    multi method add(Int $x, Any :$val, Bool :$preen = True -->) {
        return if $.contains($x);
        my Range $r = $x .. $x;
        if !+@@.table {
            @@.table[0] = $r;
            @.val[0] = $val if defined $val;
            return;
        }
        my Int $min = 0;
        my Int $max = @@.table.elems-1;
        while $min <= $max {
            if $x < @@.table[$min].min {
                @@.table.=splice: $min, 0, $r;
                @.val.=splice: $min, 0, $val if defined $val;
                $.preen if $preen;
                return;
            }
            if $x > @@.table[$max].max {
                @@.table.=splice: $max+1, 0, $r;
                @.val.=splice: $max+1, 0, $val if defined $val;
                $.preen if $preen;
                return;
            }
            my Int $mid = ($max + $min) / 2;
            if $x < @@.table[$mid].min {
                $max = $mid - 1;
            } else {
                $min = $mid + 1;
            }
        }
        die "Utable::add got lost somehow";
    }

    multi method add(Range $r, Any :$val, Bool :$preen = True -->) {
        if !+@@.table {
            @@.table[0] = $r;
            @.val[0] = $val if defined $val;
            $.preen if $preen;
            return;
        }
        my Int $min = 0;
        my Int $max = @@.table.elems-1;
        while $min <= $max {
            if $r.max < @@.table[$min].min {
                @@.table.=splice: $min, 0, $r;
                @.val.=splice: $min, 0, $val if defined $val;
                $.preen if $preen;
                return;
            }
            if $r.min > @@.table[$max].max {
                @@.table.=splice: $max+1, 0, $r;
                @.val.=splice: $max+1, 0, $val if defined $val;
                $.preen if $preen;
                return;
            }
            my Int $mid = ($max + $min) / 2;
            my Range $m := @@.table[$mid];
            if ( $r.max >= $m.min and $r.min <= $m.min ) or ( $r.max >= $m.max and $r.min <= $m.max ) {
                # $r and $m overlap
                die "Utable::add: can't add overlapping ranges with different values"
                    if $val !eqv @.val[$mid];
                $m = min($r.min, $m.min) .. max($r.max, $m.max);
                $.preen if $preen;
                return;
            }
            if $r.max < @@.table[$mid].min {
                $max = $mid - 1;
            } else {
                $min = $mid + 1;
            }
        }
        die "Utable::add got lost somehow";
    }

    method preen(-->) {
        # delete null ranges, fix up range overlaps and contiguities
        loop my Int $i = 0; $i < @@.table.elems; $i++ {
            if @@.table[$i].max < @@.table[$i].min {
                @@.table[$i].delete;
                @.val[$i].delete;
            }
            last if $i == @@.table.elems-1;
            if @@.table[$i].max >= @@.table[$i+1].min and @.val[$i] eqv @.val[$i+1]  {
                @@.table.=splice: $i, 2, @@.table[$i].min .. @@.table[$i+1].max;
            }
        }
    }
}
