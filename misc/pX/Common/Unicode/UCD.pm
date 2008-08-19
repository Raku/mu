module UCD;

use Utable;

grammar ucd {
    token xdigit { <[a..zA..Z0..9]> };

    my Regex &strip_comments := &::Str.subst.assuming( rx{ '#' .* }, '');

    token not_empty { ';' | <xdigit> }

    token code {
        (<xdigit>+)
        { $<ord> = $0.hex;
          $<str> = $<ord>.chr;
        }
    }

    token code_or_range {
        |   (<xdigit>+)
            { $<ord> = $0.hex; }
        |   (<xdigit>+) '..' (<xdigit>+)
            { $<ord> = $0.hex .. $1.hex; }
    }

    token code_or_seq {
        @<seq>=(<xdigit>+ \s*)+
        { $<str> = [~] @<seq>».hex.chr; }
    }
}

# autogenerate some error messages
BEGIN {
    &open.wrap(   -> *$file { callsame orelse die "Couldn't open $file: $!";       } );
    &close.wrap(  -> *$io   { callsame orelse die "Couldn't close $io.name(): $!"; } );
    &system.wrap( -> *@cmd  { callsame orelse die "Couldn't @cmd[]: $!";           } );
    &chdir.wrap(  -> *$dir  { callsame orelse die "Couldn't chdir to $dir: $!";    } );
    &mkdir.wrap(  -> *$dir  { callsame orelse die "Couldn't mkdir $dir: $!";       } );
}

# generic processing for UCD .txt files
multi sub process_file(Str $file, Code &each_line:(Str) --> Void) {
    my $fd = open $file, :r;
    for =$fd -> Str $line {
        $line.=ucd::strip_comments;
        next if $line !~~ &ucd::not_empty;
        each_line($line);
    }
    $fd.close;
}
multi sub process_file(Str $file, Regex &line_rx, Code &each_line --> Void) {
    my $fd = open $file, :r;
    for =$fd -> Str $line {
        $line.=ucd::strip_comments;
        next if $line !~~ &ucd::not_empty;
        $line ~~ rule { <line_rx> { $/ := $<line_rx>; each_line(); make $/; } };
            orelse die "Couldn't parse $file line '$line'";
    }
    $fd.close;
}

# call like dumphash(:%hash);
sub dumphash(Pair $thing --> Void) {
    my Str $name := $thing.k;
    my $hash := $thing.v;
    $+dumpfile.say: "\%$name := " ~ $hash.perl ~ ";\n";
}
sub dumputable(Pair $thing --> Void) {
    my Str $name := $thing.k;
    my $utable := $thing.v;
    $+dumpfile.say: "\$$name := " ~ $utable.perl ~ ";\n";
}

sub install_token(Str $name, Code &check(Str --> Bool) --> Void) {
    eval "our token $name is export " ~ '{ (.) <?{ &check($0) }> }';
}

# we have perl6 -MUCD -e mktables in lieu of a script
# the two init paths (mktables/ucd_basic_dump) are interleaved
# in the source to keep them organized - subs are gathered up
# and executed at the right time
my Code @dump_init_subs;
my Code @mktab_subs;
our sub mktables(--> Void) {
    # requires .txt files from e.g. http://www.unicode.org/Public/zipped/5.0.0/UCD.zip
    if 'ucd/Proplist.txt' !~~ :e {
        if 'ucd/UCD.zip' !~~ :e {
            mkdir 'ucd' unless 'ucd' ~~ :d;
            chdir 'ucd';
            system 'wget', 'http://www.unicode.org/Public/zipped/5.0.0/UCD.zip';
            chdir '..';
        }
        chdir 'ucd';
        system 'unzip', 'UCD.zip';
        chdir '..';
    }
    # make this a context var to avoid cluttering up lots of arglists
    my $dumpfile is context = open 'ucd_basic_dump.pm', :w;
    # run all the @mktab_subs here
    $_.() for @mktab_subs;
    $+dumpfile.close;
    exit;
}

# UnicodeData.txt
my Code @mktab_ud_subs;
# 0 codepoint
# 1 name
#     it would be a shame if all p6 programs had to carry around a huge name->codepoint table
#     I figure \c doesn't need to be efficient, so we can skip it and just grep UnicodeData.txt
our sub char_named(Str $name --> Codepoint) { ... }
# 2 General_Category
my Utable %category;
my Str @gen_cats = <Lu Ll Lt Lm Lo LC L>, <Mn Mc Me M>, <Nd Nl No N>,
    <Pc Pd Ps Pe Pi Pf Po P>, <Sm Sc Sk So S>,
    <Zs Zl Zp Z>, <Cc Cf Cs Co Cn C>;
BEGIN {
    @mktab_ud_subs.push: my sub mktab_ud_isgc(*$code, Str *$name, Str *$gc, Str *@f --> Void) {
        $code.=hex;
        %category{$gc}.add($code);
        %category<LC>.add($code) if $gc eq 'Lu'|'Ll'|'Lt';
        my Str $maj = $gc.substr(0, 1); # first letter
        %category{$maj}.add($code);
    }
}
    # a quick digression for...
    # Proplist.txt
    # 0 code range
    # 1 prop name
my Str @proplist_cats = <ASCII_Hex_Digit Bidi_Control Dash Deprecated Diacritic Extender Grapheme_Link>,
    <Hex_Digit Hyphen Ideographic IDS_Binary_Operator IDS_Trinary_Operator Join_Control>,
    <Logical_Order_Exception Noncharacter_Code_Point Other_Alphabetic>,
    <Other_Default_Ignorable_Code_Point Other_Grapheme_Extend Other_ID_Continue>,
    <Other_ID_Start Other_Lowercase Other_Math Other_Uppercase Pattern_Syntax>,
    <Pattern_White_Space Quotation_Mark Radical Soft_Dotted STerm Terminal_Punctuation>,
    <Unified_Ideograph Variation_Selector White_Space>;
    # Perlish stuff
my Str @perl_cats = <alnum alpha ascii cntrl digit graph lower print>,
    <punct space title upper xdigit word vspace hspace>;
BEGIN {
    @mktab_subs.push: my sub mktab_proplist(--> Void) {
        process_file 'ucd/Proplist.txt', rule { $<cr>=<ucd::code_or_range> ';' $<name>=(\w+) }, {
            my $n := $<cr><ord>;
            %category{$<name>}.add($n);
            #XXX where is this stuff defined in terms of unicode stuff?
            # my guesses here are probably very wrong, erring toward too inclusive...
            %category<alnum>.add($n)    if $<name> eq any <Hex_Digit Other_Alphabetic Other_ID_Start Other_Lowercase Other_Uppercase>;
            %category<alpha>.add($n)    if $<name> eq any <Other_Alphabetic Other_Lowercase Other_Uppercase>;
            %category<cntrl>.add($n)    if $<name> eq any <Bidi_Control Join_Control>;
            %category<lower>.add($n)    if $<name> eq any <Other_Lowercase>;
            %category<punct>.add($n)    if $<name> eq any <Dash Hyphen Pattern_Syntax Quotation_Mark STerm Terminal_Punctuation>;
            %category<space>.add($n)    if $<name> eq any <White_Space Pattern_White_Space>;
            %category<upper>.add($n)    if $<name> eq any <Other_Uppercase>;
            %category<xdigit>.add($n)   if $<name> eq any <ASCII_Hex_Digit Hex_Digit>;
            %category<word>.add($n)     if $<name> eq any <Hex_Digit Other_Alphabetic Other_ID_Start Other_Lowercase Other_Uppercase>;
        }
    }
    # back to UnicodeData.txt
    @mktab_ud_subs.push: my sub mktab_ud_isperl(*$code, Str *$name, Str *$gc, Str *@f --> Void) {
        $code.=hex;
        my Str $maj = $gc.substr(0, 1);
        # This is all from Camel3 p.168
        %category<alnum>.add($code)     if $gc     eq any <Lu Ll Lt Lo Nd>;
        %category<alpha>.add($code)     if $gc     eq any <Lu Ll Lt Lo>;
        %category<ascii>.add($code)     if $code   < 0x80;
        %category<cntrl>.add($code)     if $maj    eq 'C';
        %category<digit>.add($code)     if $gc     eq 'Nd';
        %category<lower>.add($code)     if $gc     eq 'Ll';
        %category<print>.add($code)     if $maj    ne 'C';
        %category<punct>.add($code)     if $maj    eq 'P';
        if $maj eq 'Z' {
            %category<space>.add($code);
        } else {
            %category<graph>.add($code) if $maj    ne 'C';
        }
        # guessing here...
        %category<title>.add($code)     if $gc     eq 'Lt';
        %category<upper>.add($code)     if $gc     eq 'Lt'|'Lu';
        %category<word>.add($code)      if $gc     eq any <Lu Ll Lt Lo Nd>;
        # guessing here...
        %category<hspace>.add($code)    if $maj    eq 'Z' and $gc eq none <Zl Zp>;
        %category<vspace>.add($code)    if $gc     eq any <Zl Zp>;
    }
    #XXX are things like /\w/ automatically hooked up to <word> etc.?
    # the answer is token regex_backslash:w
    @dump_init_subs.push: my sub dump_init_gc(--> Void) {
        for @gen_cats, @proplist_cats -> Str $cat {
            install_token "is$cat", -> Str $s { %category{$cat}.contains($s.ord) };
        }
        for @perl_cats -> Str $cat {
            install_token $cat,     -> Str $s { %category{$cat}.contains($s.ord) };
        }
    }
}
# 3 Canonical_Combining_Class
my Int %ccc;
# 4 Bidi_Class
my Utable $bidi_class;
# 5 Decomposition_Mapping
my Str %compat_decomp;
my Str %compat_decomp_type;
my Str %canon_decomp;
# 6 decimal digit
# 7 digit
# 8 numeric
my Hash of Num %numeric;
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

BEGIN{
    @mktab_ud_subs.push: my sub mktab_ud_rest(Str *$code, Str *$name, Str *$gc,
        Str *$ccc, Str *$bdc, Str *$decomp, Str *$dec, Str *$dig, Str *$num,
        Str *$bdm, Str *$u1n, Str *$iso, Str *$uc, Str *$lc, Str *$tc, Str *@f --> Void) {
            
            $code.=hex.=chr;
            %ccc{$code} = +$ccc if $ccc.chars;
            $bidi_class.add($code.ord, :val($bdc)) if $bdc.chars;
            #XXX which \w is this?
            if $decomp ~~ mm{ '<' \w+ '>'} {
                # compat
                my Str $decomp_type = $decomp.substr(0, $/.to);
                $decomp.=substr($/.to);
                $decomp = [~] $decomp.comb».hex.chr;
                %compat_decomp_type{$code} = $decomp_type;
                %compat_decomp{$code} = $decomp;
            } else {
                # canon
                $decomp = [~] $decomp.comb».hex.chr;
                %canon_decomp{$code} = $decomp if $decomp.chars;
            }
            %numeric<Decimal>{$code} = +$dec if $dec.chars;
            %numeric<Digit>{$code}   = +$dig if $dig.chars;
            %numeric<Numeric>{$code} = +$num if $num.chars;
            $bidi_mirrored.add($code.ord) if $bdm ~~ m:i{ y };
            %upper{$code} = hex $uc if $uc.chars;
            %lower{$code} = hex $lc if $lc.chars;
            %title{$code} = hex $tc if $tc.chars;
    }
}

# ArabicShaping.txt
#     post-6.0?

# STD needs this
my Str %open2close;
# these are used to construct %open2close and are not dumped
my Str %ps_to_pe;
my Str $all_codes;
BEGIN {
    @mktab_ud_subs.push: my sub mktab_ud_open2close(Str *$code, Str *@f --> Void) {
        $all_codes ~= $code.hex.chr;
    }
}
# run all the @mktab_ud_subs here
@mktab_subs.push: my sub mktab_ud_all(--> Void) {
    process_file 'ucd/UnicodeData.txt', -> Str $line {
        my Str @f = $line.split(';');
        $_.(@f) for @mktab_ud_subs;
    }

    # Some special cases added here
    for «\t \n \r \f»».ord -> Int $c {
        %category<space>.add($c);
    }
    %category<word>.add('_'.ord);
    # most of these aren't listed in UnicodeData.txt.  is there other stuff like this?
    # AC00;<Hangul Syllable, First>;Lo;0;L;;;;;N;;;;;
    # D7A3;<Hangul Syllable, Last>;Lo;0;L;;;;;N;;;;;
    %category<Lo>.add(0xAC00..0xD7A3);
    $bidi_class.add(0xAC00..0xD7A3, :val<L>);

    dumphash(  :%category);
    dumphash(  :%numeric);
    dumphash(  :%ccc);
    dumputable(:$bidi_class);
    dumputable(:$bidi_mirrored);
    dumphash(  :%upper);
    dumphash(  :%lower);
    dumphash(  :%title);
}
# BidiMirroring.txt
# 0 code
# 1 mirrored code
my Str %bidi_mirror;
BEGIN {
    @mktab_subs.push: my sub mktab_bidi_mirror(--> Void) {
        process_file 'ucd/BidiMirroring.txt', -> Str $line {
            my $code, $mirrored_code = $line.split(';')».hex.chr;
            %bidi_mirror{$code} = $mirrored_code;
            if $code < $mirrored_code
                and not %category{'Ps'|'Pe'}.contains($code|$mirrored_code) {
                    %open2close{$code} = $mirrored_code;
            }
        }
        dumphash(:%bidi_mirror);

        # only map first Pe after each Ps
        # this is easier to do with a state machine
        $all_codes ~~ token {
            [
                <-isPs>*
                (<isPs>)
                <-isPe-isPs>*
                (<isPe>?)
                { %ps_to_pe{$0} = $1 if $1.chars }
            ]*
        }
        # ps_to_pe take precedence over BidiMirroring mappings
        %open2close = %(@(%open2close), @(%ps_to_pe));
        dumphash(:%open2close);
    }
}

# Blocks.txt
# 0 code range
# 1 block name
# what do we do with block names?
my Utable $blockname;
BEGIN {
    @mktab_subs.push: my sub mktab_blocks(--> Void) {
        process_file 'ucd/Blocks.txt', rule { <r=ucd::code_or_range> ';' $<name>=(\S+\N*) }, {
            $blockname.add($<r><ord>, :val($<name>));
        }
        dumputable(:$blockname);
    }
}

# CompositionExclusions.txt
# DerivedNormalizationProps.txt has the full list

# CaseFolding.txt
# 0 code
# 1 status
# 2 mapping
my Hash of Str %casefold;
BEGIN {
    @mktab_subs.push: my sub mktab_casefold(--> Void) {
        process_file 'ucd/CaseFolding.txt', rule { <c=ucd::code> ';' (<[FTSC]>) ';' <map=ucd::code_or_seq> ';' }, {
            %casefold{$0}{$<c><str>} = $<map><str>;
        }
        dumphash(:%casefold);
    }
}

# DerivedAge.txt
#     post-6.0?

# EastAsianWidth.txt
#     post-6.0?

# HangulSyllableType.txt
my Utable $hangul;
BEGIN {
    @mktab_subs.push: my sub mktab_hst(--> Void) {
        process_file 'ucd/HangulSyllableType.txt', rule { <n=ucd::code_or_range> ';' $<name>=(\w+) }, {
            $hangul.add($<n><ord>, :val($<name>));
        }
        dumputable(:$hangul);
    }
    @dump_init_subs.push: my sub dump_init_hst(--> Void) {
        for %hst_alias.keys -> Str $st {
            install_token "isHST$st", -> Str $s       { $hangul.get($s.ord) eq $st                         };
            my Str $sta = %hst_alias{$st};
            install_token "isHST$sta", -> Str $s      { $hangul.get($s.ord) eq $st                         };
        }
    }
}


# Jamo.txt
#     post-6.0?

# LineBreak.txt
#     post-6.0?

# NameAliases.txt
# 0 code
# 1 name
#     see note about names in UnicodeData.txt
# UAX#15 also has code for hangul syllable names

# NormalizationCorrections.txt
# 0 code
# 1 Original (erroneous) decomposition
# 2 Corrected decomposition
# 3 version corrected
BEGIN {
    @mktab_subs.push: my sub mktab_norm_correct(--> Void) {
        process_file 'ucd/NormalizationCorrections.txt',
            rule { <c=ucd::code> ';' <orig=ucd::code_or_seq> ';' <corr=ucd::code_or_seq> ';' }, {
                my Str $decomp = $<corr><str>;
                my Int $code = $<c><ord>;
                if $decomp ~~ rule { '<' \w+ '>'} {
                    # compat    
                    my Str $decomp_type = $decomp.substr(0, $/.to);
                    $decomp.=substr($/.to);
                    $decomp = [~] $decomp.comb».hex.chr;
                    %compat_decomp_type{$code} = $decomp_type;
                    %compat_decomp{$code} = $decomp;
                } else {    
                    # canon     
                    $decomp = [~] $decomp.comb».hex.chr;
                    %canon_decomp{$code} = $decomp if $decomp.chars;
                }           
        }
        dumphash(  :%compat_decomp_type);
        dumphash(  :%compat_decomp);
        dumphash(  :%canon_decomp);
    }
}

# PropertyAliases.txt
# 0 abbrev
# 1 full name
# 2... more aliases
# do we actually need any of this?

# PropertyValueAliases.txt
# 0 prop
# 1 abbrev
# 2 full name
my Str %bidi_class_alias;
my Str %decomp_type_alias;
my Str %gc_alias;
my Str %hst_alias;
my Str %numeric_alias;
my Str %script_alias;
# -- for ccc ---
# 0 ccc
# 1 ccc num
# 2 abbrev
my Str %ccc_abbrev;
# 3 full name
my Str %ccc_full;
BEGIN {
    @mktab_subs.push: my sub mktab_pva(--> Void) {
        process_file 'ucd/PropertyValueAliases.txt', rule { $<prop>=(\w+) [ ';' @<alias>=(\w+) ]+ }, {
            given $<prop>  {
                when 'bc'  { %bidi_class_alias{    @<alias>[0]} = @<alias>[1]; }
                when 'dt'  { %decomp_type_alias{lc @<alias>[1]} = @<alias>[0]; }
                when 'ccc' { %ccc_abbrev{       lc @<alias>[1]} = @<alias>[0];
                             %ccc_full{         lc @<alias>[2]} = @<alias>[0]; }
                when 'gc'  { %gc_alias{            @<alias>[0]} = @<alias>[1]; }
                when 'hst' { %hst_alias{           @<alias>[0]} = @<alias>[1]; }
                when 'nt'  { %numeric_alias{       @<alias>[1]} = @<alias>[0]
                                                 unless @<alias>[0] eq 'None'; }
                when 'sc'  { %script_alias{        @<alias>[1]} = @<alias>[0]; }
            }
        }
        dumphash(:%bidi_class_alias);
        dumphash(:%decomp_type_alias);
        dumphash(:%gc_alias);
        dumphash(:%hst_alias);
        dumphash(:%numeric_alias);
        dumphash(:%script_alias);
        dumphash(:%ccc_abbrev);
        dumphash(:%ccc_full);
    }
    @dump_init_subs.push: my sub dump_init_pva(--> Void) {
        our token isBidiMirrored is export { (.) <?{ $bidi_mirrored.contains($0.ord)                    }> }
        our token isDecoCanon is export    { (.) <?{ exists %canon_decomp{$0}                           }> }
        our token isDecoCompat is export   { (.) <?{ exists %compat_decomp{$0}                          }> }
        # generic smartmatch isccc, can deal with Int, Range, List, Code:(Int), etc.
        our multi token isccc($n) is export      { (.) <?{ ( !exists %ccc{$0} and $n ~~ 0 )
                                                                               or %ccc{$0} ~~ $n
                                                                                                        }> }
        # however, isccc('Virama') "matches" 0, so a Str version is required
        our multi token isccc(Str $n) is export      { (.) <?{ ( !exists %ccc{$0} and lc $n eq 'none' )
                                                        or %ccc{$0} ~~ %ccc_abbrev{lc $n}|%ccc_full{lc $n}
                                                                                                        }> }
        for %decomp_type_alias.keys -> Str $dc {
            install_token "isDC$dc", -> Str $s     { %compat_decomp_type{$s} eq $dc                     };
            my Str $dca = %decomp_type_alias{$dc};
            install_token "isDC$dca", -> Str $s    { %compat_decomp_type{$s} eq $dc                     };
        }
        for %bidi_class_alias.keys -> Str $bc {
            install_token "isBidi$bc", -> Str $s   { $bidi_class.get($s.ord) eq $bc                     };
            my Str $bca = %bidi_class_alias{$bc};
            install_token "isBidi$bca", -> Str $s  { $bidi_class.get($s.ord) eq $bc                     };
        }
        for %gc_alias.keys -> Str $gc {
            my Str $gca = %gc_alias{$gc};
            install_token "is$gca", -> Str $s      { %category{$gc}.contains($s.ord)                    };
        }
        for %numeric_alias.keys -> Str $nt {
            install_token "is$nt", -> Str $s       { exists %numeric{$nt}{$s}                           };
            my Str $nta = %numeric_alias{$nt};
            install_token "is$nta", -> Str $s      { exists %numeric{$nt}{$s}                           };
        }
    }
}

# Scripts.txt
# 0 code range
# 1 script name
my Utable $script;
BEGIN {
    @mktab_subs.push: my sub mktab_script(--> Void) {
        process_file 'ucd/Scripts.txt', rule { <n=ucd::code_or_range> ';' $<name>=(\w+) }, {
            $script.add($<n><ord>, :val($<name>));
        }
        dumputable(:$script);
    }
    @dump_init_subs.push: my sub dump_init_script(--> Void) {
        for %script_alias.keys -> Str $sc {
            install_token "in$sc", -> Str $s       { $script.get($s.ord) eq $sc                         };
            my Str $sca = %script_alias{$sc};
            install_token "in$sca", -> Str $s      { $script.get($s.ord) eq $sc                         };
        }
    }
}

# SpecialCasing.txt
# 0 code
# 1 lower
my Str %lower_spec;
# 2 title
my Str %title_spec;
# 3 upper
my Str %upper_spec;
# 4 conditionals
my List of Hash of Str %case_cond;
BEGIN {
    @mktab_subs.push: my sub mktab_spcase(--> Void) {
        process_file 'ucd/SpecialCasing.txt',
            rule {
                    <c=ucd::code>
                ';' <lower=ucd::code_or_seq>
                ';' <title=ucd::code_or_seq>
                ';' <upper=ucd::code_or_seq>
                ';' $<cond>=(\N*)
            }, {
                if $<cond>.chars {
                    %case_cond{$<c><str>}.push: %{
                        :lower($<lower><str>),
                        :title($<title><str>),
                        :upper($<upper><str>),
                        :cond($<cond>),
                    };
                } else {
                    %lower_spec{$<c><str>} = $<lower><str>;
                    %title_spec{$<c><str>} = $<title><str>;
                    %upper_spec{$<c><str>} = $<upper><str>;
                }
        }
        dumphash(:%lower_spec);
        dumphash(:%title_spec);
        dumphash(:%upper_spec);
        dumphash(:%case_cond);
    }
}

# Unihan.txt
#     post-6.0?

# DerivedCoreProperties.txt
BEGIN {
    @dump_init_subs.push: my sub dump_init_derived_core(--> Void) {
        our token isMath is export { <+isSm+isOther_Math> }
        our token isAlphabetic is export { <+isLu+isLl+isLt+isLm+isLo+isNl+isOther_Alphabetic> }
        our token isLowercase is export { <+isLl+isOther_Lowercase> }
        our token isUppercase is export { <+isLu+isOther_Uppercase> }
        our token isID_Start is export { <+isLu+isLl+isLt+isLm+isLo+isNl+isOther_ID_Start> }
        our token isID_Continue is export { <+isID_Start+isMn+isMc+isNd+isPc+isOther_ID_Continue> }
        our token isXID_Start is export { {...} }
        our token isXID_Continue is export { {...} }
        our token isDefault_Ignorable_Code_Point is export {
            <+isOther_Default_Ignorable_Code_Point+isCf+isCc+isCs+isNoncharacters-isWhite_Space-[\x{FFF9}..\x{FFF9}]>
        }
        our token isGrapheme_Extend is export { <+isMe+isMn+isOther_Grapheme_Extend> }
        our token isGrapheme_Base is export { <-isCc-isCf-isCs-isCo-isCn-isZl-isZp-isGrapheme_Extend> }
        our token isGrapheme_Link is export { <isccc('Virama')> }
    }
}

# DerivedNormalizationProps.txt
my Utable $compex;
my Str %composition;
BEGIN {
    @mktab_subs.push: my sub mktab_compex(--> Void) {
        process_file 'ucd/DerivedNormalizationProps.txt', -> Str $line {
            if $line ~~ rule { <r=ucd::code_or_range> ';' Full_Composition_Exclusion } {
                $compex.add($<r><ord>);
            }
        }
        for %canon_decomp.keys -> Str $s {
            next if $compex.contains($s.ord);
            %composition{%canon_decomp{$s}.nfd} = $s;
        }
        dumphash(:%composition);
    }
}

# GraphemeBreakProperty.txt

# SentenceBreakProperty.txt
#     post-6.0?

# WordBreakProperty.txt

# Unicode algorithms

# Canonical Ordering                             Section 3.11

# Default Case Detection                         Section 3.13
# Default Caseless Matching                      Section 3.13 and Section 5.18
our token iscased is export { <+isLt+isUppercase+isLowercase> }
# from UAX #29, doesn't belong here...
our token isWord_BreakMidLetter is export { <[\x{0027}\x{00B7}\x{05F4}\x{2019}\x{2027}\x{003A}]> }
our token iscase_ignorable is export { <+isMn+isMe+isCf+isLm+isSk+isWord_BreakMidLetter> }
our token final_sigma(StrPos $pos) {
    <after <iscased> <iscase_ignorable>* >
    <at($pos)> .
    <!before <iscase_ignorable>* <iscased> >
}
our token after_soft_dotted(StrPos $pos) is export {
    <after <isSoft_Dotted> <-isccc(0|230)>* >
    <at($pos)> .
}
our token more_above(StrPos $pos) is export {
    <at($pos)> .
    <before <-isccc(0)>* <isccc(230)> >
}
our token before_dot(StrPos $pos) is export {
    <at($pos)> .
    <before <-isccc(0|230)>* \x{0307} >
}
our token after_i(StrPos $pos) is export {
    <after I <-isccc(0|230)>* >
    <at($pos)> .
}

# Hangul Syllable Boundary Determination         Section 3.12
# Hangul Syllable Composition                    Section 3.12
# Hangul Syllable Decomposition                  Section 3.12
# Hangul Syllable Name Generation                Section 3.12
#     post-6.0?

# Bidirectional Algorithm                        UAX #9
#     post-6.0?

# Line Breaking Algorithm                        UAX #14
#     post-6.0?

# Word Boundary Determination                    UAX #29

# Sentence Boundary Determination                UAX #29
#     post-6.0?

# Default Identifier Determination               UAX #31
# Alternative Identifier Determination           UAX #31
# Pattern Syntax Determination                   UAX #31
# Identifier Normalization                       UAX #31
# Identifier Case Folding                        UAX #31
# Standard Compression Scheme for Unicode (SCSU) UTS #6
# Collation Algorithm (UCA)                      UTS #10
#     post-6.0?

require ucd_basic_dump;
# run all the @dump_init_subs here
$_.() for @dump_init_subs;
