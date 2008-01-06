module Unicode;

use Utable;

constant Int $unicode_max = 0x10ffff;

grammar UCD {
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
sub process_file(Str $file, Code &each_line:(Str) -->) {
    my $fd = open $file, :r;
    for =$fd -> my Str $line {
        $line.=UCD::strip_comments;
        next if $line !~~ &UCD::not_empty;
        each_line($line);
    }
    $fd.close;
}

# call like dumphash($dumpfile, :%hash);
sub dumphash(IO $dumpfile, Pair $thing -->) {
    my Str $name := $thing.k;
    my $hash := $thing.v;
    $dumpfile.say: "\%$name := " ~ $hash.perl ~ ";\n";
}
sub dumputable(IO $dumpfile, Pair $thing -->) {
    my Str $name := $thing.k;
    my $utable := $thing.v;
    $dumpfile.say: "\$$name := " ~ $utable.perl ~ ";\n";
}

# we have perl6 -MUnicode -e mktables in lieu of a script
my Code @mktab_subs;
our sub mktables(-->) {
    # requires .txt files from e.g. http://www.unicode.org/Public/zipped/5.0.0/UCD.zip
    if ! 'ucd/Proplist.txt' ~~ :e {
        if ! 'ucd/UCD.zip' ~~ :e {
            mkdir 'ucd' unless 'ucd' ~~ :d;
            chdir 'ucd';
            system 'wget', 'http://www.unicode.org/Public/zipped/5.0.0/UCD.zip';
            chdir '..';
        }
        chdir 'ucd';
        system 'unzip', 'UCD.zip';
        chdir '..';
    }
    # collect all the init subs and run them
    my $dumpfile = open 'ucd_basic_dump.pm', :w;
    $_.($dumpfile) for @mktab_subs;
    $dumpfile.close;
}

# UnicodeData.txt
my Code @mktab_ud_subs;
# 0 codepoint
# 1 name
#     it would be a shame if all p6 programs had to carry around a huge name->codepoint table
#     I figure \c doesn't need to be efficient, so we can skip it and just grep UnicodeData.txt
our sub char_named(Str $name --> Codepoint) { ... }
# 2 General_Category
my Utable %is;
my Str @gen_cats = <Lu Ll Lt Lm Lo LC L>, <Mn Mc Me M>, <Nd Nl No N>,
    <Pc Pd Ps Pe Pi Pf Po P>, <Sm Sc Sk So S>,
    <Zs Zl Zp Z>, <Cc Cf Cs Co Cn C>;
BEGIN {
    @mktab_ud_subs.push: my sub mktab_ud_isgc(*$code, Str *$name, Str *$gc, Str *@f -->) {
        $code.=hex;
        %is{$gc}.add($code);
        %is<LC>.add($code) if $gc eq 'Lu'|'Ll'|'Lt';
        my Str $maj = $gc.substr(0, 1); # first letter
        %is{$maj}.add($code);
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
    for @gen_cats, @proplist_cats, @perl_cats -> my Str $cat {
        %is{$cat} = Utable.new;
    }
    @mktab_subs.push: my sub mktab_proplist(IO $dumpfile -->) {
        process_file 'ucd/Proplist.txt', -> my Str $line {
            $line ~~ mm{ $<cr>=<UCD::code_or_range> ';' $<name>=(\w+) }
                orelse die "Couldn't parse Proplist.txt line '$line'";
            my $n := $<cr><ord>;
            %is{$<name>}.add($n);
            #XXX where is this stuff defined in terms of unicode stuff?
            # my guesses here are probably very wrong, erring toward too inclusive...
            %is<alnum>.add($n) if $<name> eq any <Hex_Digit Other_Alphabetic Other_ID_Start Other_Lowercase Other_Uppercase>;
            %is<alpha>.add($n) if $<name> eq any <Other_Alphabetic Other_Lowercase Other_Uppercase>;
            %is<cntrl>.add($n) if $<name> eq any <Bidi_Control Join_Control>;
            %is<lower>.add($n) if $<name> eq any <Other_Lowercase>;
            %is<punct>.add($n) if $<name> eq any <Dash Hyphen Pattern_Syntax Quotation_Mark STerm Terminal_Punctuation>;
            %is<space>.add($n) if $<name> eq any <White_Space Pattern_White_Space>;
            %is<upper>.add($n) if $<name> eq any <Other_Uppercase>;
            %is<xdigit>.add($n) if $<name> eq any <ASCII_Hex_Digit Hex_Digit>;
            %is<word>.add($n) if $<name> eq any <Hex_Digit Other_Alphabetic Other_ID_Start Other_Lowercase Other_Uppercase>;
        }
    }
    @mktab_ud_subs.push: my sub mktab_ud_isperl(*$code, Str *$name, Str *$gc, Str *@f -->) {
        $code.=hex;
        my Str $maj = $gc.substr(0, 1);
        # This is all from Camel3 p.168
        %is<alnum>.add($code) if $gc eq any <Lu Ll Lt Lo Nd>;
        %is<alpha>.add($code) if $gc eq any <Lu Ll Lt Lo>;
        %is<ascii>.add($code) if $code < 0x80;
        %is<cntrl>.add($code) if $maj eq 'C';
        %is<digit>.add($code) if $gc eq 'Nd';
        %is<lower>.add($code) if $gc eq 'Ll';
        %is<print>.add($code) if $maj ne 'C';
        %is<punct>.add($code) if $maj eq 'P';
        if $maj eq 'Z' {
            %is<space>.add($code);
        } else {
            %is<graph>.add($code) if $maj ne 'C';
        }
        # guessing here...
        %is<title>.add($code) if $gc eq 'Lt';
        %is<upper>.add($code) if $gc eq 'Lt'|'Lu';
        %is<word>.add($code) if $gc eq any <Lu Ll Lt Lo Nd>;
        # guessing here...
        %is<hspace>.add($code) if $maj eq 'Z' and $gc eq none <Zl Zp>;
        %is<vspace>.add($code) if $gc eq any <Zl Zp>;
    }
    #XXX are things like /\w/ automatically hooked up to <word> etc.?
    for @gen_cats, @proplist_cats -> my Str $cat {
        eval "our token is$cat is export { (.) <?{ \%is<$cat>.contains(\$0.ord) }> }";
    }
    for @perl_cats -> my Str $cat {
        eval "our token $cat is export { (.) <?{ \%is<$cat>.contains(\$0.ord) }> }";
    }
}
# 3 Canonical_Combining_Class
my Int %ccc;
# 4 Bidi_Class
my Utable $bidi_class.=new;
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
my Utable $bidi_mirrored.=new;
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
        Str *$bdm, Str *$u1n, Str *$iso, Str *$uc, Str *$lc, Str *$tc, Str *@f -->) {
            
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
            %dec_digit{$code} = +$dec if $dec.chars;
            %digit{$code} = +$dig if $dig.chars;
            %numeric{$code} = +$num if $num.chars;
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
my Str %ps_to_pe;
BEGIN {
    @mktab_ud_subs.push: my sub mktab_ud_open2close(Str *$code, Str *$name, Str *$gc, Str *@f -->) {
        $code.=hex.=chr;
        if $gc eq 'Ps' {
            my Str $prev_ps = $code;
        }
        if $gc eq 'Pe' and $prev_ps {
            %ps_to_pe{$prev_ps} = $code;
            $prev_ps = undef;
        }
    }
#XXX all @mktab_ud_subs must be defined above here.
    @mktab_subs.push: my sub mktab_ud_all(IO $dumpfile -->) {
        process_file 'ucd/UnicodeData.txt', -> my Str $line {
            my Str @f = $line.split(';');
            $_.(@f) for @mktab_ud_subs;
        }

        # Some special cases added here
        for «\t \n \r \f»».ord -> my Int $c {
            %is<space>.add($c);
        }
        %is<word>.add('_'.ord);

        for keys %is -> my Str $cat {
            $dumpfile.say: "\%is<$cat> := " ~ %is{$cat}.perl ~ ";\n";
        }
        dumphash($dumpfile, :%ccc);
        dumputable($dumpfile, :$bidi_class);
        dumphash($dumpfile, :%compat_decomp_type);
        dumphash($dumpfile, :%compat_decomp);
        dumphash($dumpfile, :%canon_decomp);
        dumphash($dumpfile, :%dec_digit);
        dumphash($dumpfile, :%digit);
        dumphash($dumpfile, :%numeric);
        dumputable($dumpfile, :$bidi_mirrored);
        dumphash($dumpfile, :%upper);
        dumphash($dumpfile, :%lower);
        dumphash($dumpfile, :%title);
    }
}
# BidiMirroring.txt
# 0 code
# 1 mirrored code
my Str %bidi_mirror;
BEGIN {
    @mktab_subs.push: my sub mktab_bidi_mirror(IO $dumpfile -->) {
        process_file 'ucd/BidiMirroring.txt', -> my Str $line {
            my $code, $mirrored_code = $line.split(';')».hex.chr;
            %bidi_mirror{$code} = $mirrored_code;
            if $code < $mirrored_code
                and !%is<Ps>.contains($code.ord) and !%is<Pe>.contains($code.ord)
                and !%is<Ps>.contains($mirrored_code.ord) and !%is<Pe>.contains($mirrored_code.ord) {
                    %open2close{$code} = $mirrored_code;
            }
        }
        dumphash($dumpfile, :%bidi_mirror);

        # ps_to_pe take precedence over BidiMirroring mappings
        %open2close = %(@%open2close, @%ps_to_pe);
        dumphash($dumpfile, :%open2close);
    }
}

# Blocks.txt
# 0 code range
# 1 block name
my Utable $blockname.=new;
BEGIN {
    @mktab_subs.push: my sub mktab_blocks(IO $dumpfile -->) {
        process_file 'ucd/Blocks.txt', -> my Str $line {
            #XXX are \S and \N OK here?
            $line ~~ mm{ $<r>=<UCD::code_or_range> ';' $<name>=(\S+\N*) }
                orelse die "Couldn't parse Blocks.txt line '$line'";
            $blockname.add($<r><ord>, :val($<name>));
        }
        dumputable($dumpfile, :$blockname);
    }
}

# CompositionExclusions.txt
# 0 code
my Bool %compex;
BEGIN {
    @mktab_subs.push: my sub mktab_compex(IO $dumpfile -->) {
        process_file 'ucd/CompositionExclusions.txt', -> my Str $line {
            $line ~~ mm{ $<c>=<UCD::code> }
                orelse die "Couldn't parse CompositionExclusions.txt line '$line'";;
            %compex{$<c><str>}++;
        }
        dumphash($dumpfile, :%compex);
    }
}

# CaseFolding.txt
# 0 code
# 1 status
# 2 mapping
my Hash of Str %casefold;
BEGIN {
    @mktab_subs.push: my sub mktab_casefold(IO $dumpfile -->) {
        process_file 'ucd/CaseFolding.txt', -> my Str $line {
            $line ~~ mm{ $<c>=<UCD::code> ';' (<[FTSC]>) ';' $<map>=<UCD::code_or_seq> ';' }
                orelse die "Couldn't parse CaseFolding.txt line '$line'";
            %casefold{$0}{$<c><str>} = $<map><str>;
        }
        for <F T S C> -> my Str $s {
            $dumpfile.say: "\%casefold<$s> := " ~ %casefold{$s}.perl ~ ";\n";
        }
    }
}

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
my Str %norm_correct;
BEGIN {
    @mktab_subs.push: my sub mktab_norm_correct(IO $dumpfile -->) {
        process_file 'ucd/NormalizationCorrections.txt', -> my Str $line {
            $line ~~ mm{ $<c>=<UCD::code> ';' $<orig>=<UCD::code_or_seq> ';' $<corr>=<UCD::code_or_seq> ';' }
                orelse die "Couldn't parse NormalizationCorrections.txt line '$line'";
            %norm_correct{$<c><str>} = $<corr><str>;
        }
        dumphash($dumpfile, :%norm_correct);
    }
}

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
BEGIN {
    @mktab_subs.push: my sub mktab_script(IO $dumpfile -->) {
        process_file 'ucd/Scripts.txt', -> my Str $line {
            $line ~~ mm{ $<n>=<UCD::code_or_range> ';' $<name>=(\w+) }
                orelse die "Couldn't parse Scripts.txt line '$line'";
            $script.add($<n><ord>, :val($<name>));
        }
        dumputable($dumpfile, :$script);
    }
}

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
BEGIN {
    @mktab_subs.push: my sub mktab_spcase(IO $dumpfile -->) {
        process_file 'ucd/SpecialCasing.txt', -> my Str $line {
            $line ~~ mm{ $<c>=<UCD::code> ';' $<lower>=<UCD::code_or_seq> ';'
                $<title>=<UCD::code_or_seq> ';' $<upper>=<UCD::code_or_seq> ';' $<cond>=(\N*) }
                orelse die "Couldn't parse SpecialCasing.txt line '$line'";
            %upper_cond{$<c><str>} = $<upper><str>;
            %title_cond{$<c><str>} = $<title><str>;
            %lower_cond{$<c><str>} = $<lower><str>;
            %case_cond{$<c><str>} = $<cond>;
        }
        dumphash($dumpfile, :%upper_cond);
        dumphash($dumpfile, :%title_cond);
        dumphash($dumpfile, :%lower_cond);
        dumphash($dumpfile, :%case_cond);
    }
}

# Unihan.txt
#     post-6.0?

# DerivedCoreProperties.txt
# DerivedNormalizationProps.txt

# GraphemeBreakProperty.txt
#     post-6.0?

# SentenceBreakProperty.txt
#     post-6.0?

# WordBreakProperty.txt
#     post-6.0?

require ucd_basic_dump;

# From S29

class Str is also {
    our multi method lc(Str $string: --> Str) is export { ... }
    our multi method lcfirst(Str $string: --> Str) is export { ... }
    our multi method uc(Str $string: --> Str) is export { ... }
    our multi method ucfirst(Str $string: --> Str) is export { ... }
    our multi method capitalize(Str $string: --> Str) is export { ... }
    our multi method normalize(Str $string: Bool :$canonical = Bool::True, Bool :$recompose = Bool::False --> Str) is export { ... }
    our multi method nfd(Str $string: --> Str) is export { $string.normalize(:cononical, :!recompose); }
    our multi method nfc(Str $string: --> Str) is export { $string.normalize(:canonical, :recompose); }
    our multi method nfkd(Str $string: --> Str) is export { $string.normalize(:!canonical, :!recompose); }
    our multi method nfkc(Str $string: --> Str) is export { $string.normalize(:!canonical, :recompose); }
    our multi method ord(Str $string: --> Int|List of Int) is export { ... }

    # Cache a copy of ourself as an array of Codepoints
    has Codepoint @.as_codes;
    # XXX is this even remotely correct?
    &STORE.wrap( { @.as_codes = undef; callsame; } );
    our method to_codes(Str $string: --> List of Codepoint) {
        return @.as_codes if defined @.as_codes;
        $string ~~ m:codes{ @<codes>=(.)* };
        return @.as_codes := @<codes>;
    }

    our multi method chars(Str $string: --> Int) is export {
        # XXX how does the "current unicode level" work?
        &graphs.callsame;
    }

    our multi method bytes(Str $string: --> Int) is export { ... }
    our multi method codes(Str $string: --> Int) is export { +$string.to_codes }
    our multi method graphs(Str $string: --> Int) is export {
        my Int $nc = $string.codes;
        my Int $ng = $nc;
        for $string.to_codes -> my Str $c {
            # discount Marks XXX may be wrong
            $ng-- if %is<M>.contains($c.ord);
            # CRLF is a grapheme in 5.0 and in Perl 6
            # XXX substr must be as codes
            $ng-- if $string.substr(my Int $n++, 2) eq "\r\n";
        }
        return $ng;
    }
}
# Should I assume this stuff is done at a lower level?
# If not, how?
# our class AnyChar is Str { ... }
# our class Uni is AnyChar is Int { ... }
# our Uni multi chr( Uni $codepoint ) { $codepoint }
# our Uni multi ord( Uni $character ) { $character }
# our class Codepoint is Uni { }
# our class Grapheme is AnyChar { ... }
# our class Byte is AnyChar is Int { ... }
# our class CharLingua is AnyChar { ... }
