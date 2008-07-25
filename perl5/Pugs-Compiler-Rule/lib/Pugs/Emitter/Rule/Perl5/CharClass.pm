package Pugs::Emitter::Rule::Perl5::CharClass;

use strict;
use charnames ();
use Data::Dumper;

use vars qw( %char_class %extra_unicode );
BEGIN {
    %char_class = map { $_ => 1 } qw(
        alpha alnum ascii blank
        cntrl digit graph lower
        print punct space upper
        word  xdigit
    );
    %extra_unicode = (
        'isLr'       => '(?:\p{isLl}|\p{isLu}|\p{isLt})',
        'isInLatin1Supplement'      => '[\x{0080}–\x{00FF}]',
        'isInCyrillicSupplementary' => '[\x{0500}–\x{052F}]',

        # http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:Grapheme_Link=True:]
        'isGraphemeLink'  => '[[् ্ ੍ ્ ୍ ் ్ ್ ് ්  ꠆ ꣄ ᮪ 𐨿 ฺ ྄ ᜔ ᜴ ꥓ ္ ်  ្ ᭄]',
        
    # $ perl -MSet::IntSpan -ne ' @x=split/;/; $v=hex($x[0]); push @nums, $v if $x[4] eq "EN"; END{ $set= new Set::IntSpan @nums; $set="$set"; $set =~ s/(\d+)/ sprintf("\\x{%0X}",$1) /ge; print $set,"\n"}'  
    #     /opt/local/lib/perl5/5.8.8/unicore/UnicodeData.txt 

        'isBidiEN'   => '[\x{30}-\x{39},\x{B2}-\x{B3},\x{B9},\x{6F0}-\x{6F9},\x{2070},\x{2074}-\x{2079},\x{2080}-\x{2089},\x{2488}-\x{249B},\x{FF10}-\x{FF19},\x{1D7CE}-\x{1D7FF}]',

        'isBidiWS'   => '[\x{0C},\x{20},\x{1680},\x{180E},\x{2000}-\x{200A},\x{2028},\x{205F},\x{3000}]',

        'isBidiET'   => '[\x{23}-\x{25},\x{A2}-\x{A5},\x{B0}-\x{B1},\x{66A},\x{9F2}-\x{9F3},\x{AF1},\x{BF9},\x{E3F},\x{17DB},\x{2030}-\x{2034},\x{20A0}-\x{20B5},\x{212E},\x{2213},\x{FE5F},\x{FE69}-\x{FE6A},\x{FF03}-\x{FF05},\x{FFE0}-\x{FFE1},\x{FFE5}-\x{FFE6}]',

        'isBidiES'   => '\x{2F}',    # SOLIDUS ???

        'isBidiR'    => '[\x{5BE},\x{5C0},\x{5C3},\x{5C6},\x{5D0}-\x{5EA},\x{5F0}-\x{5F4},\x{200F},\x{FB1D},\x{FB1F}-\x{FB28},\x{FB2A}-\x{FB36},\x{FB38}-\x{FB3C},\x{FB3E},\x{FB40}-\x{FB41},\x{FB43}-\x{FB44},\x{FB46}-\x{FB4F},\x{10800}-\x{10805},\x{10808},\x{1080A}-\x{10835},\x{10837}-\x{10838},\x{1083C},\x{1083F},\x{10A00},\x{10A10}-\x{10A13},\x{10A15}-\x{10A17},\x{10A19}-\x{10A33},\x{10A40}-\x{10A47},\x{10A50}-\x{10A58}]',

        'isBidiL'    => '[\x{41}-\x{5A},\x{61}-\x{7A},\x{AA},\x{B5},\x{BA},\x{C0}-\x{D6},\x{D8}-\x{F6},\x{F8}-\x{241},\x{250}-\x{2B8},\x{2BB}-\x{2C1},\x{2D0}-\x{2D1},\x{2E0}-\x{2E4},\x{2EE},\x{37A},\x{386},\x{388}-\x{38A},\x{38C},\x{38E}-\x{3A1},\x{3A3}-\x{3CE},\x{3D0}-\x{3F5},\x{3F7}-\x{482},\x{48A}-\x{4CE},\x{4D0}-\x{4F9},\x{500}-\x{50F},\x{531}-\x{556},\x{559}-\x{55F},\x{561}-\x{587},\x{589},\x{903}-\x{939},\x{93D}-\x{940},\x{949}-\x{94C},\x{950},\x{958}-\x{961},\x{964}-\x{970},\x{97D},\x{982}-\x{983},\x{985}-\x{98C},\x{98F}-\x{990},\x{993}-\x{9A8},\x{9AA}-\x{9B0},\x{9B2},\x{9B6}-\x{9B9},\x{9BD}-\x{9C0},\x{9C7}-\x{9C8},\x{9CB}-\x{9CC},\x{9CE},\x{9D7},\x{9DC}-\x{9DD},\x{9DF}-\x{9E1},\x{9E6}-\x{9F1},\x{9F4}-\x{9FA},\x{A03},\x{A05}-\x{A0A},\x{A0F}-\x{A10},\x{A13}-\x{A28},\x{A2A}-\x{A30},\x{A32}-\x{A33},\x{A35}-\x{A36},\x{A38}-\x{A39},\x{A3E}-\x{A40},\x{A59}-\x{A5C},\x{A5E},\x{A66}-\x{A6F},\x{A72}-\x{A74},\x{A83},\x{A85}-\x{A8D},\x{A8F}-\x{A91},\x{A93}-\x{AA8},\x{AAA}-\x{AB0},\x{AB2}-\x{AB3},\x{AB5}-\x{AB9},\x{ABD}-\x{AC0},\x{AC9},\x{ACB}-\x{ACC},\x{AD0},\x{AE0}-\x{AE1},\x{AE6}-\x{AEF},\x{B02}-\x{B03},\x{B05}-\x{B0C},\x{B0F}-\x{B10},\x{B13}-\x{B28},\x{B2A}-\x{B30},\x{B32}-\x{B33},\x{B35}-\x{B39},\x{B3D}-\x{B3E},\x{B40},\x{B47}-\x{B48},\x{B4B}-\x{B4C},\x{B57},\x{B5C}-\x{B5D},\x{B5F}-\x{B61},\x{B66}-\x{B71},\x{B83},\x{B85}-\x{B8A},\x{B8E}-\x{B90},\x{B92}-\x{B95},\x{B99}-\x{B9A},\x{B9C},\x{B9E}-\x{B9F},\x{BA3}-\x{BA4},\x{BA8}-\x{BAA},\x{BAE}-\x{BB9},\x{BBE}-\x{BBF},\x{BC1}-\x{BC2},\x{BC6}-\x{BC8},\x{BCA}-\x{BCC},\x{BD7},\x{BE6}-\x{BF2},\x{C01}-\x{C03},\x{C05}-\x{C0C},\x{C0E}-\x{C10},\x{C12}-\x{C28},\x{C2A}-\x{C33},\x{C35}-\x{C39},\x{C41}-\x{C44},\x{C60}-\x{C61},\x{C66}-\x{C6F},\x{C82}-\x{C83},\x{C85}-\x{C8C},\x{C8E}-\x{C90},\x{C92}-\x{CA8},\x{CAA}-\x{CB3},\x{CB5}-\x{CB9},\x{CBD}-\x{CC4},\x{CC6}-\x{CC8},\x{CCA}-\x{CCB},\x{CD5}-\x{CD6},\x{CDE},\x{CE0}-\x{CE1},\x{CE6}-\x{CEF},\x{D02}-\x{D03},\x{D05}-\x{D0C},\x{D0E}-\x{D10},\x{D12}-\x{D28},\x{D2A}-\x{D39},\x{D3E}-\x{D40},\x{D46}-\x{D48},\x{D4A}-\x{D4C},\x{D57},\x{D60}-\x{D61},\x{D66}-\x{D6F},\x{D82}-\x{D83},\x{D85}-\x{D96},\x{D9A}-\x{DB1},\x{DB3}-\x{DBB},\x{DBD},\x{DC0}-\x{DC6},\x{DCF}-\x{DD1},\x{DD8}-\x{DDF},\x{DF2}-\x{DF4},\x{E01}-\x{E30},\x{E32}-\x{E33},\x{E40}-\x{E46},\x{E4F}-\x{E5B},\x{E81}-\x{E82},\x{E84},\x{E87}-\x{E88},\x{E8A},\x{E8D},\x{E94}-\x{E97},\x{E99}-\x{E9F},\x{EA1}-\x{EA3},\x{EA5},\x{EA7},\x{EAA}-\x{EAB},\x{EAD}-\x{EB0},\x{EB2}-\x{EB3},\x{EBD},\x{EC0}-\x{EC4},\x{EC6},\x{ED0}-\x{ED9},\x{EDC}-\x{EDD},\x{F00}-\x{F17},\x{F1A}-\x{F34},\x{F36},\x{F38},\x{F3E}-\x{F47},\x{F49}-\x{F6A},\x{F7F},\x{F85},\x{F88}-\x{F8B},\x{FBE}-\x{FC5},\x{FC7}-\x{FCC},\x{FCF}-\x{FD1},\x{1000}-\x{1021},\x{1023}-\x{1027},\x{1029}-\x{102A},\x{102C},\x{1031},\x{1038},\x{1040}-\x{1057},\x{10A0}-\x{10C5},\x{10D0}-\x{10FC},\x{1100}-\x{1159},\x{115F}-\x{11A2},\x{11A8}-\x{11F9},\x{1200}-\x{1248},\x{124A}-\x{124D},\x{1250}-\x{1256},\x{1258},\x{125A}-\x{125D},\x{1260}-\x{1288},\x{128A}-\x{128D},\x{1290}-\x{12B0},\x{12B2}-\x{12B5},\x{12B8}-\x{12BE},\x{12C0},\x{12C2}-\x{12C5},\x{12C8}-\x{12D6},\x{12D8}-\x{1310},\x{1312}-\x{1315},\x{1318}-\x{135A},\x{1360}-\x{137C},\x{1380}-\x{138F},\x{13A0}-\x{13F4},\x{1401}-\x{1676},\x{1681}-\x{169A},\x{16A0}-\x{16F0},\x{1700}-\x{170C},\x{170E}-\x{1711},\x{1720}-\x{1731},\x{1735}-\x{1736},\x{1740}-\x{1751},\x{1760}-\x{176C},\x{176E}-\x{1770},\x{1780}-\x{17B6},\x{17BE}-\x{17C5},\x{17C7}-\x{17C8},\x{17D4}-\x{17DA},\x{17DC},\x{17E0}-\x{17E9},\x{1810}-\x{1819},\x{1820}-\x{1877},\x{1880}-\x{18A8},\x{1900}-\x{191C},\x{1923}-\x{1926},\x{1930}-\x{1931},\x{1933}-\x{1938},\x{1946}-\x{196D},\x{1970}-\x{1974},\x{1980}-\x{19A9},\x{19B0}-\x{19C9},\x{19D0}-\x{19D9},\x{1A00}-\x{1A16},\x{1A19}-\x{1A1B},\x{1A1E}-\x{1A1F},\x{1D00}-\x{1DBF},\x{1E00}-\x{1E9B},\x{1EA0}-\x{1EF9},\x{1F00}-\x{1F15},\x{1F18}-\x{1F1D},\x{1F20}-\x{1F45},\x{1F48}-\x{1F4D},\x{1F50}-\x{1F57},\x{1F59},\x{1F5B},\x{1F5D},\x{1F5F}-\x{1F7D},\x{1F80}-\x{1FB4},\x{1FB6}-\x{1FBC},\x{1FBE},\x{1FC2}-\x{1FC4},\x{1FC6}-\x{1FCC},\x{1FD0}-\x{1FD3},\x{1FD6}-\x{1FDB},\x{1FE0}-\x{1FEC},\x{1FF2}-\x{1FF4},\x{1FF6}-\x{1FFC},\x{200E},\x{2071},\x{207F},\x{2090}-\x{2094},\x{2102},\x{2107},\x{210A}-\x{2113},\x{2115},\x{2119}-\x{211D},\x{2124},\x{2126},\x{2128},\x{212A}-\x{212D},\x{212F}-\x{2131},\x{2133}-\x{2139},\x{213C}-\x{213F},\x{2145}-\x{2149},\x{2160}-\x{2183},\x{2336}-\x{237A},\x{2395},\x{249C}-\x{24E9},\x{26AC},\x{2800}-\x{28FF},\x{2C00}-\x{2C2E},\x{2C30}-\x{2C5E},\x{2C80}-\x{2CE4},\x{2D00}-\x{2D25},\x{2D30}-\x{2D65},\x{2D6F},\x{2D80}-\x{2D96},\x{2DA0}-\x{2DA6},\x{2DA8}-\x{2DAE},\x{2DB0}-\x{2DB6},\x{2DB8}-\x{2DBE},\x{2DC0}-\x{2DC6},\x{2DC8}-\x{2DCE},\x{2DD0}-\x{2DD6},\x{2DD8}-\x{2DDE},\x{3005}-\x{3007},\x{3021}-\x{3029},\x{3031}-\x{3035},\x{3038}-\x{303C},\x{3041}-\x{3096},\x{309D}-\x{309F},\x{30A1}-\x{30FA},\x{30FC}-\x{30FF},\x{3105}-\x{312C},\x{3131}-\x{318E},\x{3190}-\x{31B7},\x{31F0}-\x{321C},\x{3220}-\x{3243},\x{3260}-\x{327B},\x{327F}-\x{32B0},\x{32C0}-\x{32CB},\x{32D0}-\x{32FE},\x{3300}-\x{3376},\x{337B}-\x{33DD},\x{33E0}-\x{33FE},\x{3400},\x{4DB5},\x{4E00},\x{9FBB},\x{A000}-\x{A48C},\x{A800}-\x{A801},\x{A803}-\x{A805},\x{A807}-\x{A80A},\x{A80C}-\x{A824},\x{A827},\x{AC00},\x{D7A3},\x{D800},\x{DB7F}-\x{DB80},\x{DBFF}-\x{DC00},\x{DFFF}-\x{E000},\x{F8FF}-\x{FA2D},\x{FA30}-\x{FA6A},\x{FA70}-\x{FAD9},\x{FB00}-\x{FB06},\x{FB13}-\x{FB17},\x{FF21}-\x{FF3A},\x{FF41}-\x{FF5A},\x{FF66}-\x{FFBE},\x{FFC2}-\x{FFC7},\x{FFCA}-\x{FFCF},\x{FFD2}-\x{FFD7},\x{FFDA}-\x{FFDC},\x{10000}-\x{1000B},\x{1000D}-\x{10026},\x{10028}-\x{1003A},\x{1003C}-\x{1003D},\x{1003F}-\x{1004D},\x{10050}-\x{1005D},\x{10080}-\x{100FA},\x{10100},\x{10102},\x{10107}-\x{10133},\x{10137}-\x{1013F},\x{10300}-\x{1031E},\x{10320}-\x{10323},\x{10330}-\x{1034A},\x{10380}-\x{1039D},\x{1039F}-\x{103C3},\x{103C8}-\x{103D0},\x{10400}-\x{1049D},\x{104A0}-\x{104A9},\x{1D000}-\x{1D0F5},\x{1D100}-\x{1D126},\x{1D12A}-\x{1D166},\x{1D16A}-\x{1D172},\x{1D183}-\x{1D184},\x{1D18C}-\x{1D1A9},\x{1D1AE}-\x{1D1DD},\x{1D400}-\x{1D454},\x{1D456}-\x{1D49C},\x{1D49E}-\x{1D49F},\x{1D4A2},\x{1D4A5}-\x{1D4A6},\x{1D4A9}-\x{1D4AC},\x{1D4AE}-\x{1D4B9},\x{1D4BB},\x{1D4BD}-\x{1D4C3},\x{1D4C5}-\x{1D505},\x{1D507}-\x{1D50A},\x{1D50D}-\x{1D514},\x{1D516}-\x{1D51C},\x{1D51E}-\x{1D539},\x{1D53B}-\x{1D53E},\x{1D540}-\x{1D544},\x{1D546},\x{1D54A}-\x{1D550},\x{1D552}-\x{1D6A5},\x{1D6A8}-\x{1D7C9},\x{20000},\x{2A6D6},\x{2F800}-\x{2FA1D},\x{F0000},\x{FFFFD},\x{100000},\x{10FFFD}]',
    );
}

sub vianame {
    my $c = shift;
    $c =~ s/^\s+//;
    $c =~ s/\s+$//;
    my $s = charnames::vianame($c);
    return $s if defined $s;
    $s = charnames::vianame("LINE FEED (LF)") 
        if $c eq "LINE FEED" || $c eq "LF";
    return $s if $s;
    $s = charnames::vianame("CARRIAGE RETURN (CR)") 
        if $c eq "CARRIAGE RETURN" || $c eq "CR";
    return $s if $s;
    $s = charnames::vianame("FORM FEED (FF)")  
        if $c eq "FORM FEED" || $c eq "FF";
    return $s if $s;
    $s = charnames::vianame("NEXT LINE (NEL)") 
        if $c eq "NEXT LINE" || $c eq "NEL";
    return $s if $s;
    die "unknown unicode name: $c";
}

# input format:
# [
#    '+alpha'
#    '-[z]'
# ]

# TODO - set composition logic
# ( ( ( before +alpha ) | before +digit ) before not-alpha ) before not-digit )

sub emit {
    #print Dumper( $_[0] );
    #print Dumper( @{$_[0]} );
    my @c = map { "$_" } @{$_[0]};
    #print Dumper( @c );
    my $out = '';
    #my $last_cmd = '';
    for ( @c ) {
        my ( $op, $cmd ) = /(.)(.*)/;

        $cmd =~ s/ \\c\[ ([^];]+) \; ([^];]+) \] / 
                "\\x{" . sprintf("%02X", vianame($1)) . "}"
              . "\\x{" . sprintf("%02X", vianame($2)) . "}"
            /xgme;
        $cmd =~ s/ \\c\[ ([^]]+) \] / "\\x[" . sprintf("%02X", vianame($1)) . ']' /xgme;
        $cmd =~ s/ \\C\[ ([^]]+) \] / "\\X[" . sprintf("%02X", vianame($1)) . ']' /xgme;
        $cmd =~ s/ \\o\[ ([^]]+) \] / "\\x[" . sprintf("%02X", oct($1)) . ']' /xgme;
        $cmd =~ s/ \\O\[ ([^]]+) \] / "\\X[" . sprintf("%02X", oct($1)) . ']' /xgme;
        $cmd =~ s/\s//g;
        
        $cmd =~ s/\.\./-/g;  # ranges

        if ( $cmd =~ /^ \[ \\ x \[ (.*) \] \] /x ) {
            $cmd = "(?:\\x{$1})";
        }
        elsif ( $cmd =~ /^ \[ \\ X \[ (.*) \] \] /x ) {
            $cmd = "(?!\\x{$1})\\X";
            #$cmd = "[^\\x{$1}]";
        }

        elsif ( $cmd =~ /^ \s* \[ (.*) /x ) {
           $cmd = '[' . $1;
	    }
        elsif ( $cmd =~ /^ \s* (.*) /x ) {
           my $name = $1;
           $cmd = ( exists $char_class{$name} )
                    ? "[[:$name:]]"
                    : exists $extra_unicode{$name}
                        ? $extra_unicode{$name} 
                        : "\\p{$name}";
        }

        if ( $op eq '+' ) {
            $out .=
                ( $out eq '' )
                ? '(?=' . $cmd . ')'
                : '|(?=' . $cmd . ')';
        }
        elsif ( $op eq '-' ) {
            $out .= '(?!' . $cmd . ')';
        }
        else {
            #print Dumper( @c ), ' == ', $out, "\n";
            die "invalid character set op: $op";
        }
    }
    $out = "(?:$out)\\X";

    #print Dumper( \@c ), ' == ', $out, "\n";

    return $out;
}

1;

