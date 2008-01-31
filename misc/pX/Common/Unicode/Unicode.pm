module Unicode;

use UCD;

constant Int $unicode_max = 0x10ffff;

#XXX need an option to use buf64
class *UBuf is buf32 { }

class *AnyChar {
    # one character - can be a byte, codepoint or grapheme
    has Str $.char;
    # for AnyChar, each of these needs to be defined differently for each unicode level
    multi submethod BUILD(Str $s) {...}
    method STORE(Str $s -->) {...}
    method FETCH(--> Str) {...}
    method Str(--> Str) {...}
}

class *Byte is AnyChar {
    multi submethod BUILD(Str $s) { $.char.as_bytes = @$s.as_bytes[0]; }
    method STORE(Str $s -->)      { $.char.as_bytes = @$s.as_bytes[0]; }
    method FETCH(--> Str) {
        my Str $s;
        $s.as_bytes = $.char.as_bytes;
        return $s;
    }
    method Str(--> Str) {
        my Str $s;
        $s.as_bytes = $.char.as_bytes;
        return $s;
    }
}

class *Codepoint is AnyChar {
    multi submethod BUILD(Str $s) { $.char.as_codes = @$s.as_codes[0]; }
    method STORE(Str $s -->)      { $.char.as_codes = @$s.as_codes[0]; }
    method FETCH(--> Str) {
        my Str $s;
        $s.as_codes = $.char.as_codes;
        return $s;
    }
    method Str(--> Str) {
        my Str $s;
        $s.as_codes = $.char.as_codes;
        return $s;
    }
}

my Grapheme @graph_ids;
my Int %seen_graphs;
class *Grapheme is AnyChar {
    use codepoints;
    # ID is codepoint or a unique ID > $unicode_max
    has Int $.id;
    # these are generated lazily
    has @!as_nfd is UBuf;
    has @!as_nfc is UBuf;
    has @!as_nfkd is UBuf;
    has @!as_nfkc is UBuf;
    method as_nfd(--> UBuf) {
        return @!as_nfd //= @.to_nfd;
    }
    method as_nfc(--> UBuf) {
        return @!as_nfc //= @.to_nfc;
    }
    method as_nfkc(--> UBuf) {
        return @!as_nfkc //= @.to_nfkc;
    }
    method as_nfkd(--> UBuf) {
        return @!as_nfkd //= @.to_nfkd;
    }

    multi submethod BUILD(Grapheme $g: Str $s) {
        return unless $s.as_codes.elems;
        @!as_nfd = nfd_g($s.as_codes);
        if @.as_nfc.elems == 1 {
            $.id = @.as_nfc[0];
            return;
        }
        my Str $sc = ~@.as_nfc;
        if exists %seen_graphs{$sc} {
            $g := @graph_ids[%seen_graphs{$sc}];
            return;
        }
        #XXX will this need some kind of STM protection?
        my Int $graph_num = +@graph_ids;
        $.id = $graph_num + $unicode_max+1;
        die 'Too many unique graphemes' if $.id >= 2 ** 32;
        @graph_ids[$graph_num] := $g;
        %seen_graphs{$sc} = $graph_num;
    }
    # this is like chr under use graphs
    multi submethod BUILD(Grapheme $g: Int $id) {
        if $id <= $unicode_max {
            $.id = $id;
            @!as_nfc = $id;
        } else {
            $g := @graph_ids[$id - ($unicode_max+1)];
        }
    }
    method STORE(Str $s -->) { $.id = @$s.as_graphs[0]; }
    method FETCH(--> Str) {
        my Str $s;
        $s.as_graphs = $.id;
        return $s;
    }
    method Str(--> Str) {
        my Str $s;
        $s.as_graphs = $.id;
        return $s;
    }

    # Normalization Algorithm                        UAX #15
    sub reorder(@b is UBuf --> UBuf) {
        my Str $ret;
        ~@b ~~ token :codes {
            [ $<ns>=[ <isccc(0)>* ]
                { $ret ~= $<ns>[*-1] }
            | @<s>=<isccc(1..*)>*
                { $ret ~= [~] @@<s>[*-1].sort: { %ccc{$^c} } }
            ]*
        };
        return $ret.as_codes;
    }
    sub hangul_decomp(Str $s --> Str) {
        my Int $o = $s.ord;
        # just returns $s for most $s
        return $s if $o !~~ 0xAC00..0xD7A3;
        my Str $ret;
        $o -= 0xAC00;
        $ret ~= (($o / 0x2BA4) + 0x1100).chr;
        $ret ~= (((($o % 0x2BA4) / 28 ) + 0x1161 ).chr;
        my Int $t = ($o % 28 ) + 0x11A7;
        $ret ~= $t.chr if $t != 0x11A7;
        return $ret;
    }
    sub nfd_g(@b is UBuf --> UBuf) {
        my @old is UBuf;
        my @new is UBuf = @b;
        while @old !eqv @new {
            @old = @new;
            @new = ();
            for @old -> my Int $o {
                @new.push: (%canon_decomp{$o.chr} // hangul_decomp($o.chr)).as_codes;
            }
        }
        return reorder @new;
    }
    method to_nfd(--> UBuf) {
        return nfd_g(@!as_nfc);
    }
    method to_nfkd(--> UBuf) {
        my @old is UBuf;
        my @new is UBuf = @.as_nfd;
        while @old !eqv @new {
            @old = @new;
            @new = ();
            for @old -> my Int $o {
                @new.push: (%compat_decomp{$o.chr} // %canon_decomp{$o.chr} // hangul_decomp($o.chr)).as_codes;
            }
        }
        return @new;
    }
    sub compose_hangul(Str $s --> Str) {
        my Str $ret = $s.as_codes[0].chr;
        my Str $prev = $ret;
        for 1..$s.codes -> my Int $n {
            my Str $c = $s.as_codes[$n];
            if $prev ~~ 0x1100..0x1112 
                and $c ~~ 0x1161..0x1175 {
                    $prev = ((($prev - 0x1100) * 21 + ($c - 0x1161)) * 28) + 0xAC00;
                    $ret.as_codes[$ret.codes-1] = $prev;
                    next;
            }
            if $prev ~~ 0xAC00..0xD7A3
                and ($prev - 0xAC00) % 28 == 0
                and $c ~~ 0x11A7..0x11C2 {
                    $prev = $prev + $c - 0x11A7;
                    $ret.as_codes[$ret.codes-1] = $prev;
                    next;
                }
            }
            $prev = $c;
            $ret ~= $c;
        }
        return $ret;
    }
    sub compose_graph(Str $s is copy --> Str) {
        return %composition{$s} // $s
            if $s.codes == 1;
        return compose_hangul($s)
            if $s ~~ &isGCBHangulSyllable;
        startover:
        my Str $one = $s.as_codes[0].chr;
        if exists %composition{$one} {
            my Str $new = %composition{$one};
            $new ~= @$s.as_codes[1..*].chr;
            $s = $new;
            goto startover;
        }
        for 1..$s.codes -> my Int $n {
            my Str $two = $s.as_codes[0].chr ~ $s.as_codes[$n].chr;
            if exists %composition{$two} {
                my Str $new = %composition{$two};
                for 1..$s.codes -> my Int $m {
                    next if $n == $m;
                    $new ~= $s.as_codes[$m].chr;
                }
                $s = $new;
                goto startover;
            }
        }
        return $s;
    }
    method to_nfc(--> UBuf) {
        return compose_graph(~@.as_nfd).as_codes;
    }
    method to_nfkc(--> UBuf) {
        return compose_graph(~@.as_nfkd).as_codes;
    }
    method normalize(Str :$nf = $?NF --> Str) {
        given $nf {
            when 'c'  { return ~@.as_nfc;  }
            when 'd'  { return ~@.as_nfd;  }
            when 'kc' { return ~@.as_nfkc; }
            when 'kd' { return ~@.as_nfkd; }
        }
    }
}

# From S29 (mostly)

BEGIN {
    our Str $?NF;
    our Str $?ENC;
}

module *nf {
    sub EXPORTER(Str $nf is copy) {
        $nf.=lc;
        $nf ~~ s:g/nf|\W//;
        die "Unknown Normalization Form: $nf\n"
            unless $nf eq any <c d kc kd>;
        $?NF ::= $nf;
    }
}

module *encoding {
    sub EXPORTER(Str $enc) {
        $?ENC ::= $enc;
    }
}

class *StrPos {
    # this object is only valid for strings === $s
    has Str $.s;
    # substr of $s from beginning to our pos
    has Str $.sub;
    method bytes(--> Int)  { $.sub.bytes  }
    method codes(--> Int)  { $.sub.codes  }
    method graphs(--> Int) { $.sub.graphs }
    multi infix:<->(StrPos $sp1, StrPos $sp2 --> StrLen) { StrDisp.new(:s1($sp1.sub), :s2($sp2.sub)) }
}
class *StrLen {
    # this is a non-lazy, string-independent length which cannot be converted
    has Int $.bytes;
    has Int $.codes;
    has Int $.graphs;
    my method clear(-->) { $.bytes = $.codes = $.graphs = undef; }
}
class *StrDisp is StrLen {
    # this is the "lazy" StrLen, created only by subtracting StrPos
    has Str $.s1;
    has Str $.s2;
    method bytes(--> Int)  { $.s1.bytes  - $.s2.bytes  }
    method codes(--> Int)  { $.s1.codes  - $.s2.codes  }
    method graphs(--> Int) { $.s1.graphs - $.s2.graphs }
}

class *Str is also {
    # in general only one of these is defined at a time
    # all conversions are automatic:
    # codes -> graphs is always allowed
    # defining $?NF allows graphs -> codes
    # defining $?ENC allows bytes -> codes, bytes -> graphs, codes -> bytes
    # graphs -> bytes requires both $?NF and $?ENC (although in most cases NFC and utf8 are assumed)
    has @!as_graphs is UBuf;
    has @!as_codes is UBuf;
    has @!as_bytes is Buf of int8;

    # Grapheme Cluster Boundary Determination        UAX #29
    token isGCBCR :codes { \x{000D} }
    token isGCBLF :codes { \x{000A} }
    token isGCBControl :codes { <+isZl+isZp+isCc+isCf-[\x{000D}\x{000A}\x{200C}\x{200D}]> }
    token isGCBHangulSyllable :codes {
        | <after <isHSTL>                  >          [ <isHSTL> | <isHSTV> | <isHSTLV> | <isHSTLVT> ]
        |        <isHSTL>                     <before [ <isHSTL> | <isHSTV> | <isHSTLV> | <isHSTLVT> ] >
        | <after [ <isHSTLV> | <isHSTV> ]  >          [ <isHSTV> | <isHSTT> ]
        |        [ <isHSTLV> | <isHSTV> ]     <before [ <isHSTV> | <isHSTT> ]                          >
        | <after [ <isHSTLVT> | <isHSTT> ] >          <isHSTV>
        |        [ <isHSTLVT> | <isHSTT> ]    <before <isHSTV>                                         >
    }
    # "default" / "locale-independent" grapheme cluster
    # text does not need to be normalized
    # relies on longest-token matching
    token grapheme_cluster :codes {
        | <isGCBCR> <isGCBLF>
        | [ <isGCBCR> | <isGCBLF> | <isGCBControl> ]
        | <isGCBHangulSyllable>+ <isGrapheme_Extend>*
        | <-isGCBCR-isGCBLF-isGCBControl> <isGrapheme_Extend>*
        | <isGrapheme_Extend>+
    }
    our method as_graphs(--> UBuf) is rw is export {
        return @!as_graphs if defined @!as_graphs;
        my Str $s;
        $s.as_codes = @.as_codes;
        $s ~~ token :codes{
            [ (<grapheme_cluster>)
                { my Grapheme $g.=new: :s($0[*-1]);
                  @!as_graphs.push: $g.id;
                }
            ]*
        };
        return @!as_graphs;
    }
    # codepoint mode needs to respect lexical $?NF, so we may need to renormalize ourself
    # remember what NF the as_codes is in
    has Str $!cur_nf;
    our method as_codes(--> UBuf) is rw is export {
        if $?NF ne $!cur_nf {
            @!as_codes = @.normalize.as_codes;
            $!cur_nf = $?NF;
        }
        return @!as_codes if defined @!as_codes;
        if defined @!as_graphs and defined $?NF {
            for @!as_graphs -> Int $o {
                @!as_codes.push: Grapheme.new(:id($o)).normalize.as_codes;
            }
        }
        if defined @!as_bytes and defined $?ENC {
            ...;
        }
        return @!as_codes;
    }
    our method as_bytes(--> Buf of int8) is rw is export {
        return @!as_bytes if defined @!as_bytes;
        if defined $?ENC {
            ...;
        }
        return @!as_bytes;
    }

    token :codes split_graph {
        $<st>=[ <-isGrapheme_Extend>* ]
        $<ex>=[ <isGrapheme_Extend>* ]
    }
    our multi method samebase (Str $string: Str $pattern --> Str) is export {
        my Str $ret;
        for ^$string.graphs -> my Int $n {
            #XXX this is wrong, needs substr impl
            $string.substr($n, 1).nfd ~~ &split_graph;
            $ret ~= $<st>;
            $pattern.substr($n, 1).nfd ~~ &split_graph;
            $ret ~= $<ex>;
        }
        return $ret;
    }

    # Default Case Conversion                        Section 3.13
    our multi method lc(Str $string: --> Str) is export {
        ...;
    }
    our multi method lcfirst(Str $string: --> Str) is export {
        ...;
    }
    our multi method uc(Str $string: --> Str) is export {
        ...;
    }
    our multi method ucfirst(Str $string: --> Str) is export {
        ...;
    }
    our multi method capitalize(Str $string: --> Str) is export {
        ...;
    }
    our multi method samecase (Str $string: Str $pattern --> Str) is export {
        ...;
    }
}

module *graphemes {
    class *Str is also {
        our multi method graphs(Str $string: --> Int) is export { $string.as_graphs.elems }
        our multi method chars(Str $string: --> Int) is export { $string.graphs }
        multi submethod BUILD(UBuf :@graphs) { @!as_graphs = @graphs; }
        multi method STORE(Str $s -->) {
            @!as_graphs = $s.as_graphs;
            @!as_codes = undef;
            @!as_bytes = undef;
            $!cur_nf = undef;
        }
        multi method FETCH(--> Str) {
            my Str $s;
            @$s.as_graphs = @.as_graphs;
            return $s;
        }
        our multi *infix:<~>(Str $s1, Str $s2 --> Str) is export {
            my Str $s;
            @$s.as_graphs = @$s1.as_graphs[0..*-2];
            my Str $mid;
            @$mid.as_codes = Grapheme.new(:id(@$s1.as_graphs[*-1])).as_nfc,
                Grapheme.new(:id(@$s2.as_graphs[0])).as_nfc;
            $s.as_graphs.push: @$mid.as_graphs;
            $s.as_graphs.push: @$s2.as_graphs[1..*];
            return $s;
        }
        our multi *infix:<eq>(Str $s1, Str $s2 --> Bool) is export { $s1.as_graphs eqv $s2.as_graphs }
        our multi method ord(Str $string: --> Int) is export { @.as_graphs[0] }
        our multi method ord(Str $string: --> List of Int) is export { @.as_graphs }
        our multi method substr(Str $string: StrPos $start, StrLen $length? --> Str) is rw is export {
            die 'Invalid StrPos for this Str' unless $start.s === $string;
            return class {
                method FETCH(--> Str) {
                    my Str $s;
                    @$s.as_graphs = @$string.as_graphs[ $start.graphs .. ($length.graphs+$start.graphs // *) ];
                    return $s;
                }
                method STORE(Str $s -->) {
                    my Str $s1, $s2, $s3;
                    @$s1.as_graphs = @$string.as_graphs[ 0 .. $start.graphs-1 ];
                    @$s2.as_graphs = @$string.as_graphs[ ($length.graphs+$start.graphs+1 // *) .. * ];
                    $s3 = $s1 ~ $s ~ $s2;
                    $string.as_graphs = $s3.as_graphs;
                }
            };
        }
        our multi method substr(Str $string: StrPos $start, StrPos $end? --> Str) is rw is export {
            die 'Invalid StrPos for this Str' unless $start.s === $string;
            die 'Invalid StrPos for this Str' unless $end.s === $string;
            return class {
                method FETCH(--> Str) {
                    my Str $s;
                    @$s.as_graphs = @$string.as_graphs[ $start.graphs .. ($end.graphs // *) ];
                    return $s;
                }
                method STORE(Str $s -->) {
                    my Str $s1, $s2, $s3;
                    @$s1.as_graphs = @$string.as_graphs[ 0 .. $start.graphs-1 ];
                    @$s2.as_graphs = @$string.as_graphs[ ($end.graphs+1 // *) .. * ];
                    $s3 = $s1 ~ $s ~ $s2;
                    $string.as_graphs = $s3.as_graphs;
                }
            };
        }
    }
    our multi sub *chr(Int *@grid --> Str) {
        my UBuf @graphs = @grid;
        return Str.new(:@graphs);
    }
    class *UBuf is also {
        our multi method Str(UBuf $b: --> Str) { Str.new(:graphs($b)) }
    }
    class *AnyChar is also {
        multi submethod BUILD(Str $s) { $.char.as_graphs = @$s.as_graphs[0]; }
        method STORE(Str $s -->)      { $.char.as_graphs = @$s.as_graphs[0]; }
        method FETCH(--> Str) {
            my Str $s;
            $s.as_graphs = $.char.as_graphs;
            return $s;
        }
        method Str(--> Str) {
            my Str $s;
            $s.as_graphs = $.char.as_graphs;
            return $s;
        }
    }
    class *StrLen is also {
        submethod BUILD(Int $i -->) { $.clear; $.graphs = $i; }
        method STORE(Int $i -->)    { $.clear; $.graphs = $i; }
        method FETCH(--> Int) { $.graphs }
        method Int(--> Int)   { $.graphs }
    }
    class *StrPos is also {
        submethod BUILD(Str $s, StrLen $len) {
            $.s = $s;
            my Str $sub;
            @$sub.as_graphs = @$s.as_graphs[0 .. $len.graphs];
            $.sub = $sub;
        }
        method Int(--> Int) { $.graphs }
    }
}

module *codepoints {
    class *Str is also {
        our multi method codes(Str $string: --> Int) is export { $string.as_codes.elems }
        our multi method chars(Str $string: --> Int) is export { $string.codes }
        multi submethod BUILD(UBuf :@codes) { @!as_codes = @codes; }
        multi method STORE(Str $s -->) {
            @!as_graphs = undef;
            @!as_codes = $s.as_codes;
            @!as_bytes = undef;
            $!cur_nf = $?NF;
        }
        multi method FETCH(--> Str) {
            my Str $s;
            @$s.as_codes = @.as_codes;
            return $s;
        }
        our multi *infix:<~>(Str $s1, Str $s2 --> Str) is export {
            my Str $s;
            $s.as_codes = $s1.as_codes;
            $s.as_codes.push: @$s2.as_codes;
            return $s;
        }
        # S02:737 says code Strs should be in "universal form",
        # so compare as_graphs (we can always upgrade to graphs)
        our multi *infix:<eq>(Str $s1, Str $s2 --> Bool) is export { $s1.as_graphs eqv $s2.as_graphs }
        our multi method ord(Str $string: --> Int) is export { @.as_codes[0] }
        our multi method ord(Str $string: --> List of Int) is export { @.as_codes }
        our method Buf(--> Buf) { @.as_codes }
        our multi method substr(Str $string: StrPos $start, StrLen $length? --> Str) is rw is export {
            die 'Invalid StrPos for this Str' unless $start.s === $string;
            return class {
                method FETCH(--> Str) {
                    my Str $s;
                    @$s.as_codes = @$string.as_codes[ $start.codes .. ($length.codes+$start.codes // *) ];
                    return $s;
                }
                method STORE(Str $s -->) {
                    my Str $s1, $s2, $s3;
                    @$s1.as_codes = @$string.as_codes[ 0 .. $start.codes-1 ];
                    @$s2.as_codes = @$string.as_codes[ ($length.codes+$start.codes+1 // *) .. * ];
                    $s3 = $s1 ~ $s ~ $s2;
                    $string.as_codes = $s3.as_codes;
                }
            };
        }
        our multi method substr(Str $string: StrPos $start, StrPos $end? --> Str) is rw is export {
            die 'Invalid StrPos for this Str' unless $start.s === $string;
            die 'Invalid StrPos for this Str' unless $end.s === $string;
            return class {
                method FETCH(--> Str) {
                    my Str $s;
                    @$s.as_codes = @$string.as_codes[ $start.codes .. ($end.codes // *) ];
                    return $s;
                }
                method STORE(Str $s -->) {
                    my Str $s1, $s2, $s3;
                    @$s1.as_codes = @$string.as_codes[ 0 .. $start.codes-1 ];
                    @$s2.as_codes = @$string.as_codes[ ($end.codes+1 // *) .. * ];
                    $s3 = $s1 ~ $s ~ $s2;
                    $string.as_codes = $s3.as_codes;
                }
            };
        }
    }

        our multi method normalize(Str $string: Str :$nf = $?NF --> Str) is export {
            use :$nf;
            my Str $ret;
            for @$string.as_graphs -> my Int $o {
                $ret ~= Grapheme.new(:id($o)).normalize;
            }
            return $ret;
        }
        # all of the following forms ignore $?NF
        our multi method normalize(Str $string: Bool :$canonical!, Bool :$recompose! --> Str) is export {
            return $string.nfd  if  $canonical and !$recompose;
            return $string.nfc  if  $canonical and  $recompose;
            return $string.nfkd if !$canonical and !$recompose;
            return $string.nfkc if !$canonical and  $recompose;
        }
        our multi method nfd (Str $string: --> Str) is export { return $string.normalize(:nf<d> ) }
        our multi method nfkd(Str $string: --> Str) is export { return $string.normalize(:nf<kd>) }
        our multi method nfc (Str $string: --> Str) is export { return $string.normalize(:nf<c> ) }
        our multi method nfkc(Str $string: --> Str) is export { return $string.normalize(:nf<kc>) }
    }
    our multi sub *chr(Int *@grid --> Str) {
        my UBuf @codes = @grid;
        return Str.new(:@codes);
    }
    class *UBuf is also {
        our multi method Str(UBuf $b: --> Str) { Str.new(:codes($b)) }
    }
    class *AnyChar is also {
        multi submethod BUILD(Str $s) { $.char.as_codes = @$s.as_codes[0]; }
        method STORE(Str $s -->)      { $.char.as_codes = @$s.as_codes[0]; }
        method FETCH(--> Str) {
            my Str $s;
            $s.as_codes = $.char.as_codes;
            return $s;
        }
        method Str(--> Str) {
            my Str $s;
            $s.as_codes = $.char.as_codes;
            return $s;
        }
    }
    class *StrLen is also {
        submethod BUILD(Int $i -->) { $.clear; $.codes = $i; }
        method STORE(Int $i -->)    { $.clear; $.codes = $i; }
        method FETCH(--> Int) { $.codes }
        method Int(--> Int)   { $.codes }
    }
    class *StrPos is also {
        submethod BUILD(Str $s, StrLen $len) {
            $.s = $s;
            my Str $sub;
            @$sub.as_codes = @$s.as_codes[0 .. $len.codes];
            $.sub = $sub;
        }
        method Int(--> Int) { $.codes }
    }
}

module *bytes {
    class *Str is also {
        our multi method bytes(Str $string: --> Int) is export { $string.as_bytes.elems }
        our multi method chars(Str $string: --> Int) is export { $string.bytes }
        multi submethod BUILD(UBuf :@bytes) { @!as_bytes = @bytes; }
        multi method STORE(Str $s -->) {
            @!as_graphs = undef;
            @!as_codes = undef;
            @!as_bytes = $s.as_bytes;
            $!cur_nf = undef;
        }
        multi method FETCH(--> Str) {
            my Str $s;
            @$s.as_bytes = @.as_bytes;
            return $s;
        }
        our multi *infix:<~>(Str $s1, Str $s2 --> Str) is export {
            my Str $s;
            $s.as_bytes = $s1.as_bytes;
            $s.as_bytes.push: @$s2.as_bytes;
            return $s;
        }
        our multi *infix:<eq>(Str $s1, Str $s2 --> Bool) is export { $s1.as_bytes eqv $s2.as_bytes }
        our multi method ord(Str $string: --> Int) is export { @.as_bytes[0] }
        our multi method ord(Str $string: --> List of Int) is export { @.as_bytes }
        our method Buf(--> Buf) { @.as_bytes }
        our multi method substr(Str $string: StrPos $start, StrLen $length? --> Str) is rw is export {...}
        our multi method substr(Str $string: StrPos $start, StrPos $end? --> Str) is rw is export {...}
        our multi method substr(Str $string: StrPos $start, StrLen $length? --> Str) is rw is export {
            die 'Invalid StrPos for this Str' unless $start.s === $string;
            return class {
                method FETCH(--> Str) {
                    my Str $s;
                    @$s.as_bytes = @$string.as_bytes[ $start.bytes .. ($length.bytes+$start.bytes // *) ];
                    return $s;
                }
                method STORE(Str $s -->) {
                    my Str $s1, $s2, $s3;
                    @$s1.as_bytes = @$string.as_bytes[ 0 .. $start.bytes-1 ];
                    @$s2.as_bytes = @$string.as_bytes[ ($length.bytes+$start.bytes+1 // *) .. * ];
                    $s3 = $s1 ~ $s ~ $s2;
                    $string.as_bytes = $s3.as_bytes;
                }
            };
        }
        our multi method substr(Str $string: StrPos $start, StrPos $end? --> Str) is rw is export {
            die 'Invalid StrPos for this Str' unless $start.s === $string;
            die 'Invalid StrPos for this Str' unless $end.s === $string;
            return class {
                method FETCH(--> Str) {
                    my Str $s;
                    @$s.as_bytes = @$string.as_bytes[ $start.bytes .. ($end.bytes // *) ];
                    return $s;
                }
                method STORE(Str $s -->) {
                    my Str $s1, $s2, $s3;
                    @$s1.as_bytes = @$string.as_bytes[ 0 .. $start.bytes-1 ];
                    @$s2.as_bytes = @$string.as_bytes[ ($end.bytes+1 // *) .. * ];
                    $s3 = $s1 ~ $s ~ $s2;
                    $string.as_bytes = $s3.as_bytes;
                }
            };
        }
    }
    }
    our multi sub *chr(Int *@grid --> Str) {
        my UBuf @bytes = @grid;
        return Str.new(:@bytes);
    }
    class *UBuf is also {
        our multi method Str(UBuf $b: --> Str) { Str.new(:bytes($b)) }
    }
    class *AnyChar is also {
        multi submethod BUILD(Str $s) { $.char.as_bytes = @$s.as_bytes[0]; }
        method STORE(Str $s -->)      { $.char.as_bytes = @$s.as_bytes[0]; }
        method FETCH(--> Str) {
            my Str $s;
            $s.as_bytes = $.char.as_bytes;
            return $s;
        }
        method Str(--> Str) {
            my Str $s;
            $s.as_bytes = $.char.as_bytes;
            return $s;
        }
    }
    class *StrLen is also {
        submethod BUILD(Int $i -->) { $.clear; $.bytes = $i; }
        method STORE(Int $i -->)    { $.clear; $.bytes = $i; }
        method FETCH(--> Int) { $.bytes }
        method Int(--> Int)   { $.bytes }
    }
    class *StrPos is also {
        submethod BUILD(Str $s, StrLen $len) {
            $.s = $s;
            my Str $sub;
            @$sub.as_bytes = @$s.as_bytes[0 .. $len.bytes];
            $.sub = $sub;
        }
        method Int(--> Int) { $.bytes }
    }
}

# defaults
use graphemes;
use :nf<c>;
use :encoding<utf8>;
