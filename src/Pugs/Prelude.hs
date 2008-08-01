{-# LANGUAGE ForeignFunctionInterface #-}
module Pugs.Prelude where
import Foreign.C.String
import Data.ByteString.Unsafe (unsafePackCStringLen)
import System.IO.Unsafe
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

{-# NOINLINE preludeByteString #-}
preludeByteString :: S.ByteString
preludeByteString = unsafePerformIO $ unsafePackCStringLen (text__prelude_pm, size__prelude_pm)

preludeByteStringLazy :: L.ByteString
preludeByteStringLazy = L.fromChunks [preludeByteString]

{-# NOINLINE testByteString #-}
testByteString :: S.ByteString
testByteString = unsafePerformIO $ unsafePackCStringLen (text__test_pm, size__test_pm)

testByteStringLazy :: L.ByteString
testByteStringLazy = L.fromChunks [testByteString]

foreign import ccall unsafe "text__prelude_pm"
    text__prelude_pm :: CString
foreign import ccall unsafe "size__prelude_pm"
    size__prelude_pm :: Int

foreign import ccall unsafe "text__test_pm"
    text__test_pm :: CString
foreign import ccall unsafe "size__test_pm"
    size__test_pm :: Int

{-
    Prelude bootstap. 

>   The world was young, the mountains green,
>   No stain yet on the Moon was seen,
>   No words were laid on stream or stone,
>   When Durin woke and walked alone.

-}

----------------------------------------------------------------
-- Do not modify this file; it is generated automatically by  --
--                  util/gen_prelude.pl                       --
----------------------------------------------------------------

preludeStr :: String
preludeStr = "use v6-alpha;\n\
\\n\
\module Prelude-0.0.1;\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\class Process {\n\
\    multi sub exec($prog, @args) returns Bool is builtin is primitive is unsafe {\n\
\\n\
\        Pugs::Internals::exec($prog, Bool::False, @args);\n\
\    }\n\
\    multi sub exec(@args) returns Bool is builtin is primitive is unsafe {\n\
\\n\
\        Pugs::Internals::exec(@args[0], Bool::True, @args);\n\
\    }\n\
\    multi sub exec($string) returns Bool is builtin is primitive is unsafe {\n\
\\n\
\\n\
\\n\
\\n\
\        \n\
\        my @args = $string.split;\n\
\        exec(@args);\n\
\    }\n\
\}\n\
\\n\
\class Control::Basic {\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    multi sub eval (Str $code, Str :$lang = 'Perl6') is primitive is safe is builtin {\n\
\        &::(\"Pugs::Internals::eval_\"~lc($lang))($code);\n\
\\n\
\    }\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    multi sub evalfile (Str $filename; Str :$lang = 'Perl6')\n\
\            is primitive is unsafe {\n\
\        &eval(slurp $filename, $lang);\n\
\    }\n\
\}\n\
\\n\
\\n\
\class Control::Caller {\n\
\    has Str $.package;\n\
\    has Str $.file;\n\
\    has Int $.line;\n\
\    has Str $.subname;\n\
\    has Str $.subtype;\n\
\    has Code $.sub;\n\
\    has Str $.params;   # FIXME: needs attention; don't use yet.\n\
\}\n\
\\n\
\multi sub caller (Class $kind = Any, Int :$skip = 0, Str :$label)\n\
\        returns Control::Caller is primitive is builtin is safe {\n\
\    my @caller = Pugs::Internals::caller($kind, $skip, $label);\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    @caller.elems ?? Control::Caller.new(\n\
\        package => @caller[0],\n\
\        file    => @caller[1],\n\
\        line    => @caller[2],\n\
\        subname => @caller[3],\n\
\        subtype => @caller[4],\n\
\        sub     => @caller[5],\n\
\    ) !! undef;\n\
\}\n\
\\n\
\class fatal {\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    \n\
\\n\
\\n\
\\n\
\\n\
\    $fatal::DEFAULT_FATALITY = 1;\n\
\    \n\
\    sub import {\n\
\        Pugs::Internals::install_pragma_value($?CLASS, 1);\n\
\    }\n\
\\n\
\    sub unimport {\n\
\        Pugs::Internals::install_pragma_value($?CLASS, 0);\n\
\    }\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    sub __fail($e = \"failed\") is primitive is builtin is safe {\n\
\        if Pugs::Internals::current_pragma_value($?CLASS) //\n\
\                $fatal::DEFAULT_FATALITY {\n\
\            die $e;\n\
\        } else {\n\
\            $! = $e;\n\
\            return undef; # this is probably the one place we can return\n\
\\n\
\\n\
\        }\n\
\    }\n\
\}\n\
\\n\
\class Carp {\n\
\\n\
\\n\
\    multi sub longmess (; $e = '') returns Str is primitive is safe {\n\
\        my($mess, $i);\n\
\        $mess = \"$e at $?CALLER::POSITION\";\n\
\\n\
\\n\
\\n\
\\n\
\        loop (;;) {\n\
\            my $caller = Control::Caller::caller(skip => $i++) orelse last;\n\
\            $mess ~= \"\\n\\t{$caller.package}::{$caller.subname}() at {$caller.file} line {$caller.line}\";\n\
\        };\n\
\\n\
\        $mess;\n\
\    }\n\
\}\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\class File {\n\
\    my $SEEK_START = 0;\n\
\    my $SEEK_CUR   = 1;\n\
\    my $SEEK_END   = 2;\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    multi sub open (Str $filename, Str :$layer, Bool :$r, Bool :$w, Bool :$rw,\n\
\            Bool :$a) returns IO is primitive is unsafe is builtin {\n\
\        die \"fancy open modes not supported yet\" if $a and ($r or $w or $rw);\n\
\\n\
\        my $mode;\n\
\        $mode = \"a\" if $a;\n\
\        $mode = \"w\" if $w;\n\
\        $mode = \"rw\" if $rw or ($r and $w);\n\
\        $mode //= \"r\";\n\
\\n\
\\n\
\        my $fh = Pugs::Internals::openFile($filename, $mode);\n\
\\n\
\\n\
\        Pugs::Internals::hSetBinaryMode($fh, Bool::True) if\n\
\            $layer ~~ rx:P5/:raw\\b/;\n\
\\n\
\        $fh;\n\
\    }\n\
\\n\
\    multi method seek (Int $position, Int $whence = $File::SEEK_START)\n\
\            returns Bool is primitive is unsafe is builtin {\n\
\        Pugs::Internals::hSeek(self, $position, $whence);\n\
\    }\n\
\}\n\
\\n\
\\n\
\class Pipe {\n\
\\n\
\\n\
\    multi sub open (Str $command, Bool :$r is copy, Bool :$w) returns IO\n\
\            is primitive is unsafe {\n\
\        die \"Pipe::open is unidirectional\" if all($r, $w);\n\
\        $r = Bool::True if none($r, $w);\n\
\        my ($in, $out, $err) =\n\
\            Pugs::Internals::runInteractiveCommand($command);\n\
\        close $err;\n\
\        close  ($r ?? $in !! $out);\n\
\        ($r ?? $out !! $in);\n\
\    }\n\
\\n\
\\n\
\\n\
\    multi sub open2 (Str $command) returns List is primitive is unsafe {\n\
\        my ($in, $out, $err, $pid) =\n\
\            Pugs::Internals::runInteractiveCommand($command);\n\
\        close $err;\n\
\        ($in, $out, $pid);\n\
\    }\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    multi sub open3 (Str $command) returns List is primitive is unsafe {\n\
\        my ($in, $out, $err, $pid) =\n\
\            Pugs::Internals::runInteractiveCommand($command);\n\
\        ($in, $out, $err, $pid);\n\
\    }\n\
\}\n\
\\n\
\\n\
\role Iter {\n\
\    multi sub prefix:<=> () is primitive { self.shift() }\n\
\    \n\
\    method shift   () { ... }\n\
\    method next    () { ... }\n\
\    method current () { ... }\n\
\}\n\
\\n\
\\n\
\class IO does Iter {\n\
\    method shift   () is primitive { self.readline() }\n\
\    method next    () is primitive { self.shift() }\n\
\}\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\class Str does Iter {\n\
\    method shift () is primitive { =open(self) }\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    multi method comb () is primitive is safe {\n\
\        list self ~~ rx:P5:g/\\S+/;\n\
\    }\n\
\\n\
\    multi method comb ($rx) is primitive {\n\
\        given $rx {\n\
\            when Str { list self ~~ rx:P5:g/\\Q$rx\\E/; }\n\
\            when Regex {    # XXX kludge absence of /<$rx>/ above\n\
\                my $str = ~self;\n\
\                gather {\n\
\                    while $str ~~ $rx {\n\
\                        take ~$();\n\
\                        substr($str, 0, $/.to) = \"\";\n\
\                    }\n\
\\n\
\                };\n\
\            }\n\
\        }\n\
\    }\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\    method subst($rule, $replacement) is primitive is safe {\n\
\        my ($rgx,$adv,$dup = self);\n\
\\n\
\        if $rule.isa(Str) {\n\
\          $rgx =  \"<'$rule'>\";\n\
\        }\n\
\        else {\n\
\          $rgx = Pugs::Internals::rule_pattern($rule);\n\
\          $adv = Pugs::Internals::rule_adverbs($rule);\n\
\        }\n\
\\n\
\\n\
\        my $advstr = $adv.map: { \":$_[0]\"~\"($_[1])\"}.join if $adv;\n\
\        my $apply = '()' if $replacement ~~ Code;\n\
\        my $op=\"s$advstr/$rgx/\\$replacement$apply/\";\n\
\\n\
\        eval('$dup ~~ '~$op);\n\
\        $dup\n\
\    }\n\
\\n\
\    method match(Regex $rule) is primitive is safe{\n\
\        self ~~ $rule\n\
\    }\n\
\\n\
\    method trans (Pair *@intable) is primitive {\n\
\\n\
\        my sub expand (Str $string is copy) {\n\
\            my @rv;\n\
\\n\
\            my $idx;\n\
\\n\
\            $string ~~ s:P5:g/\\s+//;\n\
\            my $delim = '!';\n\
\            while index($string,$delim) != -1 {\n\
\                $delim = chr(ord($delim)+1);    # XXX still spoofable\n\
\            }\n\
\            $string = eval \"qb$delim$string$delim\";\n\
\            while (($idx = index($string,'..')) != -1) {\n\
\                my $pre = substr($string,0,$idx-1);\n\
\                my $start = substr($string,$idx-1,1);\n\
\                my $end = substr($string,$idx+2,1);\n\
\\n\
\                push @rv, $pre.split('');\n\
\                push @rv, (~ $start)..(~ $end);\n\
\\n\
\                $string = substr($string,$idx+3);\n\
\            }\n\
\\n\
\            push @rv, $string.split('');\n\
\\n\
\            @rv;\n\
\        }\n\
\\n\
\        my %transtable;\n\
\        for @intable -> Pair $pair {\n\
\            my ($k, $v) = $pair.kv;\n\
\\n\
\            my @ks = $k.isa(Str) ?? expand($k) !! $k.values;\n\
\            my @vs = $v.isa(Str) ?? expand($v) !! $v.values;\n\
\            %transtable{@ks} = @vs;\n\
\        }\n\
\\n\
\        [~] map { %transtable{$_} // $_ }, self.split('');\n\
\    }\n\
\\n\
\    method graphs(Str $s:) returns Int is primitive is safe {\n\
\\n\
\        my Int @combining = (0x0300, 0x0301, 0x0302, 0x0303,\n\
\            0x0304, 0x0305, 0x0306, 0x0307, 0x0308, 0x0309,\n\
\            0x030A, 0x030B, 0x030C, 0x030D, 0x030E, 0x030F,\n\
\            0x0310, 0x0311, 0x0312, 0x0313, 0x0314, 0x0315,\n\
\            0x0316, 0x0317, 0x0318, 0x0319, 0x031A, 0x031B,\n\
\            0x031C, 0x031D, 0x031E, 0x031F, 0x0320, 0x0321,\n\
\            0x0322, 0x0323, 0x0324, 0x0325, 0x0326, 0x0327,\n\
\            0x0328, 0x0329, 0x032A, 0x032B, 0x032C, 0x032D,\n\
\            0x032E, 0x032F, 0x0330, 0x0331, 0x0332, 0x0333,\n\
\            0x0334, 0x0335, 0x0336, 0x0337, 0x0338, 0x0339,\n\
\            0x033A, 0x033B, 0x033C, 0x033D, 0x033E, 0x033F,\n\
\            0x0340, 0x0341, 0x0342, 0x0343, 0x0344, 0x0345,\n\
\            0x0346, 0x0347, 0x0348, 0x0349, 0x034A, 0x034B,\n\
\            0x034C, 0x034D, 0x034E, 0x034F, 0x0350, 0x0351,\n\
\            0x0352, 0x0353, 0x0354, 0x0355, 0x0356, 0x0357,\n\
\            0x0358, 0x0359, 0x035A, 0x035B, 0x035C, 0x035D,\n\
\            0x035E, 0x035F, 0x0360, 0x0361, 0x0362, 0x0363,\n\
\            0x0364, 0x0365, 0x0366, 0x0367, 0x0368, 0x0369,\n\
\            0x036A, 0x036B, 0x036C, 0x036D, 0x036E, 0x036F,\n\
\            0x0483, 0x0484, 0x0485, 0x0486, 0x0488, 0x0489,\n\
\            0x135F, 0x1DC0, 0x1DC1, 0x1DC2, 0x1DC3, 0x20D0,\n\
\            0x20D1, 0x20D2, 0x20D3, 0x20D4, 0x20D5, 0x20D6,\n\
\            0x20D7, 0x20D8, 0x20D9, 0x20DA, 0x20DB, 0x20DC,\n\
\            0x20DD, 0x20DE, 0x20DF, 0x20E0, 0x20E1, 0x20E2,\n\
\            0x20E3, 0x20E4, 0x20E5, 0x20E6, 0x20E7, 0x20E8,\n\
\            0x20E9, 0x20EA, 0x20EB, 0x3099, 0x309A, 0xFE20,\n\
\            0xFE21, 0xFE22, 0xFE23, 0x1D165, 0x1D166, 0x1D167,\n\
\            0x1D168, 0x1D169, 0x1D16D, 0x1D16E, 0x1D16F, 0x1D170,\n\
\            0x1D171, 0x1D172, 0x1D17B, 0x1D17C, 0x1D17D, 0x1D17E,\n\
\            0x1D17F, 0x1D180, 0x1D181, 0x1D182, 0x1D185, 0x1D186,\n\
\            0x1D187, 0x1D188, 0x1D189, 0x1D18A, 0x1D18B, 0x1D1AA,\n\
\            0x1D1AB, 0x1D1AC, 0x1D1AD, 0x1D242, 0x1D243, 0x1D244);\n\
\        my Int $nc = $s.codes;\n\
\        my Int $ng = $nc;\n\
\        loop(my $i = 0; $i < $nc; $i++) {\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\            $ng-- if substr($s, $i, 1).ord == any(@combining)\n\
\                    or substr($s, $i, 2) eq \"\\r\\n\";\n\
\        }\n\
\        $ng;\n\
\    }\n\
\\n\
\    method chars(Str $s:) returns Int is primitive is safe {\n\
\        $s.graphs;\n\
\    }\n\
\\n\
\}\n\
\\n\
\\n\
\sub Pugs::Internals::but_block ($obj, Code $code) is primitive is safe {\n\
\    $code($obj);\n\
\    $obj;\n\
\}\n\
\\n\
\\n\
\class Time::Local {\n\
\    has Int  $.year;    # eg., 2005; \"pre-Gregorian dates are inaccurate\" - GHC\n\
\\n\
\    has Int  $.month;   # 1 to 12 - NOTE 1-based\n\
\    has Int  $.day;     # 1 to 31\n\
\    has Int  $.hour;    # 0 to 23\n\
\    has Int  $.min;     # 0 to 59\n\
\    has Int  $.sec;     # 0 to 61 (up to two leap seconds)\n\
\    has Int  $.picosec;\n\
\    has Int  $.wday;    # 1 to 7, Sunday == 1\n\
\    has Int  $.yday;    # 0 to 365 (up to one leap day)\n\
\    has Str  $.tzname;  # string, eg, JDT\n\
\    has Int  $.tz;      # variation from UTC in seconds\n\
\    has Bool $.is_dst;\n\
\}\n\
\\n\
\multi sub localtime(Num $when = time) returns Time::Local\n\
\        is primitive is builtin is safe {\n\
\    my $res;\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\        my @tm = Pugs::Internals::localtime($when); # Bool::False, $sec, $pico);\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\        $res = Time::Local.new(\n\
\            year    => @tm[0],\n\
\            month   => @tm[1],\n\
\            day     => @tm[2],\n\
\            hour    => @tm[3],\n\
\            min     => @tm[4],\n\
\            sec     => @tm[5],\n\
\            picosec => @tm[6],\n\
\            wday    => @tm[7],\n\
\            yday    => @tm[8],\n\
\            tzname  => @tm[9],\n\
\            tz      => @tm[10],\n\
\            is_dst  => @tm[11],\n\
\        );\n\
\\n\
\    $res;\n\
\}\n\
\\n\
\class Num {\n\
\\n\
\\n\
\\n\
\    multi sub round_gen(Int $n, Code $corner) returns Int is primitive is safe {\n\
\        $n\n\
\    }\n\
\    multi sub round_gen(Num $n, Code $corner) returns Int is primitive is safe {\n\
\        (int($n) == $n) ?? int($n) !! $corner($n);\n\
\    }\n\
\\n\
\    sub do_round($n) is primitive is safe {\n\
\        ($n < 0) ?? int( $n - 0.5) !! int($n + 0.5);\n\
\    }\n\
\    sub do_ceil($n) is primitive is safe {\n\
\        ($n < 0) ?? (-int(-$n)) !! int($n + 1)\n\
\    }\n\
\    sub do_floor($n) is primitive is safe {\n\
\        ($n < 0) ?? (-int(1-$n)) !! int($n)\n\
\    }\n\
\\n\
\\n\
\\n\
\    sub round($n) is primitive is safe is builtin {\n\
\        Num::round_gen($n, &Num::do_round)\n\
\    }\n\
\\n\
\    sub truncate($n) is primitive is safe is builtin { $n.int }\n\
\    sub trunc($n) is primitive is safe is builtin { $n.int }\n\
\\n\
\    sub ceiling($n) is primitive is safe is builtin { Num::round_gen($n, &Num::do_ceil) }\n\
\    sub ceil($n) is primitive is safe is builtin { Num::round_gen($n, &Num::do_ceil) }\n\
\\n\
\    sub floor($n) is primitive is safe is builtin {\n\
\        Num::round_gen($n, &Num::do_floor)\n\
\    }\n\
\}\n\
\\n\
\\n\
\\n\
\sub pi() is primitive is builtin is safe {Math::Basic::pi}\n\
\\n\
\\n\
\sub sprintf ($fmt, *@args) is primitive is builtin is safe {\n\
\    my $flen = $fmt.chars;\n\
\    my $fi = 0;\n\
\    my $ai = 0;\n\
\    my $str = \"\";\n\
\    while ($fi < $flen) {\n\
\\n\
\        my $idx = index($fmt,\"%\",$fi);\n\
\        if $idx < 0 {\n\
\            $str ~= substr($fmt,$fi);\n\
\            last;\n\
\        } else {\n\
\            my $len = $idx - $fi;\n\
\            $str ~= substr($fmt,$fi, $len) if $len > 0;\n\
\            $fi = $idx;\n\
\        }\n\
\\n\
\\n\
\        my $start = $fi;\n\
\        $fi++;\n\
\        while !(substr($fmt,$fi,1)\n\
\                ~~ any(<% c s d u o x e f g X E G b p n i D U O F>)) {\n\
\            $fi++;\n\
\        }\n\
\        my $specifier = substr($fmt,$fi,1); $fi++;\n\
\        my $conversion = substr($fmt,$start,$fi - $start);\n\
\\n\
\\n\
\        my $arg;\n\
\        if $specifier ne '%' {\n\
\            die \"Insufficient arguments to sprintf\" if $ai >= +@args;\n\
\            $arg = @args[$ai];\n\
\            $ai++;\n\
\        }\n\
\\n\
\        given $specifier {\n\
\            when any(<c d u o x i>) {\n\
\                $str ~= Pugs::Internals::sprintf($conversion,int($arg));\n\
\            }\n\
\\n\
\\n\
\\n\
\\n\
\\n\
\            when 'b' {\n\
\                my Bool @num;\n\
\                for (0..~int($arg).bytes*8-1) -> $bit {\n\
\                    push @num, int($arg) +& ( 1 ~ (0 x ($bit))) ?? 1 !! 0;\n\
\                }\n\
\\n\
\                my $converted = int(@num.reverse.join(\"\"));\n\
\\n\
\                $conversion ~~ m:P5/(\\d+)/;\n\
\                my $formatter = ~$0;\n\
\   \n\
\                my $length = int($formatter) - $converted.bytes;\n\
\   \n\
\                my $ret;\n\
\                if ($length < 0) {\n\
\                    $ret = int(@num.reverse.join(\"\"));\n\
\                }\n\
\                else {\n\
\                    given $formatter {\n\
\                        when rx:P5/^0/ {\n\
\                            $ret = (('0' x $length) ~ $converted);\n\
\                        }\n\
\                        default {\n\
\                            $ret = ((' ' x $length) ~ $converted);\n\
\                        }\n\
\                    }\n\
\                }\n\
\                $str ~= $ret;\n\
\\n\
\            }\n\
\            when 's' {\n\
\                $str ~= Pugs::Internals::sprintf($conversion,\"$arg\");\n\
\            }\n\
\            when any(<e f g>) {\n\
\                $str ~= Pugs::Internals::sprintf($conversion,1.0*$arg);\n\
\            }\n\
\            when any(<X D U O>) {\n\
\                $str ~= uc Pugs::Internals::sprintf(lc($conversion),int($arg));\n\
\            }\n\
\            when any(<E G F>) {\n\
\                $str ~= uc Pugs::Internals::sprintf(lc($conversion),1.0*$arg);\n\
\            }\n\
\            when '%' {\n\
\                $str ~= '%';\n\
\            }\n\
\            default {\n\
\                die \"sprintf does not yet implement %{$specifier}\";\n\
\            }\n\
\        }\n\
\    }\n\
\    $str;\n\
\}\n\
\\n\
\multi shift (@array) is builtin is primitive { List::shift(@array) };\n\
\multi shift ($array) is builtin is primitive { die \"Cannot 'shift' scalar\"; };\n\
\\n\
\multi pop (@array) is builtin is primitive { List::pop(@array) };\n\
\multi pop ($array) is builtin is primitive { die \"Cannot 'pop' scalar\"; };\n\
\\n\
\multi fmt ($_; $fmt) is builtin is primitive is safe {\n\
\    when Pair { sprintf($fmt, .kv) }\n\
\    default { sprintf($fmt, $_) }\n\
\}\n\
\\n\
\\n\
\\n\
\\n\
\multi fmt (@obj; $fmt, $comma = ' ') is builtin is primitive is safe {\n\
\    join($comma, map -> $v { sprintf($fmt, $v.isa(Pair) ?? $v.kv !! $v) }, @obj );\n\
\}\n\
\multi fmt (%obj; $fmt, $comma = \"\\n\") is builtin is primitive is safe {\n\
\    join($comma, map -> $k,$v { sprintf($fmt,$k,$v) }, %obj.kv );\n\
\}\n\
\\n\
\sub PIL2JS::Internals::use_jsan_module_imp (*@whatever) {\n\
\    die \"Can't load JSAN modules when not running under PIL2JS!\";\n\
\}\n\
\our &PIL2JS::Internals::use_jsan_module_noimp ::= &PIL2JS::Internals::use_jsan_module_imp;\n\
\\n\
\sub PIL2JS::Internals::use_perl5_module_imp (*@whatever) {\n\
\    die \"Can't load perl5 modules via js when not running under PIL2JS!\";\n\
\}\n\
\our &PIL2JS::Internals::use_perl5_module_noimp ::= &PIL2JS::Internals::use_perl5_module_imp;\n\
\\n\
\\n\
\\n\
\multi prefix_M ($file) is builtin is primitive is unsafe {\n\
\  if $file ~~ :!e {\n\
\    undef;\n\
\  }\n\
\  elsif $file ~~ rx:perl5/[^-_a-zA-Z0-9\\.\\/\\\\\\:]/ {\n\
\    warn \"-M bug: avoided $file\";\n\
\    undef;\n\
\  }\n\
\  else {\n\
\    my $cmd = %?CONFIG<perl5_path>~q{ -e 'print join(\"\\n\", map {-M}, @ARGV,\"\")' }~$file;\n\
\    my $p = Pipe::open($cmd);\n\
\    my $m = slurp($p);  $p.close;\n\
\\n\
\\n\
\    +$m;\n\
\  }\n\
\}\n\
\\n\
\\n\
\sub  *prelude_test_1(){'test 1'}\n\
\sub   prelude_test_2_helper(){'test 2'}\n\
\&*prelude_test_2 ::= &prelude_test_2_helper;\n\
\sub   prelude_test_3_helper(){'test 3'}\n\
\&*prelude_test_3 :=  &prelude_test_3_helper;\n\
\multi prelude_test_4(Str $x) is builtin {'test 4'}\n\
\multi *prelude_test_5(Str $x) {'test 5'}\n\
\\n\
\\n\
\{\n\
\use v6-alpha;\n\
\module Math::Basic;\n\
\\n\
\sub pi() is export(:constants) {3.141592653589793}\n\
\\n\
\};\n\
\BEGIN { %*INC<Math::Basic> = '<precompiled>' };\n\
\\n\
\"

