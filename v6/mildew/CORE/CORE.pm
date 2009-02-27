sub say(|$capture) {
    my $i = 0;
    loop {
        if &infix:<==>:(int,int)($i.FETCH,$capture.elems) {
            $OUT.print("\n");
            return;
        } else {
           $OUT.print($capture.positional($i.FETCH).FETCH);
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
sub print($arg) {
    $OUT.print($arg.FETCH);
}
$LexicalPrelude.{'&say'} := &say;
$LexicalPrelude.{'&print'} := &print;

$LexicalPrelude.{'&infix:+:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_add($a.FETCH,$b.FETCH);
}

$LexicalPrelude.{'&infix:-:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_substract($a.FETCH,$b.FETCH);
}

$LexicalPrelude.{'&infix:==:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_equal($a.FETCH,$b.FETCH);
}
$LexicalPrelude.{'&infix:~'} := sub (|$capture) {
    my $i = 0;
    my $str = '';
    loop {
        if &infix:<==>:(int,int)($i.FETCH,$capture.elems) {
            return $str.FETCH;
        } else {
           $str = PRIMITIVES::idconst_concat($str.FETCH,$capture.positional($i.FETCH).FETCH.Str);
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
$LexicalPrelude.{'&postfix:++'} := sub ($a) {
    $a = &infix:<+>:(int,int)($a,1);
}

::MildewSOLoader.new.load('Return.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('RoleHOW.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('Multi.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('EXTERNAL.mildew.so',$LexicalPrelude.FETCH);
