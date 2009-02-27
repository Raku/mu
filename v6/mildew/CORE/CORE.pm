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
$LexicalPrelude.{'&infix:~'} := sub ($a,$b) {
    PRIMITIVES::idconst_concat($a.FETCH,$b.FETCH);
}

::MildewSOLoader.new.load('Return.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('RoleHOW.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('Multi.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('EXTERNAL.mildew.so',$LexicalPrelude.FETCH);
