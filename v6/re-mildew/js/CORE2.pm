$LexicalPrelude{'&postfix:++'} := sub ($a is ref) {
    $a = &infix:<+>:(int,int)($a.FETCH,1);
}
$LexicalPrelude{'&infix:~'} := sub (|$capture) {
    my $i = 0;
    my $str = '';
    loop {
        if $i.FETCH == $capture.elems {
            return $str.FETCH;
        } else {
           $str = PRIMITIVES::concat($str.FETCH,$capture.positional($i.FETCH).FETCH.Str);
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
$LexicalPrelude{'&infix:ne'} := sub ($a,$b) {
    if PRIMITIVES::eq($a.FETCH,$b.FETCH) {
        ::False;
    } else {
        ::True;
    }
}
$LexicalPrelude{'&infix:eq'} := sub ($a,$b) {
    PRIMITIVES::eq($a.FETCH,$b.FETCH);
}
$LexicalPrelude{'&infix:=='} := sub ($a,$b) {
    PRIMITIVES::eq($a.FETCH,$b.FETCH);
}
$LexicalPrelude{'&infix:!='} := sub ($a,$b) {
    if PRIMITIVES::eq($a.FETCH,$b.FETCH) {
        ::False;
    } else {
        ::True;
    }
}

my sub return(|$capture) {
    my $e = ::ControlExceptionReturn.new();
    $e.capture = $capture;
    $e.routine = CALLER::<&?ROUTINE>;
    $e.throw;
}
$LexicalPrelude{'&return'} := &return;
