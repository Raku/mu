$LexicalPrelude{'&postfix:++'} := sub ($a is ref) {
    $a = &infix:<+>:(int,int)($a.FETCH,1);
}
$LexicalPrelude{'&infix:~'} := sub ($a,$b) {
    PRIMITIVES::concat($a.FETCH,$b.FETCH);
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
