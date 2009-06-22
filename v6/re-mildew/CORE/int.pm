role int {
    method ACCEPTS($thing) {
        PRIMITIVES::ritest((|$thing),PRIMITIVES::SMOP_RI(2));
    }
}

multi infix:<==>(int $a,int $b) {
    &infix:<==>:(int,int)($a,$b);
}

multi infix:<!=>(int $a,int $b) {
    if &infix:<==>:(int,int)($a,$b) {
        ::False;
    } else {
        ::True;
    }
}
multi infix:<+>(int $a,int $b) {
    &infix:<+>:(int,int)($a,$b);
}
#TODO fix multi infix:<\<> {...}
multi less(int $a,int $b) {
    &infix:<<<>>:(int,int)($a,$b);
}

$LexicalPrelude.{'int'} := ::int;
$LexicalPrelude.{'&infix:=='} := &infix:<==>;
$LexicalPrelude.{'&infix:!='} := &infix:<!=>;
$LexicalPrelude.{'&infix:+'} := &infix:<+>;
$LexicalPrelude.{'&infix:<'} := &less;
