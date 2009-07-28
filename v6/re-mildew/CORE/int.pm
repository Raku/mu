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
multi infix:<->(int $a,int $b) {
    &infix:<->:(int,int)($a,$b);
}
multi prefix:<->(int $a) {
    &infix:<->:(int,int)(0,$a);
}
#TODO fix multi infix:<\<> {...}
multi less(int $a,int $b) {
    &infix:<<<>>:(int,int)($a,$b);
}
multi more(int $a,int $b) {
    if &infix:<<<>>:(int,int)($a,$b) {
        ::False;
    } elsif &infix:<==>:(int,int)($a,$b) {
        ::False;
    } else {
        ::True;
    }
}

$LexicalPrelude{'int'} := ::int;
$LexicalPrelude{'&infix:=='} := &infix:<==>;
$LexicalPrelude{'&infix:!='} := &infix:<!=>;
$LexicalPrelude{'&infix:+'} := &infix:<+>;
$LexicalPrelude{'&infix:-'} := &infix:<->;
$LexicalPrelude{'&prefix:-'} := &prefix:<->;
$LexicalPrelude{'&infix:<'} := &less;
$LexicalPrelude{'&infix:>'} := &more;
