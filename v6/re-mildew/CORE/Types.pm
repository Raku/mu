role Any {
    method ACCEPTS() {
        ::True;
    }
}
role int {
    method ACCEPTS($thing) {
        PRIMITIVES::ritest((|$thing),PRIMITIVES::SMOP_RI(2));
    }
}
$LexicalPrelude.{'int'} := ::int;
$LexicalPrelude.{'Any'} := ::Any;

