sub say($arg) {
    $OUT.print($arg.FETCH,"\n");
}
sub print($arg) {
    $OUT.print($arg.FETCH);
}
$LexicalPrelude.{'&say'} := &say;
$LexicalPrelude.{'&print'} := &print;
$LexicalPrelude.{'&infix:+:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_add($a.FETCH,$b.FETCH);
}
::MildewSOLoader.new.load('Return.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('RoleHOW.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('Multi.mildew.so',$LexicalPrelude.FETCH);
