sub say($arg) {
    $OUT.print($arg.FETCH,"\n");
}
sub print($arg) {
    $OUT.print($arg.FETCH);
}
$LexicalPrelude.{'&say'} := &say;
$LexicalPrelude.{'&print'} := &print;
::MildewSOLoader.new.load('Return.mildew.so',$LexicalPrelude.FETCH);
#::MildewSOLoader.new.load('Multi.mildew.so',$LexicalPrelude.FETCH);
