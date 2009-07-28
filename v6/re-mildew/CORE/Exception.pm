role Exception {
    method throw() {
        my $interpreter = PRIMITIVES::get_interpreter;
        my $current = $interpreter.continuation;
        loop {
            if ($current.back) {
                $current = $current.back;
                if ($current.catch) {
                    $current.catch.postcircumfix:<( )>(::capture.new(self),:cc($current.back));
                } else {
                }
            } else {
                say "uncaught exception";
                return;
            }
        }
    }
}
$LexicalPrelude{'Exception'} := ::Exception;
