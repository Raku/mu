my sub eval($code) {
    my $so = EXTERNAL::eval_perl5('sub {
        use File::Temp qw(tempfile tmpnam);
        my ($fh,$file) = tempfile();
        print $fh $_[0]; 
        my $so = tmpnam;
        system("perl mildew --return-real-value --empty-setting -Cso -o $so $file ");
        $so;
    }')($code);
    ::MildewSOLoader.new.load($so.Str,PRIMITIVES::get_interpreter.continuation.back.lexical);
}
$LexicalPrelude{'&eval'} := &eval;
