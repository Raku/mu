use v6;
::MildewSOLoader.new.load('Prelude.mildew.so',$LexicalPrelude.FETCH);
sub foo() {
    return "ok 1\n";
}
my $foo = foo();
$OUT.print($foo.FETCH);
