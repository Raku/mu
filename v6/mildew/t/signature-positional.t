knowhow Foo {
    method bar ($positional1, $positional2) {
        $OUT.print($positional1.FETCH); # the .FETCH here is only
        $OUT.print($positional2.FETCH); # because of a $OUT bug
    }
}
$OUT.print("1..4\n");
Foo.bar("ok 1\n", "ok 2\n");
my $baz = sub ($arg) {
    $OUT.print($arg.FETCH);
};
$baz.("ok 3\n");
my $foo = "ok 4\n";
$baz.($foo);
