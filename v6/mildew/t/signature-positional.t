knowhow Foo {
    method bar ($positional1, $positional2) {
        $OUT.print($positional1);
        $OUT.print($positional2);
    }
}
$OUT.print("1..2\n");
Foo.bar("ok 1\n", "ok 2\n");