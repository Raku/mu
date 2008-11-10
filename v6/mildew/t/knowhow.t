knowhow Foo {
    method bar {
        $OUT.print("ok 1\n");
    }
}
$OUT.print("1..1\n");
Foo.bar;