$OUT.print("1..2\n");
knowhow Foo {
    method ok1($foo) {
        $OUT.print("ok 1\n");
        self.ok2;
    }
    method ok2 {
        $OUT.print("ok 2\n");
    }
}
Foo.ok1();
