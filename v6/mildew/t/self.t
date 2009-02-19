say "1..2";
knowhow Foo {
    method ok1($foo) {
        say "ok 1";
        self.ok2;
    }
    method ok2 {
        say "ok 2";
    }
}
Foo.ok1();
