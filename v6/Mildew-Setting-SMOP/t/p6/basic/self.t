say "1..2";
knowhow Foo {
    method ok1($foo) {
        say $foo;
        self.ok2;
    }
    method ok2 {
        say "ok 2";
    }
}
Foo.ok1("ok 1");
