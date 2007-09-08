grammar Test {
    token foobar {
        <foo>'bar'
    }
    token foo2 {
        <foo><foo>
    }
    token foofoo {
        <foo>foo
    }
    token foo :Perl5 {foo}
}
say "1..4";
if (Test.foo("foo")) {
    say "ok 1";
} else {
    say "not ok 1";
}
if (Test.foo("bar")) {
    say "not ok 2";
} else {
    say "ok 2";
}
if (Test.foobar("foobar")) {
    say "ok 3";
} else {
    say "not ok 3";
}
if (Test.foobar("barbaz")) {
    say "not ok 4";
} else {
    say "ok 4";
}
