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
$_ = "foo";
if (Test.foo()) {
    say "ok 1";
} else {
    say "not ok 1";
}
$_ = "bar";
if (Test.foo()) {
    say "not ok 2";
} else {
    say "ok 2";
}
$_ = "foobar";
if (Test.foobar()) {
    say "ok 3";
} else {
    say "not ok 3";
}
$_ = "barbaz";
if (Test.foobar()) {
    say "not ok 4";
} else {
    say "ok 4";
}
