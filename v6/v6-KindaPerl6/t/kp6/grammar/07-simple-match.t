grammar Foo {
    token foo {
        foo
    }
}
class Main {
say "1..2";
$_ = 'foo';
if (Foo.foo()) {
    say "ok 1";
} else {
    say "not ok 1";
}
$_ = 'bar';
if (Foo.foo()) {
    say "not ok 2";
} else {
    say "ok 2";
}
}
