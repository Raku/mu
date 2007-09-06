grammar Foo {
    token foo {
        'f' 'o' 'o'
    }
    token foo2 {
        foo
    }
}
class Main {
say "1..4";
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
$_ = 'foo';
if (Foo.foo2()) {
    say "ok 3";
} else {
    say "not ok 3";
}
$_ = 'bar';
if (Foo.foo2()) {
    say "not ok 4";
} else {
    say "ok 4";
}
}
