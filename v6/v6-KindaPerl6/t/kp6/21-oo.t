use v6-alpha;
class Foo {
    method ok1() {
        say "ok 1";
    };
    method ok2() {
        say "ok 2";
    };
    method ok() {
        say "ok "~@_[0];
    }
}
module Main {
    say '1..4';
    my $foo = Foo.new();
    $foo.ok1();
    $foo.ok2();
    $foo.ok(3);
    $foo.ok(4);
}
