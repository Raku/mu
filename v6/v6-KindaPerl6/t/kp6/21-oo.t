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
    };
    method twice_ok($first,$second) {
        say "ok "~$first;
        say "ok "~$second;
    }
}
module Main {
    say '1..6';
    my $foo = Foo.new();
    $foo.ok1();
    $foo.ok2();
    $foo.ok(3);
    $foo.ok(4);
    $foo.twice_ok(5,6);
}
