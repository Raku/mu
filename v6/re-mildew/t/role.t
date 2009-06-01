role Foo {
    method bar {
        say "ok 1";
    }
    method baz($ok2) {
        say $ok2;
    }
    has $.attr1;
    has $!hidden;
    method attr2() {
        $!hidden;
    }
}
say "1..4";
Foo.bar;
my $foo = Foo.new;
$foo.baz("ok 2");
$foo.attr1 = "ok 3";
say $foo.attr1;
$foo.attr2 = "ok 4";
say $foo.attr2;
#$foo.baz("ok 2");
