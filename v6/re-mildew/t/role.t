role Foo {
    method bar {
        say "ok 1";
    }
    method baz($ok2) {
        say $ok2;
    }
}
say "1..2";
Foo.bar;
my $foo = Foo.new;
$foo.baz("ok 2");
#$foo.baz("ok 2");
