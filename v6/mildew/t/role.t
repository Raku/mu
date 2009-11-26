say "1..7";
role Foo {
    method bar() {
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
Foo.bar;
my $foo = Foo.new;
$foo.baz("ok 2");
$foo.attr1 = "ok 3";
say $foo.attr1;
$foo.attr2 = "ok 4";
say $foo.attr2;
#$foo.baz("ok 2");

role Src {
    method foo() {
        say "ok 5";
    }
}
role Dst {
    method bar() {
        say "ok 6";
    }
    ::Dst.^compose_role(::Src);
}
my $dst = Dst.new;
$dst.foo;
$dst.bar;

{
    role Conflicting {
        method foo() {
        }
        ::Conflicting.^compose_role(::Src);
        CATCH {
            say "ok 7 # a conflict";
        }
    }
}
