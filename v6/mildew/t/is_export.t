module Foo {
    our sub bar is export {
        $OUT.print("ok 1\n");
    }
    our sub baz is export(:DEFAULT) {
        $OUT.print("ok 2\n");
    }
}
$OUT.print("1..2\n");
Foo::EXPORT::ALL::bar();
Foo::EXPORT::DEFAULT::baz();