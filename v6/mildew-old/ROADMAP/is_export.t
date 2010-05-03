module Foo {
    our sub bar is export {
        say "ok 1";
    }
    our sub baz is export(:DEFAULT) {
        say "ok 2";
    }
}
say "1..2";
Foo::EXPORT::ALL::bar();
Foo::EXPORT::DEFAULT::baz();