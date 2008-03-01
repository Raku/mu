class Hierachical::Namespace::Foo {
    sub baz {
        say "ok";
    };
}
class Main {
    say "1..2";
    $Hierachical::Namespace::Foo::bar := 8;
    if  $Hierachical::Namespace::Foo::bar == 3 {
        say "ok";
    } else {
        say "not ok";
    };
    Hierachical::Namespace::Foo::baz();
}
