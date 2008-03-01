class Foo {
    has $.attr;
}
class Main {
    say "1..1";
    my $foo := Foo.new(attr=>10);
    if ($foo.attr == 10) {
        say "ok";
    }
    else {
        say "not ok";
    }
}
