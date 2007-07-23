use v6-alpha;
class Foo {
    has $.thing;
    
    method ok1() {
        say "ok ", self.thing;
    };
}
module Main {
    say '1..2';
    my $foo = Foo.new();
    $foo.thing = 1;
    $foo.ok1();
    $foo.thing = $foo.thing + 1;
    $foo.ok1();
}
