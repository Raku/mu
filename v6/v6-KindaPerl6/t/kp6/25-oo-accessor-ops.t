use v6-alpha;
class Foo {
    has $.from;
    has $.to;
    
    method ok1() {
        say "ok ", self.to - self.from;
    };
}
module Main {
    say '1..2';
    my $foo = Foo.new();
    $foo.from = 1;
    $foo.to = 2;
    $foo.ok1();
    $foo.to = $foo.to + 1;
    $foo.ok1();
}
