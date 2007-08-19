use v6-alpha;
class Foo {
    method ok1() { 
        1
    };
}
module Main {
    say '1..4';
    my $foo = Foo.new();
    if $foo.isa( 'Foo' ) {
        say "ok 1"
    }
    else {
        say "not ok 1"
    };
    
    $foo = "Foo";
    if $foo.isa( 'Foo' ) {
        say "not ok 2"
    }
    else {
        say "ok 2"
    };
    if $foo.isa( 'Str' ) {
        say "ok 3"
    }
    else {
        say "not ok 3"
    };
    if $foo.isa( 'Object' ) {
        say "ok 4"
    }
    else {
        say "not ok 4 - inheritance"
    };
}
