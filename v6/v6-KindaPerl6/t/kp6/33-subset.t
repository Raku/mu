use v6-alpha;
class Foo {
    has $.x;
}
module Main {
    say '1..4';

    subset StrOk of Str where { $_ eq 'ok' };

    # negative tests
    my $foo = Foo.new();
    if $foo.does( 'Foo' ) {
        say "ok 1"
    }
    else {
        say "not ok 1"
    };
    if $foo.does( 'StrOk' ) {
        say "not ok 2"
    }
    else {
        say "ok 2"
    };
    
    # Str negative tests
    my $foo = "Foo";
    if $foo.does( 'StrOk' ) {
        say "not ok 3"
    }
    else {
        say "ok 3"
    };

    # positive tests
    my $foo = "ok";
    if $foo.does( 'StrOk' ) {
        say "ok 4"
    }
    else {
        say "not ok 4"
    };
}
