class Main {
    say "1..4";
    my $foo;
    if ($foo) {
        say "not ok 1";
    } else {
        say "ok 1";
    };
    $foo = undef;
    if ($foo) {
        say "not ok 2";
    } else {
        say "ok 2";
    };
    my %bar;
    if (%bar{'no existent key'}) {
        say "not ok 3";
    } else {
        say "ok 3";
    };
    my @baz;
    if (@baz[666]) {
        say "not ok 4";
    } else {
        say "ok 4";
    };
}
