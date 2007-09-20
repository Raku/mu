class Main {
    method foo {
        if (0) {
            return 1
        }
    };
    say '1..1';
    my $o = Main.new();
    my $r := $o.foo();
    if ($r) {
        say 'not ok 1';
    } else {
        say 'ok 1';
    }
}