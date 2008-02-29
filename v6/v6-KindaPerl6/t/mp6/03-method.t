use v6-alpha;

class Main {
    method ok {
        say "ok 1";
    };
    say "1..1";
    my $main := Main.new();
    $main.ok;
}
