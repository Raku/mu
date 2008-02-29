use v6-alpha;

class Main {
    method is($a,$b) {
        if $a == $b {
            say "ok 1";
        }
        else {
            say "not ok 1";
        }
    };
    method is7($a) {
        if $a == 7 {
            say "ok 2";
        }
        else {
            say "not ok 2";
        }
    };
    sub is8($a) {
        if $a == 8 {
            say "ok 3";
        }
        else {
            say "not ok 3";
        }
    };
    say "1..3";
    my $main := Main.new();
    $main.is(1,1);
    $main.is7(7);
    is8(8);
}
