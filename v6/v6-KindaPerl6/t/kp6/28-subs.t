class Main {
    my sub is ($a,$b) {
        if ($a == $b) {
            say "ok";
        } else {
            say "not ok # "~$a~" != "~$b;;
        }
    };
    sub gets_arg($arg) {
        if ($arg == 7) {
            say "ok";
        } else {
            say "not ok #$arg != 7 $arg="~$arg;
        }
    };
    my sub f($nr) {
        is($nr,1);
    };
    sub h($nr) {
        is($nr,2);
    };
    our sub g($nr) {
        is($nr,3);
    };
    say "1..4";
    gets_arg(7);
    f(1);
    h(2);
    g(3);
}
