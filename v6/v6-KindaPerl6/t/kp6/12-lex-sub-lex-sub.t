class Main {

    say "1..2";
    my sub x { 2; };
    do {
        my sub x { 1; };
        if (x() == 1) {
            say "ok ", x();
        } else {
            say "not ok 1";
        }
    };
    if (x() == 2) {
        say "ok ", x();
    } else {
        say "not ok 2";
    };

}

