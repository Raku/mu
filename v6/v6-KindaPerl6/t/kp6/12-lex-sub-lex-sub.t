class Main {

    say "1..2";
    my sub x { 2; };
    do {
        my sub x { 1; };
        say "ok ", x();
    };
    say "ok ", x();

}

