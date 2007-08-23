class Main {

    say "1..1";
    my sub x { 1 };
    if (x() == 1) {
        say "ok ", x();
    } else {
        say "not ok";
    }

}

