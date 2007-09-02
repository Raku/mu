class Main {
    say '1..4';

    my @source;
    @source[0] = 2;
    @source[1] = 3;
    @source[2] = 5;
    @source[3] = 7;

    for @source -> $prime {
        say "ok";
    }

}
