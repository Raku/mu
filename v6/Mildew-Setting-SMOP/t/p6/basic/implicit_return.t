my sub foo {
    "ok 1";
}
my sub cond_value($arg) {
    if $arg {
        "ok 2";
    } else {
        "ok 3";
    }
}
say "1..3";
say foo;
say cond_value(1);
say cond_value(0);
