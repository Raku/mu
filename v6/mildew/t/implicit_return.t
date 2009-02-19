my sub foo {
    "ok 1\n";
}
my sub cond_value($arg) {
    if $arg {
        "ok 2\n";
    } else {
        "ok 3\n";
    }
}
say "1..3";
say foo;
say cond_value(1);
say cond_value(0);
