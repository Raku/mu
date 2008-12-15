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
$OUT.print("1..3\n");
$OUT.print(foo().FETCH);
$OUT.print(cond_value(1).FETCH);
$OUT.print(cond_value(0).FETCH);
