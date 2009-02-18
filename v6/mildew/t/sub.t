$OUT.print("1..2\n");
sub foo($foo) {
    $OUT.print($foo.FETCH);
}
foo("ok 1\n");
foo "ok 2\n";
