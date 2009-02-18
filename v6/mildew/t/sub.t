$OUT.print("1..3\n");
sub foo($foo) {
    $OUT.print($foo.FETCH);
}
foo("ok 1\n");
foo "ok 2\n";
&foo.("ok 3\n");
