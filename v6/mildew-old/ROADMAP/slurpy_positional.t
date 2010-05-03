sub foo($foo, *@bar) {
    $OUT.print(@bar[0].FETCH);
    $OUT.print(@bar[1].FETCH);
    $OUT.print(@bar[2].FETCH);
}

$out.print("1..6\n");

foo("not ok 1\n","ok 1\n","ok 2\n","ok 3\n");

my @values = "ok 4\n","ok 5\n","ok 6\n";

foo("not ok 4\n", @values);