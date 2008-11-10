$OUT.print("1..2\n");

if 0 {
    $OUT.print("not ok 1 # got inside if\n");
} elsif 1 {
    $OUT.print("ok 1 # got inside elsif\n");
} else {
    $OUT.print("not ok 1 # got inside else\n");
}

if 0 {
    $OUT.print("not ok 2 # got inside if\n");
} elsif 0 {
    $OUT.print("not ok 2 # got inside first elsif\n");
} elsif 1 {
    $OUT.print("ok 2 # got inside second elsif\n");
} else {
    $OUT.print("not ok 2 # got inside else\n");
}
