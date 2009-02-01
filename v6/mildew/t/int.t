$OUT.print("1..4\n");
if 1788.infix:<==>(1788) {
    $OUT.print("ok 1 # infix:<==>\n");
} else {
    $OUT.print("not ok 1 # infix:<==>\n");
}

if 1788.infix:<==>(1787) {
    $OUT.print("not ok 2 # infix:<==>\n");
} else {
    $OUT.print("ok 2 # infix:<==>\n");
}

if 1788.infix:<+>(2).infix:<==>(1790) {
    $OUT.print("ok 3 # infix:<+>\n");
} else {
    $OUT.print("not ok 3 # infix:<+>\n",);
}
if 1600.infix:<->(100).infix:<==>(1500) {
    $OUT.print("ok 4 # infix:<->\n");
} else {
    $OUT.print("not ok 4 # infix:<->\n",);
}
