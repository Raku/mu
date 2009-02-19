say "1..4";
if 1788.infix:<==>(1788) {
    say "ok 1 # infix:<==>";
} else {
    say "not ok 1 # infix:<==>";
}

if 1788.infix:<==>(1787) {
    say "not ok 2 # infix:<==>";
} else {
    say "ok 2 # infix:<==>";
}

if 1788.infix:<+>(2).infix:<==>(1790) {
    say "ok 3 # infix:<+>";
} else {
    say "not ok 3 # infix:<+>\n",;
}
if 1600.infix:<->(100).infix:<==>(1500) {
    say "ok 4 # infix:<->";
} else {
    say "not ok 4 # infix:<->\n",;
}
