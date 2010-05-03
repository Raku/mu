say "1..2";
if ('foo'.perl eq "'foo'") {
    say "ok 1";
} else {
    say "not ok 1";
}
if ('f\'o\\o'.perl eq "'f\\'o\\\\o'") {
    say "ok 2";
} else {
    say "not ok 2 #",'f\'o\\o'.perl;
}
