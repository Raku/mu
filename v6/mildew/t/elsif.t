say "1..2";

if 0 {
    say "not ok 1 # got inside if";
} elsif 1 {
    say "ok 1 # got inside elsif";
} else {
    say "not ok 1 # got inside else";
}

if 0 {
    say "not ok 2 # got inside if";
} elsif 0 {
    say "not ok 2 # got inside first elsif";
} elsif 1 {
    say "ok 2 # got inside second elsif";
} else {
    say "not ok 2 # got inside else";
}
