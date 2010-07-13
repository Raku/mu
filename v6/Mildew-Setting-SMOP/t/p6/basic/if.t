say "1..4";
if 0 {
    say "not ok 1";
}
if 1 {
    say "ok 1";
} else {
    say "not ok 1";
}

sub a {
    say "ok 2";
    0;
}
sub b {
    say "ok 3";
    1;
}
sub c {
    say "not ok";
    0;
}
sub id($arg) {
   $arg; 
}

if a() {
    say "no ok";
} elsif b() {
    say "ok 4";
} elsif id(c()) {
    say "no ok";
}
