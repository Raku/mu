use v6-alpha;
module Main {
    say "1..4";
    my %hash;
    if (%hash) {
        say "not ok 1";
    } else {
        say "ok 1";
    };
    %hash{"test"} = 2;
    %hash{"tset"} = 3;
    say "ok "~%hash{"test"};
    say "ok "~%hash{"tset"};
    if %hash.elems == 2 {
        say "ok 4";
    } else {
        say "not ok 3";
    }
}
