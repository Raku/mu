use v6-alpha;
module Main {
    say "1..2";
    my %hash;
    %hash{"test"} = 1;
    %hash{"tset"} = 2;
    say "ok "~%hash{"test"};
    say "ok "~%hash{"tset"};
}
