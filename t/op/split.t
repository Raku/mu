use v6;

say "1..11";

my @foo
if (@foo = eval 'split "", "forty-two"') { say "ok 1" } else {
    say "not ok 1 # TODO split" 
}
if (+@foo == 9)     { say "ok 2"  } else { say "not ok 2 # TODO split" }
if (@foo[0] eq "f") { say "ok 3"  } else { say "not ok 3 # TODO split" }
if (@foo[1] eq "o") { say "ok 4"  } else { say "not ok 4 # TODO split" }
if (@foo[2] eq "r") { say "ok 5"  } else { say "not ok 5 # TODO split" }
if (@foo[3] eq "t") { say "ok 6"  } else { say "not ok 6 # TODO split" }
if (@foo[4] eq "y") { say "ok 7"  } else { say "not ok 7 # TODO split" }
if (@foo[5] eq "-") { say "ok 8"  } else { say "not ok 8 # TODO split" }
if (@foo[6] eq "t") { say "ok 9"  } else { say "not ok 9 # TODO split" }
if (@foo[7] eq "w") { say "ok 10" } else { say "not ok 10 # TODO split" }
if (@foo[8] eq "o") { say "ok 11" } else { say "not ok 11 # TODO split" }
