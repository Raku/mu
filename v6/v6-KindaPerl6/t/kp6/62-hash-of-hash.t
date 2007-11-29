use v6-alpha;
    say "1..2";
    my %hash;
    ( %hash{"test"} ){'x'} = 2;
    if %hash.elems == 1 {
        say "ok 1";
    } else {
        say "not ok 1";
    };
    if ( %hash{"test"} ){'x'} == 2 {
        say "ok 2";
    } else {
        say "not ok 2";
    };
