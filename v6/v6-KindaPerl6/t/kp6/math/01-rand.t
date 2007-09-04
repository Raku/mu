use v6-alpha;

module Main {

    say '1..6';

    # ----- srand / rand
    my $seed = 1;

    Math.srand($seed);
    my $run_1_1 = Math.rand;
    my $run_1_2 = Math.rand;

    Math.srand($seed);
    my $run_2_1 = Math.rand;
    my $run_2_2 = Math.rand;

    #say "# 1-1: ", $run_1_1, ", 1-2: ", $run_1_2, ", 2-1: ", $run_2_1, ", 2-2: ", $run_2_2;

    # I know, comparing fractionals on equality is generally
    # consideres stupid. But it seem to work.
    if ($run_1_1 != $run_1_2) {
        say "ok 1 ";
    } else {
        say "not ok 1";
    };

    # check whether seed worked
    if ($run_1_1 == $run_2_1) {
        say "ok 2 ";
    } else {
        say "not ok 2";
    };

    # rand with max size - values should differ even when same seed

    Math.srand($seed);
    my $run_3_1 = Math.rand(5);
    my $run_3_2 = Math.rand(10);

    Math.srand($seed);
    my $run_4_1 = Math.rand(5);
    my $run_4_2 = Math.rand(10);

    #say "# 3-1: ", $run_3_1, ", 3-2: ", $run_3_2, ", 4-1: ", $run_4_1, ", 4-2: ", $run_4_2;

    # following values differ
    if ($run_3_1 != $run_3_2) {
        say "ok 3 ";
    } else {
        say "not ok 3";
    };

    # same seed&max gets same value
    if ($run_3_1 == $run_4_1) {
        say "ok 4 ";
    } else {
        say "not ok 4";
    };

    # same seed, but different max should differ
    if ($run_1_1 != $run_3_1) {
        say "ok 5 ";
    } else {
        say "not ok 5";
    };
    if ($run_1_2 != $run_3_2) {
        say "ok 6 ";
    } else {
        say "not ok 6";
    };

    # Perl6 runtime integration
    #if (Math.yyy != 42) {
    #    print "not ";
    #};
    #say "ok 7";
    #if (Math.zzz) {
    #    say "ok 8";
    #} else {
    #    say "not ok 8";
    #};
}
