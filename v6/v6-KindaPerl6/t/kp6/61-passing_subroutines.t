say "1..3";
sub shortcircuit_and($left_thunk,$right_thunk) {
    my $left = $left_thunk.();
    if ($left) {
        $right_thunk.();
    } else {
        $left;
    }
};
say "ok "~shortcircuit_and(sub {say "ok 1";1},sub {say "ok 2";3});
