say "1..3";
sub infix:<shortcircuit_and>($left_thunk,$right_thunk) {
    my $left = $left_thunk.();
    if ($left) {
        $left;
    } else {
        $right_thunk.();
    }
};
say "ok "~shortcircuit_and(sub {say "ok 1";1},sub {say "ok 2";3});
