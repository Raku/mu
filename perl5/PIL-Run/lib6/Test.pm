
sub plan ($n) { say "1..$n"; }

sub ok ($v,?$msg) {
    if $v { say "ok" } else { say "not ok" }
}

sub is ($v,$v2, ?$msg) {
    if $v eq $v2 { say "ok" } else { say "not ok" }
}

sub eval_ok ($v,?$msg) {
    say "not ok - eval_ok isnt implemented yet!";
}
