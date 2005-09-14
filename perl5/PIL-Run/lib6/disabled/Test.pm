
sub plan ($n) { say "1..$n"; }

my $count = 0;

sub ok ($v,?$msg) {
    $count++;
    if $v { say "ok $count - $msg" } else { say "not ok $count - $msg" }
}

sub is ($v,$v2, ?$msg) {
    $count++;
    if $v eq $v2 { say "ok $count - $msg" } else { say "not ok $count - $msg" }
}

sub eval_ok ($v,?$msg) {
    $count++;
    say "not ok $count - $msg - eval_ok isnt implemented yet!";
}
