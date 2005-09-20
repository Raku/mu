
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
    if eval($v) { say "ok $count - $msg" } else { say "not ok $count - $msg" }
}

sub dies_ok ($c,?$msg) {
    $count++;
    my $r = try { $c(); say "not ok $count - $msg"; 13 };
    say "ok $count - $msg" if $r != 13;
}
