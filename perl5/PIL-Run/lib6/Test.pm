
sub plan ($n) { say "1..$n"; }

my $count = 0;

sub _hlp (%adv) {
    "";
}

sub ok ($v, ?$msg="", *%adv) {
    $count++;
    if $v { say "ok $count - $msg"~_hlp(%adv) }
    else { say "not ok $count - $msg"~_hlp(%adv) }
}

sub is ($v,$v2, ?$msg="", *%adv) {
    $count++;
    if $v eq $v2 { say "ok $count - $msg"~_hlp(%adv) }
    else { say "not ok $count - $msg"~_hlp(%adv) }
}

sub eval_ok ($v, ?$msg="", *%adv) {
    $count++;
    if eval($v) { say "ok $count - $msg"~_hlp(%adv) }
    else { say "not ok $count - $msg"~_hlp(%adv) }
}

sub eval_is ($v,$v2, ?$msg="", *%adv) {
    $count++;
    if eval($v) eq $v2 { say "ok $count - $msg"~_hlp(%adv) }
    else { say "not ok $count - $msg"~_hlp(%adv) }
}

sub dies_ok ($c, ?$msg="", *%adv) {
    $count++;
    my $r = try { $c(); say "not ok $count - $msg"~_hlp(%adv); 13 };
    say "ok $count - $msg"~_hlp(%adv) if $r != 13;
}

sub pass (?$msg="") {
    $count++;
    say "ok $count - $msg";
}
sub fail (?$msg="") {
    $count++;
    say "not ok $count - $msg";
}
