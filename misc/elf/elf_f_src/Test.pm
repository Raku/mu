class GLOBAL {

sub plan ($n) { say "1.."~$n; }
sub force_todo ($n) { say "# Ignoring force_todo("~$n~")"; }

my $count = 0;

sub isa_ok($v, $v2, $msg, $todo) {
    ok(isa($v,$v2),$msg,$todo)
}

sub ok ($v, $msg, $todo) {
    my $st = ""; if $todo.WHAT eq 'Pair' { $st = " # TODO "~$todo.value };
    $count++;
    if $v { say "ok "~$count~" - "~$msg~$st }
    else { say "not ok "~$count~" - "~$msg~$st }
}

sub is ($v,$v2, $msg, $todo) {
    my $st = ""; if $todo.WHAT eq 'Pair' { $st = " # TODO "~$todo.value };
    $count++;
    if $v eq $v2 { say "ok "~$count~" - "~$msg~$st }
    else { say "not ok "~$count~" - "~$msg~$st }
}
sub is_deeply($v,$v2, $msg, $todo) {
    is($v,$v2, $msg, $todo)
}

sub isnt ($v,$v2, $msg, $todo) {
    my $st = ""; if $todo.WHAT eq 'Pair' { $st = " # TODO "~$todo.value };
    $count++;
    if $v ne $v2 { say "ok "~$count~" - "~$msg~$st }
    else { say "not ok "~$count~" - "~$msg~$st }
}

sub eval_ok ($v, $msg, $todo) {
    my $st = ""; if $todo.WHAT eq 'Pair' { $st = " # TODO "~$todo.value };
    $count++;
    if eval($v) { say "ok "~$count~" - "~$msg~$st }
    else { say "not ok "~$count~" - "~$msg~$st }
}

sub eval_is ($v,$v2, $msg, $todo) {
    my $st = ""; if $todo.WHAT eq 'Pair' { $st = " # TODO "~$todo.value };
    $count++;
    if eval($v) eq $v2 { say "ok "~$count~" - "~$msg~$st }
    else { say "not ok "~$count~" - "~$msg~$st }
}

sub dies_ok ($c, $msg, $todo) {
    my $st = ""; if $todo.WHAT eq 'Pair' { $st = " # TODO "~$todo.value };
    $count++;
    # don't have try{} yet
    #my $r = try { $c(); say "not ok "~$count~" - "~$msg~$st; 13 };
    #say "ok "~$count~" - "~$msg~$st if $r != 13;
    say "not ok "~$count~" - "~$msg~$st;
}

sub lives_ok ($c, $msg, $todo) {
    my $st = ""; if $todo.WHAT eq 'Pair' { $st = " # TODO "~$todo.value };
    $count++;
    #my $r = try { $c(); say "ok "~$count~" - "~$msg~$st; 13 };
    #say "not ok "~$count~" - "~$msg~$st if $r != 13;
    say "not ok "~$count~" - "~$msg~$st;
}

sub pass ($msg) {
    $count++;
    say "ok "~$count~" - "~$msg;
}
sub fail ($msg) {
    $count++;
    say "not ok "~$count~" - "~$msg;
}

};
