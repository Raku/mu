
sub plan ($n) { say "1..$n"; }

my $count = 0;

sub ok ($v, $msg="", $todo?) {
    my $st = ref($todo) eq 'Pair' ?? " # TODO "~$todo.value !! "";
    $count++;
    if $v { say "ok $count - $msg"~$st }
    else { say "not ok $count - $msg"~$st }
}

sub is ($v,$v2, $msg="", $todo?) {
    my $st = ref($todo) eq 'Pair' ?? " # TODO "~$todo.value !! "";
    $count++;
    if $v eq $v2 { say "ok $count - $msg"~$st }
    else { say "not ok $count - $msg"~$st }
}

sub isnt ($v,$v2, $msg="", $todo?) {
    my $st = ref($todo) eq 'Pair' ?? " # TODO "~$todo.value !! "";
    $count++;
    if $v ne $v2 { say "ok $count - $msg"~$st }
    else { say "not ok $count - $msg"~$st }
}

sub dies_ok ($c, $msg="", $todo?) {
    my $st = ref($todo) eq 'Pair' ?? " # TODO "~$todo.value !! "";
    $count++;
    my $r = try { $c(); say "not ok $count - $msg"~$st; 13 };
    say "ok $count - $msg"~$st if $r != 13;
}

sub lives_ok ($c, $msg="", $todo?) {
    my $st = ref($todo) eq 'Pair' ?? " # TODO "~$todo.value !! "";
    $count++;
    my $r = try { $c(); say "ok $count - $msg"~$st; 13 };
    say "not ok $count - $msg"~$st if $r != 13;
}

sub pass ($msg="") {
    $count++;
    say "ok $count - $msg";
}
sub fail ($msg="") {
    $count++;
    say "not ok $count - $msg";
}
