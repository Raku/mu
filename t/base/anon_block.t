use v6;

say "1..3";

my $anon_sub = sub { 1 };
if($anon_sub() == 1) { say "ok 1"; } else { say "not ok 1"; }

my $anon_sub_w_arg = sub ($arg) { 1 + $arg };
if($anon_sub_w_arg(3) == 4) { say "ok 2"; } else { say "not ok 2"; }

my $anon_block = { 1 };
if($anon_block() == 1) { say "ok 3"; } else { say "not ok 3"; }