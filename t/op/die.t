use v6;

say "1..2";

if (eval 'die "foo"; 1') { say "not ok 1" } else { say "ok 1" };
my $error;
eval '$error = $!';  # pugs does not know $! yet
if ($error eq 'foo' ) { say "ok 2" } else { say "not ok 2 # TODO die" }
