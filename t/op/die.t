use v6;

say "1..3";

if (eval 'die "foo"; 1') { say "not ok 1" } else { say "ok 1" };
my $error;
eval '$error = $!';  # pugs does not know $! yet
if ($error eq 'foo' ) { say "ok 2 # TODO die" } else { say "not ok 2 # TODO die" }

my $foo = "-foo-";
eval '$foo = die "bar"';
$foo; # this is testing for a bug where an error is stored into $foo in
      # the above eval; unfortunately the if below doesn't detect this on it's
      # own, so this lone $foo will die if the bug is present
if ($foo eq "-foo-") { say "ok 3" } else { say "not ok 3" }
