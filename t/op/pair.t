use v6;

say "1..5";

my $pair1 = 'foo' => 'bar';

my $foo = $pair1.key;
my $bar = $pair1.value;

if ($foo eq 'foo') { say "ok 1" } else { say "not ok 1" }
if ($bar eq 'bar') { say "ok 2" } else { say "not ok 2" }

my @pair1 = $pair1.kv;

if (@pair1[0] eq 'foo') { say "ok 3" } else { say "not ok 3" }
if (@pair1[1] eq 'bar') { say "ok 4" } else { say "not ok 4" }

sub quux { 'not quux' }

my $pair2 = quux => "xyzzy";
my $quux = $pair2.key;

if ($quux eq 'quux') { say "ok 5" } else { say "not ok 5 # TODO => lhs quotes" }


