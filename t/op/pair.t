use v6;

say "1..5";

my $pair1 = 'foo' => 'bar';

my $foo; eval '$foo = $pair1.key';
my $bar; eval '$bar = $pair1.value';

if ($foo eq 'foo') { say "ok 1" } else { say "not ok 1 # TODO pair.key" }
if ($bar eq 'bar') { say "ok 2" } else { say "not ok 2 # TODO pair.value" }

my @pair1 = $pair1.kv;

if (@pair1[0] == 'foo') { say "ok 3" } else { say "not ok 3 # TODO pair.kv" }
if (@pair1[1] == 'bar') { say "ok 4" } else { say "not ok 4 # TODO pair.kv" }

my $pair2; eval '$pair2 = quux => "xyzzy"';
my $quux; eval '$quux = $pair2.key';

if ($quux eq 'quux') { say "ok 5" } else { say "not ok 5 # TODO => lhs quotes" }


