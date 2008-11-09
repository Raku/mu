$OUT.print("1..3\n");

my $hash;
$hash = ::Hash.new;
$hash.{'foo'} = "ok 2\n";
$hash.{'bar'} = "ok 1\n";

my $scalar;
$hash.{'baz'} := $scalar;
$scalar = "ok 3\n";
$OUT.print($hash.{'bar'}.FETCH);
$OUT.print($hash.{'foo'}.FETCH);
$OUT.print($hash.{'baz'}.FETCH);
