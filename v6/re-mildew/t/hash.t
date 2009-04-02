say "1..3";

my $hash;
$hash = ::Hash.new;
$hash.{'foo'} = "ok 2\n";
$hash.{'bar'} = "ok 1\n";

my $scalar;
$hash.{'baz'} := $scalar;
$scalar = "ok 3\n";
say $hash.{'bar'};
say $hash.{'foo'};
say $hash.{'baz'};
