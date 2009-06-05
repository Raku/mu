say "1..1\n";
my $foo;
my $bar;
$bar := $foo;
$foo = "ok 1\n";
say $bar;

$foo := 1;
$bar := $foo;
say $bar;
