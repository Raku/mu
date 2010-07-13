say "1..2";
my $foo;
my $bar;
$bar := $foo;
$foo = "ok 1";
say $bar;

$foo := "ok 2";
$bar := $foo;
say $bar;
