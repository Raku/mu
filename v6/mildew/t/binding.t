$OUT.print("1..1\n");
my $foo;
my $bar;
$bar := $foo;
$foo = "ok 1\n";
$OUT.print($bar);
