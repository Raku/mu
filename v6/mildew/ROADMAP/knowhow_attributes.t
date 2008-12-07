knowhow Foo {
	has $.bar;
}
$OUT.print("1..2\n");
Foo.bar = "ok 1\n";
$OUT.print(Foo.bar.FETCH);
my $bar = Foo.^clone();
Foo.bar = "not ok 2\n";
$bar.bar = "ok 2\n";
$OUT.print($bar.bar.FETCH);

