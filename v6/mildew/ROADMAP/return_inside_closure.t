my sub foo($code) {
   $code();
}
my sub bar {
   foo({ return "ok 1\n"; });
   return "not ok 1\n";
}
$OUT.print("1..1\n");
my $a = bar;
$OUT.print($a.FETCH);
