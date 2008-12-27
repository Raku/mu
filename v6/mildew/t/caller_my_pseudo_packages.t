my sub bar {
    my $foo = "ok 1\n";
    $OUT.print(MY::<$foo>.FETCH);
    $OUT.print(CALLER::<$foo>.FETCH);
}
my $foo = "ok 2\n";
$OUT.print("1..2\n");
bar();