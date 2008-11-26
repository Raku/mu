$OUT.print("1..3\n");
my $baz = "ok 1\n";
my sub bzzt($boing) {
    if $boing {
        $baz = $boing;
    } else {
        bzzt("ok 2\n");
    }
}
$OUT.print($baz.FETCH);
bzzt();
$OUT.print($baz.FETCH);
bzzt("ok 3\n");
$OUT.print($baz.FETCH);
