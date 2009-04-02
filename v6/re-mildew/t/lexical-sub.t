say "1..3";
my $baz = "ok 1\n";
my sub bzzt($boing) {
    if $boing {
        $baz = $boing;
    } else {
        $baz = "ok 2\n";
    }
}
say $baz;
bzzt();
say $baz;
bzzt("ok 3\n");
say $baz;
