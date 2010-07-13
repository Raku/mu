say "1..3";
my $baz = "ok 1";
my sub bzzt($boing) {
    if $boing {
        $baz = $boing;
    } else {
        bzzt("ok 2");
    }
}
say $baz;
bzzt(0);
say $baz;
bzzt("ok 3");
say $baz;
