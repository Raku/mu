class Main {
    say '1..4';

    my @source;
    @source[0] = 2;
    @source[1] = 3;
    @source[2] = 5;
    @source[3] = 7;

    my @mapped;
    @mapped = @source.map(sub { "prim:" ~ $_ });
    if (@mapped[0] ne "prim:" ~ @source[0]) { print "not "; }; say "ok 1";
    if (@mapped[1] ne "prim:" ~ @source[1]) { print "not "; }; say "ok 2";
    if (@mapped[2] ne "prim:" ~ @source[2]) { print "not "; }; say "ok 3";
    if (@mapped[3] ne "prim:" ~ @source[3]) { print "not "; }; say "ok 4";

}
