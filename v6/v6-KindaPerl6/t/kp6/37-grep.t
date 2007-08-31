class Main {
    say '1..4';

    sub grep_sub ($a) {
        $a == 1;
    };

    my @l;
    @l[0] = 2;
    @l[1] = 1;

    my @grepped;
    @grepped = @l.grep(&grep_sub);
    say 'ok ' ~ @grepped[0];
    say 'ok ' ~ @l[0];
    # 2 elements in @l ...
    if (@l[1]) {
        say "ok 3";
    } else {
        say "not ok 3";
    };
    # ... but not 2 elements in @grepped
    if (@grepped[1]) {
        print "not ";
    };
    say "ok 4";
}
