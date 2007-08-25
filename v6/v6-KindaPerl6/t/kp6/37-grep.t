class Main {
    say '1..2';

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
    
}
