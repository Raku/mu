class Main {
    say '1..3';

    my sub sort_sub ($a, $b) {
           $a <=> $b 
    };

    my @l;
    @l[0] = 2;
    @l[1] = 1;
    @l[2] = 3;

    my @sorted;
    @sorted = @l.sort(&sort_sub);
    say 'ok ' ~ @sorted[0];
    say 'ok ' ~ @sorted[1];
    say 'ok ' ~ @sorted[2];
    
}
