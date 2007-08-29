class Main {
    say '1..1';

    my $junc = Junction.new;
    $junc.things = [
        1, 2, 4,
    ];
    $junc.type = 'all';

    say '# junction: ', $junc;

    say 'ok 1 - survived so far';
    
}
