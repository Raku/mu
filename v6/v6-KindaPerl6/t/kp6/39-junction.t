class Main {
    say '1..4';

    my $junc = Junction.new;
    $junc.things = [
        '2', '3', '4',
    ];
    $junc.type = 'all';

    say '# junction: ', $junc.str;
    say '# junction: ', $junc;

    say 'ok 1 - survived so far';
    
    # $junc.say;

    my sub sayok ( $a ) {
        ( 'ok ' ~ $a ).say;
    };

    sayok( $junc );
}
