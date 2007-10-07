class Main {
    say '1..4';

    my $junc = Junction.new(type=>'all',things=>[
        '2', '3', '4',
    ]);

    say 'ok 1 - survived so far';
    
    # $junc.say;

    my sub sayok ( $a ) {
        ( 'ok ' ~ $a ).say;
    };

    sayok( $junc );
}
