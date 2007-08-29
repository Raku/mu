

class Main {
    say '1..4';

    my $junc = all( 1,2,3,4);

    say '# junction: ', $junc.str;
    say '# junction: ', $junc;

    # $junc.say;

    my sub sayok ( $a ) {
        ( 'ok ' ~ $a ).say;
    };

    sayok( $junc );
}
