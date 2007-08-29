

class Main {
    say '1..6';

    my $junc = all( 1,2,3,4);

    say '# junction: ', $junc.str;
    say '# junction: ', $junc;

    # $junc.say;

    my sub sayok ( $a ) {
        ( 'ok ' ~ $a ).say;
    };

    sayok( $junc );

    if $junc {
        say "ok 5";
    }
    else {
        say "not ok 5";
    };

    if all( 1, 0 ) {
        say "not ok 6";
    }
    else {
        say "ok 6";
    };
}
