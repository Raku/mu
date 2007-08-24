class Main {
    say '1..2';

    my sub ab_2_ ($a,$b) {
        'ab';
    };

    my sub ab_1_ ($a) {
        'a';
    };

    my $multi = Multi.new;
    $multi.long_names = [
        &ab_2_,
        &ab_1_,
    ];
    say '# long_names: ', $multi.long_names;

    my $capture = \( 1, 2 );

    say 'ok 1';

    say '# Signature: ', &ab_2_.signature;
    say '# Capture:   ', $capture;
    
    $multi.APPLY( $capture );
    
}
