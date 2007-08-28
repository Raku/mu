class Main {
    say '1..4';

    my sub ab_1_ ($a) {
        say 'not ok 2';
    };

    my sub ab_3_ ($a,$b,$c) {
        say 'ok 3';
    };

    my sub ab_4_ ($a,$b,$c,$d) {
        say 'ok 4';
    };

    my &multi := Multi.new;
    &multi.long_names = [
        
        sub ($a,$b) {
            say 'ok 2';
        },
        
        &ab_1_,
        &ab_3_,
        &ab_4_,
    ];
    say '# long_names: ', &multi.long_names;

    say 'ok 1 - survived so far';

    say '# Signature: ', &ab_3_.signature;

    say '# flattened Capture:   ', $capture;    
    my $capture = \( 1, 2 );
    multi( |$capture );
    
    say '# Param list:';    
    multi( 42, 43, 44 );
    
    say '# flattened Array:';    
    my @x = [ 1,2,3,4 ];
    multi( |@x );
    
}
