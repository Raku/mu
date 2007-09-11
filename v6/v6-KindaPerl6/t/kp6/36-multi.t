class Main {
    say '1..4';

    my sub ab_1_ ($a) {
        say 'not ok 2';
    };

    my sub ab_3_ ($a,$b,$c) {
        say 'ok 3';
    };

    # my &my_multi := Multi.new;
    proto my_multi {};

    &my_multi.long_names = [
        
        sub ($a,$b) {
            say 'ok 2';
        },
        
        &ab_1_,
        &ab_3_,
    ];
    
    # (&my_multi.long_names).push( 
    #    sub ($a,$b,$c,$d) {
    #        say 'ok 4';
    #    }
    # );
    multi my_multi ($a,$b,$c,$d) {
        say 'ok 4';
    }
    
    say '# long_names: ', &my_multi.long_names;

    say 'ok 1 - survived so far';

    say '# Signature: ', &ab_3_.signature;

    say '# flattened Capture:   ', $capture;    
    my $capture = \( 1, 2 );
    my_multi( |$capture );
    
    say '# Param list:';    
    my_multi( 42, 43, 44 );
    
    say '# flattened Array:';    
    my @x = [ 1,2,3,4 ];
    my_multi( |@x );
        
}
