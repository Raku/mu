use strict;

my $sub = do {
    my $state = 'L1';
    my $i;
    
    sub {
        goto $state;
        L1:
        
        $i = 1;
        FOR: {
            print "ok sub $i\n";
            
            $state = 'L2'; return; L2: ;
            
            $i++;
            goto FOR if $i < 3;
        }
        print "done sub\n";
        
        $state = 'L3'; L3: ; return;
    }
};


my $c = do {
    my $state = 'L1';
    my $i;
    
    sub {
        goto $state;
        L1:
        
        $i = 1;
        FOR: {
            print "ok $i\n";
            
            # TODO sub->reset
            $sub->();
            # TODO until sub->done
            
            $state = 'L2'; return; L2: ;
            
            $i++;
            goto FOR if $i < 3;
        }
        print "done\n";
        
        $state = 'L3'; L3: ; return;
    }
};


$c->() for 1..12;
