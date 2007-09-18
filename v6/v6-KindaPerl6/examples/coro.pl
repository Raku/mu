use strict;

my $sub = do {
    my $state = 'L1';
    my $i;
    
    {
        init => sub { $state = 'START'; },
        is_done => sub { $state eq 'END' },
        run  => sub {
            goto $state;
            START:
            
            $i = 1;
            FOR: {
                print "ok sub $i\n";
                
                $state = 'L2'; return; L2: ;
                
                $i++;
                goto FOR if $i < 3;
            }
            print "done sub\n";
            
            $state = 'END'; END: ; return;
        },
    };
};


my $c = do {
    my $state = 'START';
    my $i;
    
    {
        init => sub { $state = 'START'; },
        is_done => sub { $state eq 'END' },
        run  => sub {
            goto $state;
            START:
            
            $i = 1;
            FOR: {
                print "ok $i\n";
                
                $sub->{init}();
                L1:
                unless ( $sub->{is_done}() ) {
                    $state = 'L1'; return $sub->{run}(); 
                }
                
                $state = 'L2'; return; L2: ;
                
                $i++;
                goto FOR if $i < 3;
            }
            print "done\n";
            
            $state = 'END'; END: ; return;
        },
    };
};


                $c->{init}();
                until ( $c->{is_done}() ) {
                    $c->{run}(); 
                }
