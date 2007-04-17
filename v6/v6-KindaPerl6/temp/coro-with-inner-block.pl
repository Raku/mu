#    coro mysub {
#        my $i;
#        $i = 42;
#        print "num: $i \n";
#        # yield
#        $i++;
#        print "num: $i \n";
#        {
#            my $i;
#            $i = 99;
#            print "inner: $i \n";
#            # yield
#            $i++;
#            print "inner: $i \n";
#        }
#        $i++;
#        print "num: $i \n";
#        # return
#    }

our $Sub_mysub = \&mysub;
{
    my $i;
    sub mysub {
            $i = 42;
            print "num: $i \n";
            # yield
            $Sub_mysub = \&mysub_2;
        };
    sub mysub_2 {
            $i++;
            print "num: $i \n";
            mysub_anon(); # enter inner scope
        };
    {
        my $i;
        sub mysub_anon {
            $i = 99;
            print "inner: $i \n";
            # yield
            $Sub_mysub = \&mysub_anon_2;
        }
        sub mysub_anon_2 {
            $i++;
            print "inner: $i \n";
            mysub_3();  # back to outer scope
        }
    }
    sub mysub_3 {
        $i++;
        print "num: $i \n";
        # plain return
        $Sub_mysub = \&mysub;
    }
}

{
    my $CORO2;
    sub myothersub {
        goto $CORO2 if $CORO2;
        {
            print "--- \n";
            # yield
            $CORO2 = 'HEREA';
            return $END;
            HEREA: ;
        };
        print "+++ \n";
        # plain return
        $CORO2 = undef;
        return;
    }
}

for (1..10) {
    $Sub_mysub->(); 
    myothersub();
}
