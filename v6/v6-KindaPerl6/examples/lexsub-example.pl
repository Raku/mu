class X { 
    my  $a;
        sub xxx { 123 };  # our
    our sub xxx { 123 };  
    my  sub xxx { 123 };  
    my $a := sub { 123 };

    do {
        my  $a;
            sub xxx { 123 };  
        our sub xxx { 123 };  
        my  sub xxx { 123 };  
        my $a := sub { 123 };
    }

}
