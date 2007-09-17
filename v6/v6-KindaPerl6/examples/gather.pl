use v5;

sub gather {
    my $code = shift;
    my @results;
    local $take = sub { 
        print "taking \n";
        push @results, +shift;
    };
    $code->();
    return \@results;
}

sub take {
    $take->( +shift );
}

my $array =
    gather(
        sub { 
            for (1,2,3) { 
                print "gathering $_ \n";
                take( $_ );
            }
        },
    );
    
print "got: ", @$array, "\n";
