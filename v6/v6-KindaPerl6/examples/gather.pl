use v5;

# TODO - test with nested iterators

use threads;
use strict;
use warnings;

our $take;

sub gather {
    my $code = shift;
    my @results;
    # TODO don't use local(), use a "gather object" instead
    local $take = sub { 
        print "taking \n";
        push @results, +shift;
    };
    # TODO start a thread
    # TODO set a "finished" flag on return
    
    my $thr = threads->new( $code );
    $thr->join;  # fixme
    
    #$code->();  # no threads
    
    # TODO return an iterator
    return \@results;
}

sub take {
    # TODO block until the iterator requires data
    $take->( +shift );
}

my $array =
    gather(
        sub { 
            for (1,2,3) { 
                print "gathering $_ \n";
                # TODO finish cleanly when the iterator is destroyed
                take( $_ );
            }
        },
    );

# TODO get data from the iterator instead    
print "got: ", @$array, "\n";
