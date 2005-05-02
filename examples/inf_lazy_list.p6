use v6;

# See http://perlmonks.org/?node_id=440531 and 441347 for details
# Produces an infinite streams of merged primes that is evaluated lazily

sub lazy_merge (@list is rw) returns Ref {
    my $last = 0;

    my $by_n = sub { my ($n, $k) = (shift(@_), 0); return sub { @_[0] ?? $k += $n :: $k } };
    for @list { $_ = $by_n( $_ ) }

    return sub {
        my $low;
        for ( @list ) {
            my $val = $_();
            $val = $_( 'next' ) if $val <= $last;
            $low = $val if (! defined $low) || ($val < $low);
        }
        return $last = $low;
    };
}

my $end   = @*ARGS[0] // 22;
my @prime = (2, 3, 5);
my $next  = lazy_merge( @prime );
for 1..$end { say $next() };
