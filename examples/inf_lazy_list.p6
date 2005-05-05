#!/usr/bin/pugs

use v6;

# See http://perlmonks.org/?node_id=440531 and 441347 for details
# Produces an infinite streams of merged primes that is evaluated lazily

# original code (http://www.perlmonks.org/index.pl?node_id=453215)
sub lazy_merge (@list is copy) returns Ref {
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

# further perl6-ification (http://www.perlmonks.org/index.pl?node_id=453402)
sub lazy_merge2 (@list is copy) returns Sub {
    my $last = 0;

    my $by_n = sub ($n) { my $k = 0; return -> $x { $x ?? $k += $n :: $k } };
    @list = @list.map:{ $by_n( $_ ) };

    return sub {
        my $low;
        for ( @list ) -> $sub {
            my $val = $sub();
            $val = $sub(<next>) if $val <= $last;
            $low = $val if !$low.defined || $val < $low;
        }
        return $last = $low;
    };
}

my $end   = @*ARGS[0] // 22;
my @prime = (2, 3, 5);
my $next  = lazy_merge( @prime );
my $next2  = lazy_merge2( @prime );
for 1..$end { say "v1: " ~ $next() ~ "    v2:" ~ $next2() };