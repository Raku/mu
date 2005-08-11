#!/usr/bin/pugs

use v6;
use Test;

plan 5;

use_ok( 'Recurrence' );
use Recurrence;   # XXX should not need this

my $universe = Recurrence.new( 
    closure_next =>     sub ( $x is copy ) { 
        return -Inf if $_ == -Inf; Inf if $_ ==  Inf; return $x + 1 },
    closure_previous => sub ( $x is copy ) { 
        return  Inf if $_ ==  Inf; return -Inf if $_ == -Inf; return $x - 1 },
    :is_universe(1) 
);

{
    # 0 .. Inf
    my $span = Recurrence.new( 
        closure_next =>        sub { $_ >= 0 ?? $_ + 1 ::    0 },
        closure_previous =>    sub { $_ > 0 ??  $_ - 1 :: -Inf },
        complement_next =>     sub { $_ < 1 ??  $_ + 1 ::  Inf },
        complement_previous => sub { $_ < 0 ??  $_ - 1 ::   -1 },
        universe => $universe );

    my $span1 = $span.grep:{ 
        # say "grep ", $^a; 
        $^a != 0 & 5;
    };
    
    is( $span1.start,   1, "start" );
    is( $span1.end,   Inf, "end" );

    my $compl = $span1.complement;

    is( $compl.start, -Inf, "start" );

    # XXX - is this fixable ?
    # is( $compl.end,      5, "end" );

    is( $compl.next(1),  5, "next" );
}
