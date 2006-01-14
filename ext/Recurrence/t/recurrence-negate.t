#!/usr/bin/pugs

use v6;
use Test;

plan 8;

use Recurrence; pass "(dummy instead of broken use_ok)";
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
    my $span1 = Recurrence.new( 
        closure_next =>        sub { $_ >= 0 ?? $_ + 1 !!    0 },
        closure_previous =>    sub { $_ > 0 ??  $_ - 1 !! -Inf },
        complement_next =>     sub { $_ < 1 ??  $_ + 1 !!  Inf },
        complement_previous => sub { $_ < 0 ??  $_ - 1 !!   -1 },
        universe => $universe );
    
    is( $span1.start,   0, "start" );
    is( $span1.end,   Inf, "end" );

    my $neg = $span1.negate;

    is( $neg.start,   -Inf, "start" );
    is( $neg.end,        0, "end" );
    is( $neg.next(-10), -9, "next" );

    my $compl = $neg.complement;

    is( $compl.start,   1, "start" );
    is( $compl.end,   Inf, "end" );
}
