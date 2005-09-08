#!/usr/bin/pugs

use v6;
use Test;

plan 10;

use_ok( 'Recurrence' );
use Recurrence;   # XXX should not need this

my $universe = Recurrence.new( 
    closure_next =>     sub ( $x is copy ) { return -Inf if $_ == -Inf; Inf if $_ ==  Inf; return $x + 1 },
    closure_previous => sub ( $x is copy ) { return  Inf if $_ ==  Inf; return -Inf if $_ == -Inf; return $x - 1 },
    :is_universe(1) );

isa_ok( $universe, 'Recurrence', 
    'created a Recurrence' );

my $even_numbers = Recurrence.new( 
    closure_next =>     sub { return -Inf if $_ == -Inf; Inf if $_ ==  Inf; return 2 * int( $_ / 2 ) + 2 },
    closure_previous => sub { return  Inf if $_ ==  Inf; return -Inf if $_ == -Inf; return 2 * int( ( $_ - 2 ) / 2 ) },
    universe => $universe );

my $odd_numbers = $even_numbers.complement;

{
    # 0 .. Inf
    my $span1 = Recurrence.new( 
        closure_next =>        sub { $_ >= 0 ?? $_ + 1 !!    0 },
        closure_previous =>    sub { $_ > 0 ??  $_ - 1 !! -Inf },
        complement_next =>     sub { $_ < 1 ??  $_ + 1 !!  Inf },
        complement_previous => sub { $_ < 0 ??  $_ - 1 !!   -1 },
        universe => $universe );
    
    # -Inf .. 10
    my $span3 = Recurrence.new( 
        closure_next =>         sub { $_ < 10 ??  $_ + 1 !!  Inf },
        closure_previous =>     sub { $_ < 11 ??  $_ - 1 !!   10 },
        complement_next =>      sub { $_ >= 10 ?? $_ + 1 !!   11 },
        complement_previous =>  sub { $_ > 11 ??  $_ - 1 !! -Inf },
        universe => $universe );
    
    {
        my $span5 = $span1 ∩ $span3;
        is( $span5.start,  0, "start" );
        is( $span5.end  , 10, "end" );
    }
    {
        my $span5 = $span1 ∖ $span3;
        is( $span5.start,  11, "start" );
        is( $span5.end  , Inf, "end" );
    }
    {
        my $span5 = $span3 ∖ $span1;
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,   -1, "end" );
    }
    {
        my $span5 = $span3 ∪ $span1;
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,  Inf, "end" );
    }
}


