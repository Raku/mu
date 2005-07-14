#!/usr/bin/pugs

use v6;
use Test;

plan 37;

use_ok( 'Span::Code' );
use Span::Code;   # XXX should not need this

my $universe = Span::Code.new( 
    closure_next =>     sub { $_ + 1 },
    closure_previous => sub { $_ - 1 },
    :is_universe(1) );

isa_ok( $universe, 'Span::Code', 
    'created a Span::Code' );

is( $universe.start, -Inf, "start" );
is( $universe.end  ,  Inf, "end" );
is( $universe.stringify, '-Infinity..Inf', "stringify" );

is( $universe.start_is_open,   bool::false, "start_is_open" );
is( $universe.end_is_open,     bool::false, "end_is_open" );

is( $universe.start_is_closed, bool::true, "start_is_closed" );
is( $universe.end_is_closed,   bool::true, "end_is_closed" );

is( $universe.next( 10 ), 11, 'next' );
is( $universe.previous( 10 ), 9, 'previous' );

{
    use Span::Num;
    my $continuous = Span::Num.new( start => 10, end => 20 );
    is( $continuous.stringify, '[10,20]', 'continuous' );
    my $range = $universe.intersection( $continuous );
    is( $range.stringify, '10,11,12..18,19,20', 'range from continuous' );
}
{
    my $set = $universe.complement;
    is( $set.start,  undef, "start empty set" );
    is( $set.end  ,  undef, "end" );
    is( $set.stringify, '', "stringify" );
}
{
    my $even_numbers = Span::Code.new( 
        closure_next =>     sub { 2 * int( $_ / 2 ) + 2 },
        closure_previous => sub { 2 * int( ( $_ - 2 ) / 2 ) },
        universe => $universe );
    is( $even_numbers.next( 10 ), 12, 'next even' );
    is( $even_numbers.previous( 10 ), 8, 'previous even' );

    my $odd_numbers = $even_numbers.complement;
    is( $odd_numbers.next( 10 ), 11, 'odd even' );
    is( $odd_numbers.previous( 10 ), 9, 'odd even' );
}
{
    # 0 .. Inf
    my $span1 = Span::Code.new( 
        closure_next =>        sub { $_ >= 0 ?? $_ + 1 ::    0 },
        closure_previous =>    sub { $_ > 0 ??  $_ - 1 :: -Inf },
        complement_next =>     sub { $_ < 1 ??  $_ + 1 ::  Inf },
        complement_previous => sub { $_ < 0 ??  $_ - 1 ::   -1 },
        universe => $universe );
    
    is( $span1.start,    0, "start" );
    is( $span1.end  ,  Inf, "end" );

    # -Inf .. 10
    my $span3 = Span::Code.new( 
        closure_next =>         sub { $_ < 10 ??  $_ + 1 ::  Inf },
        closure_previous =>     sub { $_ < 11 ??  $_ - 1 ::   10 },
        complement_next =>      sub { $_ >= 10 ?? $_ + 1 ::   11 },
        complement_previous =>  sub { $_ > 11 ??  $_ - 1 :: -Inf },
        universe => $universe );
    
    is( $span3.start, -Inf, "start" );
    is( $span3.end  ,   10, "end" );

    is( $span1.intersects( $span3 ), bool::true, 'intersects' );

    {
        my $span2 = $span1.complement;
        is( $span2.start, -Inf, "start" );
        is( $span2.end  ,   -1, "end" );
    }
    {
        my $span4 = $span3.complement;
        is( $span4.start,   11, "start" );
        is( $span4.end  ,  Inf, "end" );
        is( $span4.stringify, '11,12,13..Inf', "stringify" );
    }
    {
        my $span5 = $span1.intersection( $span3 );
        is( $span5.start,  0, "start" );
        is( $span5.end  , 10, "end" );
        is( $span5.stringify, '0,1,2..8,9,10', "stringify" );
    }
    {
        my $span5 = $span1.difference( $span3 );
        is( $span5.start,  11, "start" );
        is( $span5.end  , Inf, "end" );
    }
    {
        my $span5 = $span3.difference( $span1 );
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,   -1, "end" );
    }
    {
        my $span5 = $span3.union( $span1 );
        is( $span5.start, -Inf, "start" );
        is( $span5.end  ,  Inf, "end" );
    }
}


=for later

is( $span.size, 2, "real size" );
# is( $span.size( density => 1 ), 3, "integer size" );

my $span2 = Span::Num.new( 
    start => 2, end => 4, start_is_open => bool::false, end_is_open => bool::false );

my $span3 = Span::Num.new( 
    start => 4, end => 6, start_is_open => bool::false, end_is_open => bool::false );

is( $span.intersects( $span2 ), bool::true, 'intersects' );

is( $span.intersects( $span3 ), bool::false, 'doesn\'t intersect' );

{
    my @a = $span.complement;
    # XXX inconsistent stringification of -Inf
    is( @a[0].stringify ~ ' ' ~ @a[1].stringify, '(-Infinity,1) (3,Inf)', 'complement' );
}

is( $span.intersection( $span2 ).stringify, '[2,3]', 'intersection' );

is( $span.union( $span2 ).stringify, '[1,4]', 'union' );

=cut
