#!/usr/bin/pugs

use v6;
use Test;

plan 55;

use_ok( 'Span::Code' );
use Span::Code;   # XXX should not need this

my $universe = Span::Code.new( 
    closure_next =>     sub ( Int $x is copy ) { return $x + 1 },
    closure_previous => sub ( Int $x is copy ) { return $x - 1 },
    :is_universe(1) );

isa_ok( $universe, 'Span::Code', 
    'created a Span::Code' );

is( $universe.start, -Inf, "start" );
is( $universe.end  ,  Inf, "end" );
is( $universe.stringify, '-Infinity..Inf', "stringify" );
# XXX - is( $universe.universe.stringify, '-Infinity..Inf', "universe accessor" );

is( $universe.start_is_open,   bool::false, "start_is_open" );
is( $universe.end_is_open,     bool::false, "end_is_open" );

is( $universe.start_is_closed, bool::true, "start_is_closed" );
is( $universe.end_is_closed,   bool::true, "end_is_closed" );

is( $universe.next( 10 ), 11, 'next' );
is( $universe.previous( 10 ), 9, 'previous' );

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => 10, end => Inf, :end_is_open(bool::true) );
    is( $continuous.stringify, '[10,Inf)', 'continuous 10-Inf' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '10,11,12..Inf', 'range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '-Infinity..7,8,9', 'range complement' );
}

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => -Inf, end => 10, :start_is_open(bool::true) );
    is( $continuous.stringify, '(-Infinity,10]', 'continuous (-Inf,10]' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '-Infinity..8,9,10', 'range from continuous' );
    
    my $complement = $range.complement;
    is( $complement.stringify, '11,12,13..Inf', 'range complement' );
}

{
    # -- intersection with a continuous span
    use Span::Num;
    my $continuous = Span::Num.new( start => 10, end => 20 );
    is( $continuous.stringify, '[10,20]', 'continuous 10-20' );
    my $range = $universe.intersection( $continuous );
    isa_ok( $range, 'Span::Code', 'range from continuous' );
    is( $range.stringify, '10,11,12..18,19,20', 'range from continuous' );

    $continuous = Span::Num.new( start => 10, end => 20, end_is_open => 1 );
    is( $continuous.stringify, '[10,20)', 'continuous' );
    $range = $universe.intersection( $continuous );
    is( $range.stringify, '10,11,12..17,18,19', 'range from continuous semi' );
    
    my $complement = $range.complement;
    # XXX - universe slice should be written '(-Infinity,Inf)'
    is( $complement.stringify, '-Infinity..Inf', 'range complement' );

    my $complement1 = $complement.intersection( Span::Num.new( start => -Inf, end => 15 ) );
    is( $complement1.stringify, '-Infinity..7,8,9', 'complement 1' );
    $complement1 = $complement.intersection( Span::Num.new( start => 15, end => Inf ) );
    is( $complement1.stringify, '20,21,22..Inf', 'complement 2' );
}
{
    # -- intersection with a discrete span
    use Span::Int;
    my $continuous = Span::Int.new( start => 10, end => 20 );
    is( $continuous.stringify, '[10,20]', 'continuous' );
    my $range = $universe.intersection( $continuous );
    is( $range.stringify, '10,11,12..18,19,20', 'range from discrete' );
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
