#!/usr/bin/pugs

use v6;
use Test;

plan 14;

use_ok( 'Span::Num' );
use Span::Num;   # XXX should not need this

my $span = Span::Num.new( 
    start => 1, end => 3, start_is_open => bool::false, end_is_open => bool::false );

isa_ok( $span, 'Span::Num', 
    'created a Span::Num' );

is( $span.start, 1, "start" );
is( $span.end  , 3, "end" );

# XXX - doesn't work
# $span.start = 5;
# is( $span.start, 1, "start is read-only" );

is( $span.start_is_open,   bool::false, "start_is_open" );
is( $span.end_is_open,     bool::false, "end_is_open" );

is( $span.start_is_closed, bool::true, "start_is_closed" );
is( $span.end_is_closed,   bool::true, "end_is_closed" );

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

