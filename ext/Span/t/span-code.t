#!/usr/bin/pugs

use v6;
use Test;

plan 10;

use_ok( 'Span::Code' );
use Span::Code;   # XXX should not need this

my $universe = Span::Code.new( 
    closure_next =>     sub { $^a + 1 },
    closure_previous => sub { $^a - 1 } );

isa_ok( $universe, 'Span::Code', 
    'created a Span::Code' );

is( $universe.start, -Inf, "start" );
is( $universe.end  ,  Inf, "end" );

is( $universe.start_is_open,   bool::false, "start_is_open" );
is( $universe.end_is_open,     bool::false, "end_is_open" );

is( $universe.start_is_closed, bool::true, "start_is_closed" );
is( $universe.end_is_closed,   bool::true, "end_is_closed" );

{
    my $span1 = Span::Code.new( 
        closure_next =>     sub { $^a < 0 ?? 0 :: $^a + 1 },
        closure_previous => sub { $^a - 1 },
        universe => $universe );
    
    is( $span1.start,    0, "start" );
    is( $span1.end  ,  Inf, "end" );

=for later
    my $span2 = $span1.complement;

    is( $span2.start, -Inf, "start" );
    is( $span2.end  ,   -1, "end" );
=cut

}


=for later

is( $span.start, 1, "start" );
is( $span.end  , 3, "end" );

# XXX - doesn't work
# $span.start = 5;
# is( $span.start, 1, "start is read-only" );

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
