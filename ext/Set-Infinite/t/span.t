#!/usr/bin/pugs

use v6;
use Test;

plan 22;

use_ok( 'Span' );
use Span;   # XXX should not need this

my $span = Span.new( :start(1), :end(3) );

isa_ok( $span, 'Span', 'created a Span' );

is( $span.stringify, '[1,3]', 'stringify' );

isa_ok( Span.new( object => 10 ), 'Span', 'created a Span with a single element' );

is( Span.new( object => 10 ).stringify, '10', 'stringify a single element' );
is( Span.new( object => 1 .. 10 ).stringify, '[1,10]', 'created a Span from a range' );

{
    my $copy = $span.clone;
    is( $copy.stringify, '[1,3]', 'clone' );
    $copy.set_start( -5 );
    is( $copy.stringify, '[-5,3]', 'set_start' );
    $copy.set_end( 5 );
    is( $copy.stringify, '[-5,5]', 'set_end' );
}

is( $span.start, 1, "start" );
is( $span.end  , 3, "end" );

is( $span.start_is_open,   bool::false, "start_is_open" );
is( $span.end_is_open,     bool::false, "end_is_open" );

is( $span.start_is_closed, bool::true, "start_is_closed" );
is( $span.end_is_closed,   bool::true, "end_is_closed" );

is( $span.size, 2, "real size" );

# XXX - this should work too
# is( Span.new( :start(1), :end(3), :int ).size, 3, "integer size" );

is( Span.new( :int, :start(1), :end(3) ).size, 3, "integer size" );
is( Span.new( :start(1), :end(3), :int(1) ).size, 3, "integer size" );

my $span2 = Span.new( start => 2, end => 4 );

my $span3 = Span.new( start => 4, end => 6 );

is( $span.intersects( 2 ), bool::true, 'intersects object' );
is( $span.intersects( $span2 ), bool::true, 'intersects span' );
is( $span.intersects( $span3 ), bool::false, 'doesn\'t intersect span' );

is( $span.contains( 2 ), bool::true, 'contains object' );
is( $span.contains( 9 ), bool::false, 'doesn\'t contain object' );

{
    # XXX my @a = $span.complement;
    # XXX is( @a.elems, 2, 'complement' );
}

# XXX is( $span.intersection( $span2 ).stringify, '[2,3]', 'intersection' );

# XXX is( $span.union( $span2 ).stringify, '[1,4]', 'union' );

