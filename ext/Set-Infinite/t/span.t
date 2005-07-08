#!/usr/bin/pugs

use v6;
use Test;

plan 9;

use_ok( 'Span' );
use Span;   # XXX should not need this

my $span = Span.new( start => 1, end => 3 );

isa_ok( $span, 'Span', 
    'created a Span' );

{
  my $single = Span.new( object => 10 );
  isa_ok( $span, 'Span', 'created a Span with a single element' );
}

is( $span.start, 1, "start" );
is( $span.end  , 3, "end" );

is( $span.start_is_open,   bool::false, "start_is_open" );
is( $span.end_is_open,     bool::false, "end_is_open" );

is( $span.start_is_closed, bool::true, "start_is_closed" );
is( $span.end_is_closed,   bool::true, "end_is_closed" );

# XXX is( $span.size, 2, "real size" );
# XXX is( $span.size( density => 1 ), 3, "integer size" );

my $span2 = Span.new( start => 2, end => 4 );

my $span3 = Span.new( start => 4, end => 6 );

# XXX is( $span.intersects( $span2 ), bool::true, 'intersects' );

# XXX is( $span.intersects( $span3 ), bool::false, 'doesn\'t intersect' );

{
    # XXX my @a = $span.complement;
    # XXX is( @a.elems, 2, 'complement' );
}

# XXX is( $span.intersection( $span2 ).stringify, '[2,3]', 'intersection' );

# XXX is( $span.union( $span2 ).stringify, '[1,4]', 'union' );

