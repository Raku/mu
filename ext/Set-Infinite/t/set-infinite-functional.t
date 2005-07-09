#!/usr/bin/pugs

use v6;
use Test;

plan 12;

use_ok( 'Set::Infinite::Functional' );
use Set::Infinite::Functional;   # XXX should not need this

my $span1 = Span::Functional.new( 
    start => 1, end => 3, start_is_open => bool::false, end_is_open => bool::false );
my $set1 = Set::Infinite::Functional.new( spans => $span1 );

my $span2 = Span::Functional.new( 
    start => 2, end => 4, start_is_open => bool::false, end_is_open => bool::false );
my $set2 = Set::Infinite::Functional.new( spans => $span2 );

my $span3 = Span::Functional.new( 
    start => 4, end => 6, start_is_open => bool::false, end_is_open => bool::false );
my $set3 = Set::Infinite::Functional.new( spans => $span3 );

isa_ok( $set1, 'Set::Infinite::Functional', 
    'created a Set::Infinite::Functional' );

is( $set1.start, 1, "start" );
is( $set1.end  , 3, "end" );

is( $set1.start_is_open,   bool::false, "start_is_open" );
is( $set1.end_is_open,     bool::false, "end_is_open" );

is( $set1.start_is_closed, bool::true, "start_is_closed" );
is( $set1.end_is_closed,   bool::true, "end_is_closed" );

is( $set1.size, 2, "real size" );
# XXX is( $set1.size( density => 1 ), 3, "integer size" );

# XXX is( $set1.intersects( $set2 ), bool::true, 'intersects' );

# XXX is( $set1.intersects( $set3 ), bool::false, 'doesn\'t intersect' );

{
    # XXX my $a = $set1.complement;
    # XXX is( @a.elems, 2, 'complement' );
}

# XXX is( $span.intersection( $span2 ).stringify, '[2,3]', 'intersection' );

is( $set1.union( $set2 ).stringify, '[1,4]', 'union' );
is( $set2.union( $set1 ).stringify, '[1,4]', 'union' );
is( $set1.union( $set3 ).stringify, '[1,3],[4,6]', 'union' );

