#!/usr/bin/pugs

use v6;
use Test;

plan 53;

use_ok( 'Span' );
use Span;   # XXX should not need this

my $span = Span.new( :start(1), :end(3) );

isa_ok( $span, 'Span', 'created a Span' );

is( $span.is_empty, bool::false, 'is not empty' );
is( $span.stringify, '[1,3]', 'stringify' );

{
    my $a = Span.new();
    isa_ok( $a, 'Span', 'created an empty Span' );
    is( $a.is_empty, bool::true, 'created an empty Span is_empty' );
    is( $a.stringify, '', 'created an empty Span stringify' );
}

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
    my @a = $span.complement;
    is( @a.elems, 2, 'complement' );
    is( @a[0].stringify, '(-Infinity,1)', 'complement' );
    is( @a[1].stringify, '(3,Inf)', 'complement' );
}

{
    my @a = $span.intersection( $span2 );
    is( @a.elems, 1, 'intersection' );
    is( @a[0].stringify, '[2,3]', 'intersection 0' );
}

{
    my @a = $span.union( $span2 );
    is( @a.elems, 1, 'union' );
    is( @a[0].stringify, '[1,4]', 'union 0' );
}

{
    my @a = $span.difference( $span2 );
    is( @a.elems, 1, 'difference span' );
    is( @a[0].stringify, '[1,2)', 'difference 0' );
}

{
    my @a = $span.difference( 2 );
    is( @a.elems, 2, 'difference value' );
    is( @a[0].stringify, '[1,2)', 'difference 0' );
    is( @a[1].stringify, '(2,3]', 'difference 1' );
}

{
    my $span = Span.new( :int, :start(1), :end(3) );
    my @a = $span.difference( 2 );
    is( @a.elems, 2, 'integer difference value' );
    is( @a[0].stringify, '1', 'difference 0' );
    is( @a[1].stringify, '3', 'difference 1' );

    my @a = $span.difference( 3 );
    is( @a.elems, 1, 'integer difference value end' );
    is( @a[0].stringify, '[1,2]', 'difference 0' );
}

if(0)
{
    # XXX - this test should emit a warning - how to test this?
    my $span = Span.new( :start(1), :end(2) );
    my $iter = $span.iterator;
    my $i;
    is( $i = $iter.next, undef, 'iterator continuous' );
}

{
    my $span = Span.new( :int, :start(1), :end(2) );

    {
    my $iter = $span.iterator;
    my $i;
    # say $i while $i = $iter.next;
    is( $i = $iter.next, 1, 'iterator next 0' );
    is( $i = $iter.current, 1, 'iterator currenct' );
    is( $i = $iter.next, 2, 'iterator next 1' );
    is( $i = $iter.next, undef, 'iterator next 2' );
    }
    
    {
    my$iter = $span.iterator;
    my $i;
    # say $i while $i = $iter.previous;
    is( $i = $iter.previous, 2, 'iterator previous 0' );
    is( $i = $iter.previous, 1, 'iterator previous 1' );
    is( $i = $iter.previous, undef, 'iterator previous 2' );
    }
}

{
    my $span = Span.new( :int(1) );

    {
    my $iter = $span.iterator;
    my $i;
    is( $i = $iter.next, undef, 'next undef' );
    }

    {
    my $iter = $span.iterator;
    my $i;
    is( $i = $iter.previous, undef, 'previous undef' );
    }
}
