#!/usr/bin/pugs

use v6;
use Test;

plan 44;

use_ok( 'Set::Infinite' );
use Set::Infinite;   # XXX should not need this

my $span1 = Span::Num.new( 
    start => 1, end => 3, start_is_open => bool::false, end_is_open => bool::false );
my $set1 = Set::Infinite.new( spans => $span1 );

isa_ok( $set1, 'Set::Infinite', 
    'created a Set::Infinite' );

is( $set1.spans.[0].stringify, '[1,3]', 'spans' );

my $span2 = Span::Num.new( 
    start => 2, end => 4, start_is_open => bool::false, end_is_open => bool::false );
my $set2 = Set::Infinite.new( spans => $span2 );

my $span3 = Span::Num.new( 
    start => 4, end => 6, start_is_open => bool::false, end_is_open => bool::false );
my $set3 = Set::Infinite.new( spans => $span3 );

is( Set::Infinite.empty_set.stringify, '', 'empty set' );
is( Set::Infinite.universal_set.stringify, '(-Infinity,Inf)', 'universal set' );

is( Set::Infinite.empty_set.is_empty, bool::true, 'is empty' );
is( $set1.is_empty, bool::false, 'is not empty' );

is( $set1.start, 1, "start" );
is( $set1.end  , 3, "end" );

is( $set1.start_is_open,   bool::false, "start_is_open" );
is( $set1.end_is_open,     bool::false, "end_is_open" );

is( $set1.start_is_closed, bool::true, "start_is_closed" );
is( $set1.end_is_closed,   bool::true, "end_is_closed" );

is( $set1.size, 2, "real size" );
# XXX is( $set1.size( density => 1 ), 3, "integer size" );

is( $set1.intersects( $set2 ), bool::true, 'intersects' );

is( $set1.intersects( $set3 ), bool::false, "doesn't intersect" );

is( $set1.intersection( $set2 ).stringify, '[2,3]', 'intersection' );

is( $set1.union( $set2 ).stringify, '[1,4]', 'union' );
is( $set2.union( $set1 ).stringify, '[1,4]', 'union' );
is( $set1.union( $set3 ).stringify, '[1,3],[4,6]', 'union' );

is( $set1.complement.stringify, '(-Infinity,1),(3,Inf)', 'complement' );
is( $set1.union( $set3 ).complement.stringify, '(-Infinity,1),(3,4),(6,Inf)', 'complement of union' );
is( Set::Infinite.empty_set.complement.stringify, '(-Infinity,Inf)', 'complement of empty set' );
is( Set::Infinite.universal_set.complement.stringify, '', 'complement of universal set' );
is( Set::Infinite.empty_set.complement.complement.stringify, '', 'complement of complement' );

is( $set1.difference( $set2 ).stringify, '[1,2)', 'difference' );

{
    # from synopsis and examples
    my $set = Set::Infinite.new( objects => ( 1, 3, 9 ) );
    is( $set.stringify, '1,3,9', 'from scalars' );
}

{
    # iterator
    my $span = Span.new( :int, :start(1), :end(2) );
    my $set = Set::Infinite.new( objects => ( 2, 7, 9, $span ) );
    is( $set.stringify, '[1,2],7,9', 'from scalars and span' );

    # next / previous($x)
    is( $set.next( 5 ), 7, 'next' );
    is( $set.previous( 5 ), 2, 'previous' );
    
    my $iter = $set.iterator;
    isa_ok( $iter, 'Set::Infinite::Iterator' );
    my $i;
    # say $i while $i = $iter.next;
    is( $i = $iter.next, 1, 'iterator next 0' );
    is( $i = $iter.current, 1, 'iterator current' );
    $iter.reset;
    is( $i = $iter.next, 1, 'iterator reset next 0' );
    is( $i = $iter.next, 2, 'iterator next 1' );
    is( $i = $iter.next, 7, 'iterator next 2' );
    is( $i = $iter.next, 9, 'iterator next 3' );
    is( $i = $iter.next, undef, 'iterator next 4' );
    $iter.reset;
    is( $i = $iter.previous, 9, 'iterator previous 1' );
    is( $i = $iter.previous, 7, 'iterator previous 2' );
    is( $i = $iter.previous, 2, 'iterator previous 3' );
    is( $i = $iter.previous, 1, 'iterator previous 4' );
    is( $i = $iter.previous, undef, 'iterator previous 5' );

    {
        my $i;
        my $a;
        my @a;
        push @a, $a while $a = $set.lazy;
        is( ~@a, '1 2 7 9', 'lazy iterator' );
    }

    # XXX - fix me
    # {
    #    my @a = $span.lazy;
    #    is( @a, "xxx", "lazy array" );
    # }

}
