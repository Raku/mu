#!/usr/bin/pugs

use v6;
use Test;

plan 10;

use_ok( 'Set::Functional::Span' );
use Set::Functional::Span;   # XXX should not need this

my $span = Set::Functional::Span.new( 
    start => 1, end => 3, start_is_open => bool::false, end_is_open => bool::false );

isa_ok( $span, 'Set::Functional::Span', 
    'created a Set::Functional::Span' );

is( $span.start, 1, "start" );
is( $span.end  , 3, "end" );

is( $span.start_is_open,   bool::false, "start_is_open" );
is( $span.end_is_open,     bool::false, "end_is_open" );

is( $span.start_is_closed, bool::true, "start_is_closed" );
is( $span.end_is_closed,   bool::true, "end_is_closed" );

is( $span.size, 2, "real size" );
is( $span.size( density => 1 ), 3, "integer size" );

print "ok\n";
