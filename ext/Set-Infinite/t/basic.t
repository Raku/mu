#!/usr/bin/pugs

use v6;
use Test;

plan 2;

use_ok( 'Set::Functional::Span' );
use Set::Functional::Span;   # XXX should not need this

my $span = Set::Functional::Span.new( 
    start => 1, end => 3, start_is_open => bool::false, end_is_open => bool::false );

isa_ok( $span, 'Set::Functional::Span', 
    'created a Set::Functional::Span' );

print "ok\n";
