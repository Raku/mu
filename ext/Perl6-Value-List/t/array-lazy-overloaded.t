#!/usr/bin/pugs

use v6;
use Test;

plan 10;
 
use Perl6::Value::List;

multi infix:<..> ( Int $a, Int $b ) { Perl6::Value::List.from_range( start => $a, end => $b, step => 1 ) }

{
  # end of stream
  my $a = 1 .. 2;
  is( $a.shift, 1, 'iter 0' );
  is( $a.shift, 2, 'iter 1' );
  is( $a.shift, undef, 'end' );
}

{
  # 'Iter' object
  my $span = 0 .. Inf;
  is( $span.shift, 0, 'iter 0' );
  is( $span.shift, 1, 'iter 1' );
  
  is( $span.pop, Inf, 'pop' );
  is( $span.pop, Inf, 'pop' );
  
  # reverse
  my $rev = $span.reverse();

  isa_ok( $rev, 'List', 'reversed' );

  is( $rev.shift, Inf, 'shift reverse' );
  is( $rev.pop,   2,   'pop reverse' );
}

