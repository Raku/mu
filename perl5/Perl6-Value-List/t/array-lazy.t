#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 13;
 
use Perl6::Value::List;

{
  # end of stream
  my $a = Perl6::Value::List->from_single( 1 .. 2 );
  is( $a->shift, 1, 'iter 0' );
  is( $a->shift, 2, 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # end of lazy stream
  my $a = Perl6::Value::List->from_range( start => 1, end => 2, step => 1 );
  is( $a->shift, 1, 'iter 0' );
  is( $a->shift, 2, 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # 'Iter' object
  my $span = Perl6::Value::List->from_range( start => 0, end => Inf, step => 1 );
  is( $span->shift, 0, 'iter 0' );
  is( $span->shift, 1, 'iter 1' );
  
  is( $span->pop, Inf, 'pop' );
  is( $span->pop, Inf, 'pop' );
  
  # reverse
  my $rev = $span->reverse();
  isa_ok( $rev, 'Perl6::Value::List', 'reversed' );
  is( $rev->shift, Inf, 'shift reverse' );
  is( $rev->pop,   2,   'pop reverse' );
}

