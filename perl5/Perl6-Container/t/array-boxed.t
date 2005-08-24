#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 12;

use Perl6::Container::Array; 
use Perl6::Value;
use Perl6::Value::List;

use constant Inf => Perl6::Value::Num::Inf;

{
  # str()
  my $span = Array->new();
  is( $span->str, '', '... str()' );
  $span->push( 1 );
  is( $span->str, '1', '...' );
  $span->push( 2 );
  is( $span->str, '1, 2', '...' );
  $span->push( 3 );
  is( $span->str, '1, 2, 3', '...' );
  $span->push( 4 );
  is( $span->str, '1, 2, 3, 4', '...' );
  $span->push( List->new( '$.unboxed' => Perl6::Value::List->from_num_range( start => 0, end => Inf, step => 1  ) ) );
  is( $span->str, '1, 2, 3 ... '.&Inf, '...' );
  is( $span->fetch(6), 2, '...' );
}

{
  # normal splice

  my $span = Array->new();
  $span->push( Perl6::Value::List->from_single( 1 .. 10 ) );
  isa_ok( $span, 'Array', 'created an Array' );
  is( $span->perl, '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]', '... perl()' );

  my $spliced = $span->splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Array', '... splice' );

  is( $span->perl, '[1, 2, 23, 24, 25, 6, 7, 8, 9, 10]', '... splice' );
  is( $spliced->perl, '[3, 4, 5]', '... splice' );
}

