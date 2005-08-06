#!/usr/bin/pugs

use v6;
use Test;

plan 20;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 
use Iter::Range;

{
  # normal splice

  my $span = Array::Lazy.new( 1 .. 10 );
  isa_ok( $span, 'Array::Lazy', 'created an Array::Lazy' );

  my $spliced = $span.splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Array::Lazy', 'result is an Array::Lazy' );

  is( $span.stringify, '1,2,23,24,25,6,7,8,9,10', 'span' );
  is( $spliced.stringify, '3,4,5', 'splice' );
}

{
  # splice with negative offset

  my $span = Array::Lazy.new( 1 .. 10 );
  my $spliced = $span.splice( -4, 3, 23..25 );

  is( $span.stringify, '1,2,3,4,5,6,23,24,25,10', 'span' );
  is( $spliced.stringify, '7,8,9', 'splice' );
}

{
  # 'Iter' object
  my $iter = Iter::Range.new( start => 0, end => Inf, step => 1 );
  is( $iter.shift, 0, 'iter 0' );
  is( $iter.shift, 1, 'iter 1' );
  
  my $span = Array::Lazy.new( 'x', 'y', 'z', $iter );
  my $spliced = $span.splice( 1, 4, () );

  is( $span.stringify, 'x,<obj:Iter::Range>', 'span' );
  is( $spliced.stringify, 'y,z,2,3', 'splice' );

  $spliced = $span.splice( 0, 3, ( 'a' ) );

  is( $span.stringify, 'a,<obj:Iter::Range>', 'span' );
  is( $spliced.stringify, 'x,4,5', 'splice again' );
  
  is( $span.shift, 'a', 'shift' );
  is( $span.shift, '6', 'shift' );

  is( $span.pop, Inf, 'pop' );
  is( $span.pop, Inf, 'pop' );
  
  $span.push( "z" );
  is( $span.pop, "z", 'push - pop' );
  is( $span.pop, Inf, 'pop' );

  $span.unshift( "a" );
  is( $span.shift, "a", 'unshift - shift' );
  is( $span.shift, 7, 'shift' );
}

# TODO - test offset == 0, 1, 2, -1, -2, -Inf, Inf
# TODO - test length == 0, 1, 2, Inf
# TODO - test list == (), (1), (1,2), Iterators, ...
# TODO - test with junctions
# TODO - splice an empty array
