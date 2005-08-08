#!/usr/bin/pugs

use v6;
use Test;

plan 41;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 
use Iter::Range;

{
  # normal splice

  my $span = Array::Lazy.new( 1 .. 10 );
  isa_ok( $span, 'Array::Lazy', 'created an Array::Lazy' );

  my $spliced = $span.splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Array::Lazy', 'result is an Array::Lazy' );

  is( $span.items.join(','), '1,2,23,24,25,6,7,8,9,10', 'span' );
  is( $spliced.items.join(','), '3,4,5', 'splice' );
}

{
  # end of stream
  my $a = Array::Lazy.new( 1 .. 2 );
  is( $a.shift, 1, 'iter 0' );
  is( $a.shift, 2, 'iter 1' );
  is( $a.shift, undef, 'end' );
}

{
  # end of lazy stream
  my $iter = Lazy::Range.new( start => 1, end => 2, step => 1 );
  my $a = Array::Lazy.new( $iter );
  is( $a.shift, 1, 'iter 0' );
  is( $a.shift, 2, 'iter 1' );
  is( $a.shift, undef, 'end' );
}

{
  # splice with negative offset

  my $span = Array::Lazy.new( 1 .. 10 );
  my $spliced = $span.splice( -4, 3, 23..25 );

  is( $span.items.join(','), '1,2,3,4,5,6,23,24,25,10', 'span' );
  is( $spliced.items.join(','), '7,8,9', 'splice' );
}

{
  # a lazy list of junctions
  my $iter = Lazy::Range.new( start => 0|1, end => Inf, step => 1 );
  is( $iter.shift, 0|1, 'iter 0|1' );
  is( $iter.shift, 1|2, 'iter 1|2' );
  
  # reversed
  my $rev_iter = $iter.reverse;
  is( $rev_iter.pop, 2|3, 'iter reverse 1' );
  is( $rev_iter.pop, 3|4, 'iter reverse 2' );
}

{
  # 'Iter' object
  my $iter = Lazy::Range.new( start => 0, end => Inf, step => 1 );
  is( $iter.shift, 0, 'iter 0' );
  is( $iter.shift, 1, 'iter 1' );
  
  my $span = Array::Lazy.new( 'x', 'y', 'z', $iter );
  my $spliced = $span.splice( 1, 4, () );

  is( $span.items.join(','), 'x,<obj:Lazy::Range>', 'span' );
  is( $spliced.items.join(','), 'y,z,2,3', 'splice' );

  $spliced = $span.splice( 0, 3, ( 'a' ) );

  is( $span.items.join(','), 'a,<obj:Lazy::Range>', 'span' );
  is( $spliced.items.join(','), 'x,4,5', 'splice again' );
  
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
  
  # reverse
  my $rev = $span.reverse();
  isa_ok( $rev, 'Array::Lazy', 'reversed' );
  is( $rev.shift, Inf, 'shift reverse' );
  is( $rev.pop,   8,   'pop reverse' );
  my $rev_splice = $rev.splice( -2, 1, ('k') );
  is( $rev_splice.pop,   10,   'spliced' );
  is( $rev.pop,   9,   'splice reverse' );
  is( $rev.pop, 'k',   'splice reverse' );
  is( $rev.pop,  11,   'splice reverse' );
}

# TODO - test splice offset == 0, 1, 2, -1, -2, -Inf, Inf
# TODO - test splice length == 0, 1, 2, Inf, negative
# TODO - test splice list == (), (1), (1,2), Iterators, ...
# TODO - splice an empty array
# TODO - test multi-dimensional array
# TODO - test optional splice parameters
