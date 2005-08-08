#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 
use Iter::Range;

{

  my $a1 = Array::Lazy.new(
        Lazy::Range.new( start => 'a', end => Inf, step => undef ) );
  is( $a1.FETCH(2), 'c', 'fetch' );
  is( $a1.FETCH(2), 'c', 'fetch again' );
  is( $a1.FETCH(3), 'd', 'fetch', :todo<bug> );
  is( $a1.FETCH(1), 'b', 'fetch' );

  $a1.STORE( 2, 'x' );
  is( $a1.FETCH(0), 'a', 'fetch after a store' );
  is( $a1.FETCH(1), 'b', 'fetch', :todo<bug> );
  is( $a1.FETCH(2), 'x', 'fetch', :todo<bug> );
  is( $a1.FETCH(3), 'd', 'fetch', :todo<bug> );
}

    # TODO - test against dimension splat in fetch, store
    # get list
    # store list
