#!/usr/bin/pugs

use v6;
use Test;

plan 20;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 
use Iter::Range;

{
  # FETCH/STORE
  my $a1 = Array::Lazy.new(
        Lazy::Range.new( start => 'a', end => Inf, step => undef ) );
  is( $a1.FETCH(2), 'c', 'fetch' );
  is( $a1.FETCH(2), 'c', 'fetch again' );
  is( $a1.FETCH(3), 'd', 'fetch' );
  is( $a1.FETCH(1), 'b', 'fetch' );

  $a1.STORE( 2, 'x' );
  is( $a1.FETCH(0), 'a', 'fetch after a store' );
  is( $a1.FETCH(1), 'b', 'fetch' );
  is( $a1.FETCH(2), 'x', 'fetch' );
  is( $a1.FETCH(3), 'd', 'fetch' );

  # store a list
  $a1.STORE( 2, ( 5,7,9 ) );
  is( $a1.FETCH(1), 'b', 'fetch after storing a list' );
  is( $a1.FETCH(2), '5 7 9', 'fetch' );
  is( $a1.FETCH(3), 'd', 'fetch' );

  # store a lazy list
  $a1.STORE( 2, Lazy::Range.new( start => 0, end => Inf ));
  is( $a1.FETCH(1), 'b', 'fetch after storing a lazy list' );
  is( $a1.FETCH(2), '<obj:Lazy::Range>', 'fetch' );
  is( $a1.FETCH(3), 'd', 'fetch' );
}

{
  # FETCH/STORE x shift/pop
  my $a1 = Array::Lazy.new(
        Lazy::Range.new( start => 'a', end => Inf ) );
  # store a lazy list
  $a1.STORE( 0, Lazy::Range.new( start => 0, end => Inf ));
  is( $a1.FETCH(0), '<obj:Lazy::Range>', 'fetch lazy list' );
  is( $a1.shift,    '<obj:Lazy::Range>', 'shift lazy list' );
}

{
  # lazy slice
  my $a1 = Array::Lazy.new(
        Lazy::Range.new( start => 'a', end => Inf, step => undef ) );

  my $indexes = Lazy::Range.new( start => 2, end => Inf, step => 2 );

  my $sliced = $a1.slice( $indexes );

  is( $sliced.shift,  'c', 'shift from slice' );
  is( $sliced.shift,  'e', 'shift from slice' );
  is( $sliced.shift,  'g', 'shift from slice' );

  # '$sliced' is Modifiable
  $sliced.STORE( 1, 'x' );
  is( $sliced.FETCH( 1 ),  'x', 'STORE and FETCH from slice' );

}
