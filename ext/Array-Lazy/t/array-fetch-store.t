#!/usr/bin/pugs

use v6;
use Test;

plan 16;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 
use Iter::Range;

{
  # fetch/store
  my $a1 = Perl6::Container::Array.new(
        Perl6::Value::List.from_range( start => 'a', end => Inf, step => undef ) );
  is( $a1.fetch(2), 'c', 'fetch' );
  is( $a1.fetch(2), 'c', 'fetch again' );
  is( $a1.fetch(3), 'd', 'fetch' );
  is( $a1.fetch(1), 'b', 'fetch' );

  $a1.store( 2, 'x' );
  is( $a1.fetch(0), 'a', 'fetch after a store' );
  is( $a1.fetch(1), 'b', 'fetch' );
  is( $a1.fetch(2), 'x', 'fetch' );
  is( $a1.fetch(3), 'd', 'fetch' );

  # store a list
  $a1.store( 2, ( 5,7,9 ) );
  is( $a1.fetch(1), 'b', 'fetch after storing a list' );
  is( $a1.fetch(2), '5 7 9', 'fetch' );
  is( $a1.fetch(3), 'd', 'fetch' );

  # store a lazy list
  $a1.store( 2, Perl6::Value::List.from_range( start => 0, end => Inf ));
  is( $a1.fetch(1), 'b', 'fetch after storing a lazy list' );
  is( $a1.fetch(2), '<obj:Perl6::Value::List>', 'fetch' );
  is( $a1.fetch(3), 'd', 'fetch' );
}

{
  # fetch/store x shift/pop
  my $a1 = Perl6::Container::Array.new(
        Perl6::Value::List.from_range( start => 'a', end => Inf ) );
  # store a lazy list
  $a1.store( 0, Perl6::Value::List.from_range( start => 0, end => Inf ));
  is( $a1.fetch(0), '<obj:Perl6::Value::List>', 'fetch lazy list' );
  is( $a1.shift,    '<obj:Perl6::Value::List>', 'shift lazy list' );
}

{
  # removed -- lazy slice
  # my $a1 = Perl6::Container::Array.new(
  #      Lazy::Range.new( start => 'a', end => Inf, step => undef ) );
  #
  # my $indexes = Lazy::Range.new( start => 2, end => Inf, step => 2 );
  #
  # my $sliced = $a1.fetch_slice( $indexes );
  #
  # is( $sliced.shift,  'c', 'shift from slice' );
  # is( $sliced.shift,  'e', 'shift from slice' );
  # is( $sliced.shift,  'g', 'shift from slice' );
  #
  # '$sliced' is Modifiable
  # $sliced.store( 1, 'x' );
  # is( $sliced.fetch( 1 ),  'x', 'store and fetch from slice' );

}
