#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 13;

# use_ok( 'Pugs::Runtime::Container::Array' );
use Pugs::Runtime::Container::Array; 
use Pugs::Runtime::Value;
use Pugs::Runtime::Value::List;

use constant Inf => Pugs::Runtime::Value::Num::Inf;

{
  # fetch/store
  my $a1 = Pugs::Runtime::Container::Array->from_list(
        Pugs::Runtime::Value::List->from_range( start => 'a', end => Inf, step => undef ) );
  is( $a1->fetch(2), 'c', 'fetch' );
  is( $a1->fetch(2), 'c', 'fetch again' );
  is( $a1->fetch(3), 'd', 'fetch' );
  is( $a1->fetch(1), 'b', 'fetch' );

  $a1->store( 2, 'x' );
  is( $a1->fetch(0), 'a', 'fetch after a store' );
  is( $a1->fetch(1), 'b', 'fetch' );
  is( $a1->fetch(2), 'x', 'fetch' );
  is( $a1->fetch(3), 'd', 'fetch' );

  # store a list
  $a1->store( 2, [ 5,7,9 ] );
  is( $a1->fetch(1), 'b', 'fetch after storing a list' );
  is_deeply( $a1->fetch(2), [5, 7, 9], 'fetch' );
  is( $a1->fetch(3), 'd', 'fetch' );

  # store a lazy list
  $a1->store( 2, Pugs::Runtime::Value::List->from_range( start => 0, end => Inf ));
  is( $a1->fetch(1), 'b', 'fetch after storing a lazy list' );

  # not portable
  # is( $a1->fetch(2), '<obj:Pugs::Runtime::Value::List>', 'fetch' );

  is( $a1->fetch(3), 'd', 'fetch' );
}

{
  # fetch/store x shift/pop
  my $a1 = Pugs::Runtime::Container::Array->from_list(
        Pugs::Runtime::Value::List->from_range( start => 'a', end => Inf ) );
  # store a lazy list
  $a1->store( 0, Pugs::Runtime::Value::List->from_range( start => 0, end => Inf ));

  # not portable
  # is( $a1->fetch(0), '<obj:Pugs::Runtime::Value::List>', 'fetch lazy list' );
  # is( $a1->shift,    '<obj:Pugs::Runtime::Value::List>', 'shift lazy list' );
}

{
  # removed -- lazy slice
  # my $a1 = Pugs::Runtime::Container::Array->from_list(
  #      Lazy::Range->from_list( start => 'a', end => Inf, step => undef ) );
  #
  # my $indexes = Lazy::Range->from_list( start => 2, end => Inf, step => 2 );
  #
  # my $sliced = $a1->fetch_slice( $indexes );
  #
  # is( $sliced->shift,  'c', 'shift from slice' );
  # is( $sliced->shift,  'e', 'shift from slice' );
  # is( $sliced->shift,  'g', 'shift from slice' );
  #
  # '$sliced' is Modifiable
  # $sliced->store( 1, 'x' );
  # is( $sliced->fetch( 1 ),  'x', 'store and fetch from slice' );

;}
