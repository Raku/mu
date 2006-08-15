#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 63;

use Pugs::Runtime::Container::Array; 
use Pugs::Runtime::Value;
use Pugs::Runtime::Value::List;

# use constant Inf => Pugs::Runtime::Value::Num::Inf;

{
  # str()
  my $span = Array->new( );
  is( $span->str->unboxed, '()', '... str()' );
  $span->push( 1 );
  is( $span->str->unboxed, '(1)', '...' );
  $span->push( 2 );
  is( $span->str->unboxed, '(1, 2)', '...' );
  $span->push( 3 );
  is( $span->str->unboxed, '(1, 2, 3)', '...' );
  $span->push( 4 );
  is( $span->str->unboxed, '(1, 2, 3, 4)', '...' );
  $span->push( List->new( '$.unboxed' => Pugs::Runtime::Value::List->from_num_range( start => 0, end => Inf, step => 1  ) ) );
  is( $span->str->unboxed, '(1, 2, 3 ... Inf)', '...' );
  is( $span->fetch(6)->fetch, 2, '...' );
}

{
  # splice: offset > 0, length > 0

  my $span = Array->new();
  $span->push( Pugs::Runtime::Value::List->from_single( 1 .. 10 ) );
  isa_ok( $span, 'Array', 'created an Array' );
  is( $span->perl->unboxed, '(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)', '... perl()' );

  my $spliced = $span->splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Array', '... splice' );

  is( $span->perl->unboxed, '(1, 2, 23, 24, 25, 6, 7, 8, 9, 10)', '... splice: offset > 0, length > 0' );
  is( $spliced->perl->unboxed, '(3, 4, 5)', '... splice' );
}

{
  # splice: offset > 0, length = 0
  my $span = Array->new();
  $span->push( Pugs::Runtime::Value::List->from_single( 1 .. 10 ) );
  my $spliced = $span->splice( 2, 0, 99 );
  is( $span->perl->unboxed, '(1, 2, 99, 3, 4, 5, 6, 7, 8, 9, 10)', '... splice: offset > 0, length = 0' );
  is( $spliced->perl->unboxed, '()', '... splice' );
}

{
  # splice: offset > 0, length < 0
  my $span = Array->new();
  $span->push( Pugs::Runtime::Value::List->from_single( 1 .. 10 ) );
  my $spliced = $span->splice( 2, -1, 99 );
  is( $span->perl->unboxed, '(1, 2, 99, 10)', '... splice: offset > 0, length < 0' );
  is( $spliced->perl->unboxed, '(3, 4, 5, 6, 7, 8, 9)', '... splice' );
}

{
  # splice: offset < 0, length > 0
  my $span = Array->new();
  $span->push( Pugs::Runtime::Value::List->from_single( 1 .. 10 ) );
  my $spliced = $span->splice( -2, 1, 99 );
  is( $span->perl->unboxed, '(1, 2, 3, 4, 5, 6, 7, 8, 99, 10)', '... splice: offset < 0, length > 0' );
  is( $spliced->perl->unboxed, '(9)', '... splice' );
}

{
  # splice: offset < 0, length = 0
  my $span = Array->new();
  $span->push( Pugs::Runtime::Value::List->from_single( 1 .. 10 ) );
  my $spliced = $span->splice( -2, 0, 99 );
  is( $span->perl->unboxed, '(1, 2, 3, 4, 5, 6, 7, 8, 99, 9, 10)', '... splice: offset < 0, length = 0' );
  is( $spliced->perl->unboxed, '()', '... splice' );
}

{
  # splice: offset < 0, length < 0
  my $span = Array->new();
  $span->push( Pugs::Runtime::Value::List->from_single( 1 .. 10 ) );
  my $spliced = $span->splice( -3, -1, 99 );
  is( $span->perl->unboxed, '(1, 2, 3, 4, 5, 6, 7, 99, 10)', '... splice: offset < 0, length < 0' );
  is( $spliced->perl->unboxed, '(8, 9)', '... splice' );
}

{
  # Scalar with an Array inside

    my $scalar = Scalar->new();
    my $array = Array->new();
    $array->push( Pugs::Runtime::Value::List->from_single( 1 .. 2 ) );
    $scalar->store( $array );

    # use Data::Dumper;
    # $Data::Dumper::Indent = 1;
    # print Dumper( $scalar );
    my $x = $scalar->fetch( 1 );
    #print Dumper( \$x );
    is( $x->fetch, 2, 'fetch from auto-dereferenced scalar works' );

    # store
    $scalar->store( 1, 3 );
    $x = $scalar->fetch( 1 );
    #print Dumper( \$x );
    is( $x->fetch, 3, 'store to auto-dereferenced scalar works' );
}

{
  # fetch

  my $list = Pugs::Runtime::Value::List->from_num_range( start => 0, end => 1000000 );
  my $array1 = Array->new();
  $array1->push( $list );

  $array1->fetch( 100 );
  is( $array1->fetch( 100 )->fetch, 100, 'fetch' );
  is( $array1->fetch(  99 )->fetch,  99, '... fetch' );
  is( $array1->fetch(  98 )->fetch,  98, '... fetch' );
  is( $array1->fetch(   0 )->fetch,   0, '... fetch' );
  is( $array1->fetch( 1000000 )->fetch, 1000000, '... fetch' );
  is( $array1->store(  50, 99 ), $array1, 'store' );
  is( $array1->fetch(  49 )->fetch,  49, '... fetch' );
  is( $array1->fetch(  50 )->fetch,  99, '... fetch' );
  is( $array1->fetch(  51 )->fetch,  51, '... fetch' );
  is( $array1->fetch(   0 )->fetch,   0, '... fetch' );
  is( $array1->fetch( 1000000 )->fetch, 1000000, '... fetch' );
}

{
  # slicing

  my $array = Array->new();
  $array->push( Pugs::Runtime::Value::List->from_single( 1 .. 10 ) );

  my $slice = Array->new();
  $slice->push( Pugs::Runtime::Value::List->from_single( 4, 5, 6 ) );

  my $sliced = $array->slice( $slice );

  isa_ok( $sliced, 'Array', 'slice is an Array' );
  is( $sliced->perl->unboxed, '(5, 6, 7)', '... slice perl()' );

  $sliced->store( 1, 99 );

  is( $array->perl->unboxed, '(1, 2, 3, 4, 5, 99, 7, 8, 9, 10)', '... original array' );

  # fetch returns a lvalue
  my $a = $array->fetch( 2 );
  $a->store( 55 );
  is( $array->perl->unboxed, '(1, 2, 55, 4, 5, 99, 7, 8, 9, 10)', 'fetch returns lvalue' );
  
  # fetch lvalue is a normal scalar - if it is bound to another value,
  # the binding to the array must be lost
  my $b = Scalar->new();
  $a->bind( $b );
  $a->store( 44 );
  is( $b->fetch, 44 );
  is( $array->perl->unboxed, '(1, 2, 55, 4, 5, 99, 7, 8, 9, 10)', 
      'fetch returns a normal scalar' );
}

{
  # lazy slicing - fetch/store elements

  my $array = Array->new();
  $array->push( Pugs::Runtime::Value::List->from_num_range( start => 1000, end => 100_000 ) );

  my $slice = Array->new();
  # TODO - lazy slice index with step!=1 should die
  $slice->push( Pugs::Runtime::Value::List->from_num_range( start => 4, end => 1_000_000 ) );

  my $sliced = $array->slice( $slice );

  isa_ok( $sliced, 'Array', 'lazy slice is an Array' );
  is( $sliced->perl(max=>3)->unboxed, 
      '(1004, 1005, 1006 ... undef, undef, undef)',
      '... slice perl()' );

  $sliced->store( 1, 99 );

  is( $array->perl(max=>7)->unboxed, 
      '(1000, 1001, 1002, 1003, 1004, 99, 1006 ... '.
      '99994, 99995, 99996, 99997, 99998, 99999, 100000)', 
      '... original array' );
}

{
  # lazy slicing - fetch whole slices

  my $array = Array->new();
  $array->push( Pugs::Runtime::Value::List->from_num_range( start => 1000, end => 100_000 ) );

  my $idx = Array->new();
  $idx->push( 2 );
  $idx->push( Pugs::Runtime::Value::List->from_num_range( start => 4, end => 1_000_000 ) );
  # TODO TEST - should die if a lazy slice index has step!=1 
  my $sliced = $array->slice( $idx );

  # fetch a lazy slice
  my $array2 = Array->new();
  $array2->store( $sliced );

  is( $array2->perl(max=>3)->unboxed, 
      '(1002, 1004, 1005 ... undef, undef, undef)',
      'lazy slice was stored into a new Array' );
      
  is( $array->perl(max=>7)->unboxed, 
      '(1000, 1001, 1002, 1003, 1004, 1005, 1006 ... '.
      '99994, 99995, 99996, 99997, 99998, 99999, 100000)', 
      '... original array is unmodified' );
  
}

{
  # lazy slicing - store to whole slices

  my $array = Array->new();
  $array->push( Pugs::Runtime::Value::List->from_num_range( start => 1000, end => 100_000 ) );

  my $idx = Array->new();
  $idx->push( 2 );
  $idx->push( Pugs::Runtime::Value::List->from_num_range( start => 4, end => 100 ) ); 
  my $sliced = $array->slice( $idx );

  # store to a lazy slice
  my $array2 = Array->new();
  $array2->push( 99, Pugs::Runtime::Value::List->from_num_range( start => 100, end => 200_000_000 ) );
  $sliced->store( $array2 );

  is( $sliced->perl(max=>3)->unboxed, 
      '(99, 100, 101 ... 194, 195, 196)',
      'Array slice was modified' );
      
  is( $idx->perl(max=>3)->unboxed, 
      '(2, 4, 5 ... 98, 99, 100)',
      '... index Array was not modified' );
      
  is( $array->perl(max=>7)->unboxed, 
      '(1000, 1001, 99, 1003, 100, 101, 102 ... '.
      '99994, 99995, 99996, 99997, 99998, 99999, 100000)', 
      '... original array' );
  
}

{
  # lazy slicing - store to whole slices - special case, tests a new feature in splice()

  my $array = Array->new();
  $array->push( Pugs::Runtime::Value::List->from_num_range( start => 1000, end => 100_000 ) );
  is( $array->perl(max=>3)->unboxed, 
      '(1000, 1001, 1002 ... 99998, 99999, 100000)', 'Array to be sliced' );
  my $array_elems = $array->elems->unboxed;
  is( $array_elems, 99001, '... Array has xxx elements' );

  my $idx = Array->new();
  $idx->push( 2 );
  $idx->push( Pugs::Runtime::Value::List->from_num_range( start => 4, end => 1_000_000 ) ); 
    # end => 1_000_000 - was not supported before
  is( $idx->perl(max=>3)->unboxed, 
      '(2, 4, 5 ... 999998, 999999, 1000000)', '... Array with indexes' );
  my $elems = $idx->elems->unboxed;
  is( $elems, 999998, '... Array with indexes has xxx elements' );
      
  my $sliced = $array->slice( $idx );
  is( $sliced->perl(max=>3)->unboxed, 
      '(1002, 1004, 1005 ... undef, undef, undef)', '... Resulting Array slice' );
      
  # array that will be stored into lazy slice
  my $array2 = Array->new();
  $array2->push( 99, Pugs::Runtime::Value::List->from_num_range( start => 100, end => 200_000_000 ) );
  is( $array2->perl(max=>3)->unboxed, 
      '(99, 100, 101 ... 199999998, 199999999, 200000000)', '... Array to be stored into the slice' );
      
  # store to a lazy slice
  $sliced->store( $array2 );

  is( $elems, $idx->elems->unboxed, '... Array with indexes still has xxx elements' );
  is( $elems, $sliced->elems->unboxed, '... Array slice also has xxx elements' );
  is( $sliced->perl(max=>3)->unboxed, 
      '(99, 100, 101 ... undef, undef, undef)',
      '... Array slice was modified; elements without source generate undefs' );
      
  is( $idx->perl(max=>3)->unboxed, 
      '(2, 4, 5 ... 999998, 999999, 1000000)',
      '... index Array was not modified' );
      
  is( $array->perl(max=>7)->unboxed, 
      '(1000, 1001, 99, 1003, 100, 101, 102 ... 99090, 99091, 99092, 99093, 99094, 99095, 99096)', 
      '... original array' );
  is( $array_elems, $array->elems->unboxed, '... Original Array still has xxx elements' );
  
}

{
  # (a,b) = (b,a)
  # lazy slicing - fetch/store to whole slices

  my $array = Array->new();
  $array->push( Pugs::Runtime::Value::List->from_num_range( start => 1000, end => 1010 ) );

  my $idx1 = Array->new();
  $idx1->push( Pugs::Runtime::Value::List->from_num_range( start => 1, end => 5 ) ); 
  my $slice1 = $array->slice( $idx1 );

  my $idx2 = Array->new();
  $idx2->push( Pugs::Runtime::Value::List->from_num_range( start => 3, end => 6 ) ); 
  my $slice2 = $array->slice( $idx2 );

  $slice1->store( $slice2 );

  is( $slice1->perl()->unboxed, 
      '(1003, 1004, 1005, 1006, undef)',
      'Array slice was modified' );
            
  is( $array->perl()->unboxed, 
      '(1000, 1003, 1004, 1005, 1006, undef, 1006, 1007, 1008, 1009, 1010)', 
      '... original array' );
  
}
