#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 30;

use Perl6::Container::Array; 
use Perl6::Value;
use Perl6::Value::List;

# use constant Inf => Perl6::Value::Num::Inf;

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
  $span->push( List->new( '$.unboxed' => Perl6::Value::List->from_num_range( start => 0, end => Inf, step => 1  ) ) );
  is( $span->str->unboxed, '(1, 2, 3 ... Inf)', '...' );
  is( $span->fetch(6)->fetch, 2, '...' );
}

{
  # normal splice

  my $span = Array->new();
  $span->push( Perl6::Value::List->from_single( 1 .. 10 ) );
  isa_ok( $span, 'Array', 'created an Array' );
  is( $span->perl->unboxed, '(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)', '... perl()' );

  my $spliced = $span->splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Array', '... splice' );

  is( $span->perl->unboxed, '(1, 2, 23, 24, 25, 6, 7, 8, 9, 10)', '... splice' );
  is( $spliced->perl->unboxed, '(3, 4, 5)', '... splice' );
}

{
  # Scalar with an Array inside

    my $scalar = Scalar->new();
    my $array = Array->new();
    $array->push( Perl6::Value::List->from_single( 1 .. 2 ) );
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
  # slicing

  my $array = Array->new();
  $array->push( Perl6::Value::List->from_single( 1 .. 10 ) );

  my $slice = Array->new();
  $slice->push( Perl6::Value::List->from_single( 4, 5, 6 ) );

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
  $array->push( Perl6::Value::List->from_num_range( start => 1000, end => 100_000 ) );

  my $slice = Array->new();
  # TODO - lazy slice index with step!=1 should die
  $slice->push( Perl6::Value::List->from_num_range( start => 4, end => 1_000_000 ) );

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
  $array->push( Perl6::Value::List->from_num_range( start => 1000, end => 100_000 ) );

  my $idx = Array->new();
  $idx->push( 2 );
  $idx->push( Perl6::Value::List->from_num_range( start => 4, end => 1_000_000 ) );
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
  $array->push( Perl6::Value::List->from_num_range( start => 1000, end => 100_000 ) );

  my $idx = Array->new();
  $idx->push( 2 );
  $idx->push( Perl6::Value::List->from_num_range( start => 4, end => 100 ) ); 
    # XXX - TODO - end => 1_000_000 - not supported yet
  my $sliced = $array->slice( $idx );

  # store to a lazy slice
  my $array2 = Array->new();
  $array2->push( 99, Perl6::Value::List->from_num_range( start => 100, end => 200_000_000 ) );
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
  # (a,b) = (b,a)
  # lazy slicing - fetch/store to whole slices

  my $array = Array->new();
  $array->push( Perl6::Value::List->from_num_range( start => 1000, end => 1010 ) );

  my $idx1 = Array->new();
  $idx1->push( Perl6::Value::List->from_num_range( start => 1, end => 5 ) ); 
  my $slice1 = $array->slice( $idx1 );

  my $idx2 = Array->new();
  $idx2->push( Perl6::Value::List->from_num_range( start => 3, end => 6 ) ); 
  my $slice2 = $array->slice( $idx2 );

  $slice1->store( $slice2 );

  is( $slice1->perl()->unboxed, 
      '(1003, 1004, 1005, 1006, undef)',
      'Array slice was modified' );
            
  is( $array->perl()->unboxed, 
      '(1000, 1003, 1004, 1005, 1006, undef, 1006, 1007, 1008, 1009, 1010)', 
      '... original array' );
  
}
