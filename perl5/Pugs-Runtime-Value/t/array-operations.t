#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 36;

# use_ok( 'Pugs::Runtime::Container::Array' );
use Pugs::Runtime::Container::Array; 
use Pugs::Runtime::Value;
use Pugs::Runtime::Value::List;

use constant Inf => Pugs::Runtime::Value::Num::Inf;

{
  # string range
  my $iter = Pugs::Runtime::Value::List->from_range( start => 'a', end => Inf, step => undef );
  is( $iter->shift, 'a', 'string range' );  
  is( $iter->shift, 'b', 'string range 1' );
}

{
  # 'Iter' object
  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 0, end => 13, step => 1 );

  # warn "iter ". join(',', map{ $iter->shift } (0..10) );

  my $span = Pugs::Runtime::Container::Array->from_list( -1, 9, $iter );

  # warn "span ". join(',', map{ $span->shift } (0..10) );

  #warn $span;
  #warn "@{[$span->items]} items";

  my $grepped = $span->to_list->grep( sub { $_[0] % 3 == 0 } );
  #warn "@{[$grepped->items]} items";
  is( $grepped->shift, 9, 'grep  ' );  
  #warn "@{[$grepped->items]} items";
  is( $grepped->shift, 0, 'grep 0' );
  #warn "@{[$grepped->items]} items";
  is( $grepped->shift, 3, 'grep 1' );

  my $mapped = $grepped->map( sub { $_[0] % 6 == 0 ? ($_[0], $_[0]) : () } );
  is( $mapped->shift,  6, 'map 0' );
  is( $mapped->shift,  6, 'map 1' );
  is( $mapped->shift, 12, 'map 0' );
  is( $mapped->shift, 12, 'map 1' );

  is( $mapped->shift, undef, 'end' );
}

{
  # multidimensional lazy array
  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 0, end => 9, step => 1 );
  my $a1 = Pugs::Runtime::Container::Array->from_list( $iter );
  my @a2 = 1..10;
  my $b1 = Pugs::Runtime::Container::Array->from_list( $a1, [@a2] ); 
  isa_ok( $b1->shift, 'Pugs::Runtime::Container::Array', 'without "splat", inserts an array' );
  isa_ok( $b1->shift, 'ARRAY' );
}

{
  # splat
  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 0, end => 9, step => 1 );
  my $a1 = Pugs::Runtime::Container::Array->from_list( $iter );
  my $b1 = Pugs::Runtime::Container::Array->from_list( $a1->to_list, 1..10 );
  is( ref($b1->shift), '', 'with "splat", inserts the elements' );
  is( ref($b1->shift), '' );
}

{
  # uniq
  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 0, end => 9, step => 1 );
  my $a1 = Pugs::Runtime::Container::Array->from_list( 1, $iter );
  $a1 = $a1->to_list->uniq;
  is( $a1->shift, 1, 'not seen element' );
  is( $a1->shift, 0, 'not seen element' );
  is( $a1->shift, 2, 'seen element was skipped' );

  # end
  $a1 = Pugs::Runtime::Container::Array->from_list( $a1 );
  is( $a1->end, 9, 'end' );
  is( $a1->pop, 9, 'end is still there' );
}

{
  # (test originally written to test coroutines)

  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 1, end => 2, step => 1 );
  my $a1 = Pugs::Runtime::Container::Array->from_list( $iter );
  is( $a1->shift, 1, 'lazy array from subroutine' );
  is( $a1->shift, 2, 'subroutine' );
  is( $a1->shift, undef, 'subroutine end' );
  is( $a1->shift, undef, 'subroutine really ended' );
}

{
  # kv
  
  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 4, end => 5, step => 1 );
  my $a1 = Pugs::Runtime::Container::Array->from_list( $iter );
  $a1 = $a1->to_list->kv;
  is( $a1->shift, 0, 'kv' );
  is( $a1->shift, 4, 'kv' );
  is( $a1->shift, 1, 'kv' );
  is( $a1->shift, 5, 'kv' );
}

{
  # pairs
  
#  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 4, end => 5, step => 1 );
#  my $a1 = Pugs::Runtime::Container::Array->from_list( $iter );
#  $a1 = $a1->to_list.Pugs::Runtime::Value::List::pairs;
#  my $p = $a1->shift;
#  is( $p->ref,  'Pair',     'pair', :todo<wrong type> );
#  is( $p->perl, '(0 => 4)', 'pair', :todo<wrong type> );
;}

{
  # zip
  
  my $iter1 = Pugs::Runtime::Value::List->from_num_range( start => 4, end => 5, step => 1 );
  my $a1 =    Pugs::Runtime::Container::Array->from_list( $iter1 );
  my $iter2 = Pugs::Runtime::Value::List->from_num_range( start => 1, end => 3, step => 1 );
  my $a2 =    Pugs::Runtime::Container::Array->from_list( $iter2 );
  
  $a1 = $a1->to_list->zip( $a2 );
  is( $a1->shift, 4, 'zip' );
  is( $a1->shift, 1, 'zip' );
  is( $a1->shift, 5, 'zip' );
  is( $a1->shift, 2, 'zip' );
  is( $a1->shift, undef, 'zip' );
  is( $a1->shift, 3, 'zip' );
  is( $a1->shift, undef, 'zip' );
}

{
  # elems
  my $iter = Pugs::Runtime::Value::List->from_num_range( start => 1, end => 1000000, step => 2 );
  is( $iter->elems, 500000, 'Lazy List elems' );

  # not implemented
  # is( $iter->kv->elems, 1000000, 'Lazy List elems doubles after kv()' );

  my $a1 =    Pugs::Runtime::Container::Array->from_list( 'z', $iter );
  is( $a1->elems, 500001, 'Lazy Array elems' );
}
