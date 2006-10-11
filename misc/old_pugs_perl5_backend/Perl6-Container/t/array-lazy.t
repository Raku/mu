#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 32;

# use_ok( 'Perl6::Container::Array' );
use Perl6::Container::Array; 
use Perl6::Value;
use Perl6::Value::List;

#use constant Inf => Perl6::Value::Num::Inf;

   # NOTE - these tests no longer have to use "is_deeply" 

{
  # normal splice

  my $span = Perl6::Container::Array->from_list( 1 .. 10 );
  isa_ok( $span, 'Perl6::Container::Array', 'created an Perl6::Container::Array' );
  is( join(',', $span->items), '1,2,3,4,5,6,7,8,9,10', 'span' );

  my $spliced = $span->splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Perl6::Container::Array', 'result is an Perl6::Container::Array' );

  is( join(',', $span->items), '1,2,23,24,25,6,7,8,9,10', 'span' );
  is( join(',', $spliced->items), '3,4,5', 'splice' );
}

{
  # end of stream
  my $a = Perl6::Container::Array->from_list( 1 .. 2 );
  is( $a->shift, 1, 'iter 0' );
  is_deeply( [$a->shift], [2], 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # end of lazy stream
  my $iter = Perl6::Value::List->from_range( start => 1, end => 2, step => 1 );
  my $a = Perl6::Container::Array->from_list( $iter );
  is_deeply( [$a->shift], [1], 'iter 0' );
  is_deeply( [$a->shift], [2], 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # splice with negative offset

  my $span = Perl6::Container::Array->from_list( 1 .. 10 );
  my $spliced = $span->splice( -4, 3, 23..25 );

  is_deeply( [$span->items], [1,2,3,4,5,6,23,24,25,10], 'span' );
  is_deeply( [$spliced->items], [7,8,9], 'splice' );
}

{
  # -- no longer supported
  # according to pugs, this should yield 0..Inf|1..Inf
  # instead of 0|1,1|2,..Inf
  #
  # a lazy list of junctions
  # my $j = 0|1;
  # my $iter = Perl6::Value::List->from_range( start => $j, end => Inf, step => 1 );
  # is( $iter->shift, 0|1, 'iter 0|1' );
  # is( $iter->shift, 1|2, 'iter 1|2' );
  #
  # reversed
  # my $rev_iter = $iter->Perl6::Value::List::reverse;
  # is( $rev_iter->pop, 2|3, 'iter reverse 1' );
  # is( $rev_iter->pop, 3|4, 'iter reverse 2' );
;}

{
  # 'Iter' object
  my $iter = Perl6::Value::List->from_range( start => 0, end => Inf, step => 1 );
  is_deeply( [$iter->shift], [0], 'iter 0' );
  is_deeply( [$iter->shift], [1], 'iter 1' );
  
  my $span = Perl6::Container::Array->from_list( 'x', 'y', 'z', $iter );
  my $spliced = $span->splice( 1, 4, () );

  # test skipped - not portable
  # is_deeply( [$span->items], 'x,<obj:Perl6::Value::List>', 'span' );

  is_deeply( [$spliced->items], ['y','z',2,3], 'splice' );

  $spliced = $span->splice( 0, 3, ( 'a' ) );

  # test skipped - not portable
  # is( join(',',$span->items), 'a,<obj:Perl6::Value::List>', 'span' );

  is( join(',',$spliced->items), 'x,4,5', 'splice again' );
  
  is_deeply( [$span->shift], ['a'], 'shift' );
  is_deeply( [$span->shift], [6], 'shift' );

  is_deeply( [$span->pop], [Inf], 'pop' );
  is_deeply( [$span->pop], [Inf], 'pop' );
  
  $span->push( "z" );
  is_deeply( [$span->pop], ["z"], 'push - pop' );
  is_deeply( [$span->pop], [Inf], 'pop' );

  $span->unshift( "a" );
  is_deeply( [$span->shift], ["a"], 'unshift - shift' );
  is_deeply( [$span->shift], [7], 'shift' );
  
  # reverse
  my $rev = $span->reverse();   # $rev == Inf..8
  isa_ok( $rev, 'Perl6::Container::Array', 'reversed' );
  is_deeply( [$rev->shift], [Inf], 'shift reverse' );
  is_deeply( [$rev->pop],   [8],   'pop reverse' );  
  my $rev_splice = $rev->splice( -2, 1, ('k') );          # $rev == Inf..11,'k',9
  is_deeply( $rev_splice->pop,   10,   'spliced' );
  is_deeply( [$rev->pop],   [9],   'splice reverse' );
  is_deeply( [$rev->pop], ['k'],   'splice reverse' );
  is_deeply( [$rev->pop],  [11],   'splice reverse' );
}

