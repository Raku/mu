#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 33;

# use_ok( 'Perl6::Container::Array' );
use Perl6::Container::Array; 
use Perl6::Value::List qw(Inf);

{
  # normal splice

  my $span = Perl6::Container::Array->from_list( 1 .. 10 );
  isa_ok( $span, 'Perl6::Container::Array', 'created an Perl6::Container::Array' );

  my $spliced = $span->splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Perl6::Container::Array', 'result is an Perl6::Container::Array' );

  is( join(',', $span->items), '1,2,23,24,25,6,7,8,9,10', 'span' );
  is( join(',', $spliced->items), '3,4,5', 'splice' );
}

{
  # end of stream
  my $a = Perl6::Container::Array->from_list( 1 .. 2 );
  is( $a->shift, 1, 'iter 0' );
  is( $a->shift, 2, 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # end of lazy stream
  my $iter = Perl6::Value::List->from_range( start => 1, end => 2, step => 1 );
  my $a = Perl6::Container::Array->from_list( $iter );
  is( $a->shift, 1, 'iter 0' );
  is( $a->shift, 2, 'iter 1' );
  is( $a->shift, undef, 'end' );
}

{
  # splice with negative offset

  my $span = Perl6::Container::Array->from_list( 1 .. 10 );
  my $spliced = $span->splice( -4, 3, 23..25 );

  is( join(',',$span->items), '1,2,3,4,5,6,23,24,25,10', 'span' );
  is( join(',',$spliced->items), '7,8,9', 'splice' );
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
  is( $iter->shift, 0, 'iter 0' );
  is( $iter->shift, 1, 'iter 1' );
  
  my $span = Perl6::Container::Array->from_list( 'x', 'y', 'z', $iter );
  my $spliced = $span->splice( 1, 4, () );

  is( join(',',$span->items), 'x,<obj:Perl6::Value::List>', 'span' );
  is( join(',',$spliced->items), 'y,z,2,3', 'splice' );

  $spliced = $span->splice( 0, 3, ( 'a' ) );

  is( join(',',$span->items), 'a,<obj:Perl6::Value::List>', 'span' );
  is( join(',',$spliced->items), 'x,4,5', 'splice again' );
  
  is( $span->shift, 'a', 'shift' );
  is( $span->shift, '6', 'shift' );

  is( $span->pop, Inf, 'pop' );
  is( $span->pop, Inf, 'pop' );
  
  $span->push( "z" );
  is( $span->pop, "z", 'push - pop' );
  is( $span->pop, Inf, 'pop' );

  $span->unshift( "a" );
  is( $span->shift, "a", 'unshift - shift' );
  is( $span->shift, 7, 'shift' );
  
  # reverse
  my $rev = $span->reverse();
  isa_ok( $rev, 'Perl6::Container::Array', 'reversed' );
  is( $rev->shift, Inf, 'shift reverse' );
  is( $rev->pop,   8,   'pop reverse' );
  my $rev_splice = $rev->splice( -2, 1, ('k') );
  is( $rev_splice->pop,   10,   'spliced' );
  is( $rev->pop,   9,   'splice reverse' );
  is( $rev->pop, 'k',   'splice reverse' );
  is( $rev->pop,  11,   'splice reverse' );
}

