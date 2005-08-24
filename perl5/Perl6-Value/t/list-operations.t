#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 31;
 
use Perl6::Value;
use Perl6::Value::List;

use constant Inf => Perl6::Value::Num::Inf;

{
  # string range
  my $iter = Perl6::Value::List->from_range( start => 'a', end => Inf );
  is( $iter->shift, 'a', 'string range' );  
  is( $iter->shift, 'b', 'string range 1' );
}

{
  # stringify
  my $span = Perl6::Value::List->from_num_range( start => 0, end => 10, step => 1 );
  is( $span->perl, '(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)', 'perl()' );
  $span = Perl6::Value::List->from_num_range( start => 0, end => Inf, step => 1 );
  is( $span->perl, '(0, 1, 2 ... '.&Inf.')', 'perl()' );
}

{
  # 'Iter' object
  my $span = Perl6::Value::List->from_num_range( start => 0, end => 13, step => 1 );

  is( $span->elems, 14, 'elems' );

  my $grepped = $span->grep( sub { $_[0] % 3 == 0 } );
  is( $grepped->shift, 0, 'grep  ' );  
  is( $grepped->shift, 3, 'grep 0' );

  my $mapped = $grepped->map( sub { $_[0] % 6 == 0 ? ($_[0], $_[0]) : () } );
  is( $mapped->shift,  6, 'map 0' );
  is( $mapped->shift,  6, 'map 1' );
  is( $mapped->shift, 12, 'map 0' );
  is( $mapped->shift, 12, 'map 1' );

  # this is hard to fix -- after '12', there is one element left in the list;
  # when grep finds out that there are no more useful elements, it is too late.

  # possible fix - map() always fetch one extra element to the buffer, before returning.

  is( $mapped->shift, undef, 'end' );
}

{
  # subroutine
 
  my @a = ( 1, 2 ); 
  sub mylist { shift @a }
  
  my $a1 = Perl6::Value::List->from_coro( \&mylist ); 
  is( $a1->shift, 1, 'lazy array from subroutine' );
  is( $a1->shift, 2, 'subroutine end' );
  # is( $a1->shift, undef, 'subroutine really ended' );
}

{
  # elems
  my $iter = Perl6::Value::List->from_num_range( start => 1, end => 1000000, step => 2 );
  is( $iter->elems, 500000, 'Lazy List elems' );

  # is( $iter->kv->elems, 1000000, 'Lazy List elems doubles after kv()' );
}

{
  # uniq
 
  my @a = ( 1, 2, 2, 3 ); 
  sub mylist0 { shift @a; } # my $x = shift @a;print " --> shifting $x\n";  $x }
  
  my $a1 = Perl6::Value::List->from_coro( \&mylist0 )->uniq; 
  is( $a1->shift, 1, 'uniq' );
  is( $a1->shift, 2, 'uniq' );
  is( $a1->shift, 3, 'uniq' );
  # is( $a1->shift, undef, 'subroutine really ended' );
}

{
  # kv
  
  my @a = ( 4, 5 ); 
  sub mylist2 { shift @a }
  
  my $a1 = Perl6::Value::List->new( cstart => \&mylist2 ); 
  $a1 = $a1->kv;
  is( $a1->shift, 0, 'kv' );
  is( $a1->shift, 4, 'kv' );
  is( $a1->shift, 1, 'kv' );
  is( $a1->shift, 5, 'kv' );
}

{
#  # pairs
#  
#  my $a1 = Perl6::Value::List->from_range( start => 4, end => 5 ); 
#
#  $a1 = $a1->pairs;
#  my $p = $a1->shift;
# TODO  is( $p->ref,  'Pair',     'pair' );
# TODO  is( $p->perl, '(0 => 4)', 'pair' );
;}

{
  # zip
  
  my $a1 = Perl6::Value::List->from_num_range( start => 4, end => 5 ); 
  my $a2 = Perl6::Value::List->from_num_range( start => 1, end => 3 ); 
  $a1 = $a1->zip( $a2 );
  is( $a1->shift, 4, 'zip' );
  is( $a1->shift, 1, 'zip' );

  # is( $a1->elems, Inf, 'zip' );

  is( $a1->shift, 5, 'zip' );
  is( $a1->shift, 2, 'zip' );
  is( $a1->shift, undef, 'zip' );
  is( $a1->elems, 1 );
  is( $a1->shift, 3, 'zip' );
  is( $a1->elems, 0 );
  is( $a1->shift, undef, 'zip' );
}

