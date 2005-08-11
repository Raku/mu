#!/usr/bin/perl -w

use strict;

use Test::More;
plan tests => 28;
 
use Perl6::Value::List qw(Inf);

{
  # string range
  my $iter = Perl6::Value::List->from_range( start => 'a', end => Inf, step => undef );
  is( $iter->shift, 'a', 'string range' );  
  is( $iter->shift, 'b', 'string range 1' );
}

{
  # 'Iter' object
  my $span = Perl6::Value::List->from_range( start => 0, end => 13, step => 1 );

  my $grepped = $span->grep( sub { $_[0] % 3 == 0 } );
  is( $grepped->shift, 0, 'grep  ' );  
  is( $grepped->shift, 3, 'grep 0' );

  my $mapped = $grepped->map( sub { $_[0] % 6 == 0 ? ($_[0], $_[0]) : () } );
  is( $mapped->shift,  6, 'map 0' );
  is( $mapped->shift,  6, 'map 1' );
  is( $mapped->shift, 12, 'map 0' );
  is( $mapped->shift, 12, 'map 1' );

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
  my $iter = Perl6::Value::List->from_range( start => 1, end => 1000000, step => 2 );
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
  
  my $a1 = Perl6::Value::List->from_range( start => 4, end => 5 ); 
  my $a2 = Perl6::Value::List->from_range( start => 1, end => 3 ); 
  $a1 = $a1->zip( $a2 );
  is( $a1->shift, 4, 'zip' );
  is( $a1->shift, 1, 'zip' );

  # is( $a1->elems, Inf, 'zip' );

  is( $a1->shift, 5, 'zip' );
  is( $a1->shift, 2, 'zip' );
  is( $a1->shift, undef, 'zip' );
  is( $a1->shift, 3, 'zip' );
  is( $a1->shift, undef, 'zip' );
}

