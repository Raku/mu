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

  my $grepped = $span->grep( sub { $_ % 3 == 0 } );
  is( $grepped->shift, 0, 'grep  ' );  
  is( $grepped->shift, 3, 'grep 0' );

  my $mapped = $grepped->map( sub { $_ % 6 == 0 ? ($_, $_) : () } );
  is( $mapped->shift,  6, 'map 0' );
  is( $mapped->shift,  6, 'map 1' );
  is( $mapped->shift, 12, 'map 0' );
  is( $mapped->shift, 12, 'map 1' );

  is( $mapped->shift, undef, 'end' );
}

{
  # subroutine
  
  sub mylist { 1 }
  
  my $a1 = Perl6::Value::List->from_sub( &mylist ); 
  is( $a1->shift, 1, 'lazy array from subroutine' );
  is( $a1->shift, undef, 'subroutine end' );
  is( $a1->shift, undef, 'subroutine really ended' );
}

{
  # elems
  my $iter = Perl6::Value::List->from_range( start => 1, end => 1000000, step => 2 );
  is( $iter->Perl6::Value::List::elems, 500000, 'Lazy List elems' );

  is( $iter->kv.Perl6::Value::List::elems, 1000000, 'Lazy List elems doubles after kv()' );
}

__END__

** TODO

{
  # kv
  
  my sub mylist { yield $_ for 4..5; yield; }
  
  my $a1 = Perl6::Value::List->new( cstart => &mylist ); 
  $a1 = $a1->kv;
  is( $a1->shift, 0, 'kv' );
  is( $a1->shift, 4, 'kv' );
  is( $a1->shift, 1, 'kv' );
  is( $a1->shift, 5, 'kv' );
}

{
  # pairs
  
  my sub mylist { yield $_ for 4..5; yield; }
  
  my $a1 = Perl6::Value::List->new( cstart => &mylist ); 
  $a1 = $a1->pairs;
  my $p = $a1->shift;
  is( $p->ref,  'Pair',     'pair', :todo<wrong type> );
  is( $p->perl, '(0 => 4)', 'pair', :todo<wrong type> );
}

{
  # zip
  
  my $a1 = Perl6::Value::List->from_range( start => 4, end => 5 ); 
  
  my sub mylist2 { yield $_ for 1..3; yield; }
  my $a2 = Perl6::Value::List->new( cstart => &mylist2 ); 
  
  $a1 = $a1->Perl6::Value::List::zip( $a2 );
  is( $a1->shift, 4, 'zip' );
  is( $a1->shift, 1, 'zip' );
  is( $a1->shift, 5, 'zip' );
  is( $a1->shift, 2, 'zip' );
  is( $a1->shift, undef, 'zip' );
  is( $a1->shift, 3, 'zip' );
  is( $a1->shift, undef, 'zip' );
}

