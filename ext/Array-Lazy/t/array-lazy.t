#!/usr/bin/pugs

use v6;
use Test;

plan 6;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 

{
  my $span = Array::Lazy.new( 1 .. 10 );
  isa_ok( $span, 'Array::Lazy', 'created an Array::Lazy' );

  my $spliced = $span._splice( 2, 3, 23..25 );
  isa_ok( $spliced, 'Array::Lazy', 'result is an Array::Lazy' );

  is( $span.stringify, '1,2,23,24,25,6,7,8,9,10', 'span' );
  is( $spliced.stringify, '3,4,5', 'splice' );
}

{
  # negative offset

  my $span = Array::Lazy.new( 1 .. 10 );
  my $spliced = $span._splice( -4, 3, 23..25 );

  is( $span.stringify, '1,2,3,4,5,6,23,24,25,10', 'span' );
  is( $spliced.stringify, '7,8,9', 'splice' );
}

# TODO - create an 'Iter' object here

# TODO - test offset == 0, 1, 2, -1, -2, -Inf, Inf
# TODO - test length == 0, 1, 2, Inf
# TODO - test list == (), (1), (1,2), Iterators, ...


