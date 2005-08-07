#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 
use Iter::Range;

{
  # 'Iter' object
  my $iter = Lazy::Range.new( start => 0, end => 13, step => 1 );
  my $span = Array::Lazy.new( -1, 9, $iter );

  my $grepped = $span.grep:{ $_ % 3 == 0 };
  is( $grepped.shift, 9, 'grep  ' );  
  is( $grepped.shift, 0, 'grep 0' );
  is( $grepped.shift, 3, 'grep 1' );

  my $mapped = $grepped.map:{ $_ % 6 == 0 ?? ($_, $_) :: () };
  is( $mapped.shift,  6, 'map 0' );
  is( $mapped.shift,  6, 'map 1' );
  is( $mapped.shift, 12, 'map 0' );
  is( $mapped.shift, 12, 'map 1' );

  is( $mapped.shift, undef, 'end' );
}
