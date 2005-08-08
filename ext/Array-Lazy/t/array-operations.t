#!/usr/bin/pugs

use v6;
use Test;

plan 38;

# use_ok( 'Array::Lazy' );
use Array::Lazy; 
use Iter::Range;

{
  # string range
  my $iter = Lazy::Range.new( start => 'a', end => Inf, step => undef );
  is( $iter.shift, 'a', 'string range' );  
  is( $iter.shift, 'b', 'string range 1' );
}

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

{
  # multidimensional lazy array
  my $iter = Lazy::Range.new( start => 0, end => 9, step => 1 );
  my $a1 = Array::Lazy.new( $iter );
  my @a2 = 1..10;
  my $b1 = Array::Lazy.new( $a1, [[@a2]] );  # XXX why does it need double []?
  isa_ok( $b1.shift, 'Array::Lazy', 'without "splat", inserts an array' );
  isa_ok( $b1.shift, 'Array' );
}

{
  # splat
  my $iter = Lazy::Range.new( start => 0, end => 9, step => 1 );
  my $a1 = Array::Lazy.new( $iter );
  my $b1 = Array::Lazy.new( $a1.splat, 1..10 );
  isa_ok( $b1.shift, 'Int', 'with "splat", inserts the elements' );
  isa_ok( $b1.shift, 'Int' );
}

{
  # elems
  my $iter = Lazy::Range.new( start => 0, end => 9, step => 1 );
  my $a1 = Array::Lazy.new( $iter );
  is( $a1.elems, Inf, 'unknown size returns Inf' );
  $a1 = Array::Lazy.new( 0..9 );
  is( $a1.elems, 10, 'known size returns size' );
}

{
  # uniq
  my $iter = Lazy::Range.new( start => 0, end => 9, step => 1 );
  my $a1 = Array::Lazy.new( 1, $iter );
  $a1 = $a1.uniq;
  is( $a1.shift, 1, 'not seen element' );
  is( $a1.shift, 0, 'not seen element' );
  is( $a1.shift, 2, 'seen element was skipped' );

  # end
  is( $a1.end, 9, 'end' );
  is( $a1.pop, 9, 'end is still there' );
}

{
  # coroutine
  
  my coro mylist { yield $_ for 1..2; yield; }
  
  my $iter = Lazy::CoroList.new( start => &mylist ); 
  my $a1 = Array::Lazy.new( $iter );
  is( $a1.shift, 1, 'lazy array from coroutine' );
  is( $a1.shift, 2, 'coroutine' );
  is( $a1.shift, undef, 'coroutine end' );
  is( $a1.shift, undef, 'coroutine really ended' );
}

{
  # kv
  
  my coro mylist { yield $_ for 4..5; yield; }
  
  my $iter = Lazy::CoroList.new( start => &mylist ); 
  my $a1 = Array::Lazy.new( $iter );
  $a1 = $a1.kv;
  is( $a1.shift, 0, 'kv' );
  is( $a1.shift, 4, 'kv' );
  is( $a1.shift, 1, 'kv' );
  is( $a1.shift, 5, 'kv' );
}

{
  # pairs
  
  my coro mylist { yield $_ for 4..5; yield; }
  
  my $iter = Lazy::CoroList.new( start => &mylist ); 
  my $a1 = Array::Lazy.new( $iter );
  $a1 = $a1.pairs;
  my $p = $a1.shift;
  is( $p.ref,  'Pair',     'pair', :todo<wrong type> );
  is( $p.perl, '(0 => 4)', 'pair', :todo<wrong type> );
}

{
  # zip
  
  my coro mylist1 { yield $_ for 4..5; yield; }
  my $iter1 = Lazy::CoroList.new( start => &mylist1 ); 
  my $a1 =    Array::Lazy.new( $iter1 );
  
  my coro mylist2 { yield $_ for 1..3; yield; }
  my $iter2 = Lazy::CoroList.new( start => &mylist2 ); 
  my $a2 =    Array::Lazy.new( $iter2 );
  
  $a1 = $a1.Array::Lazy::zip( $a2 );
  is( $a1.shift, 4, 'zip' );
  is( $a1.shift, 1, 'zip' );
  is( $a1.shift, 5, 'zip' );
  is( $a1.shift, 2, 'zip' );
  is( $a1.shift, undef, 'zip' );
  is( $a1.shift, 3, 'zip' );
  is( $a1.shift, undef, 'zip' );
}
