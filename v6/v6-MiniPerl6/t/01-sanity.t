use Test::More;
use MiniPerl6::Grammar;
use Data::Dumper;

{
  my $p = MiniPerl6::Grammar->var( '$abc' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->exp( '$abc' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->exp_seq( '$abc' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->exp_seq( '$abc, $def, $xyz' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->apply( 'mysub( $abc, $def, $xyz )' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->call( '$obj.meth( $abc, $def, $xyz )' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->bind( '$obj := $xyz' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->exp_mapping( '$obj => $xyz' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->exp_mapping( '$obj => $xyz, $a => $b' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->lit_object( '::Tree($a => $x, $b => $y)' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->return( 'return $a' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->while( 'while $a { $b }' );
  print Dumper( $$p );
}

{
  my $p = MiniPerl6::Grammar->for( 'for $a -> $b { $b }' );
  print Dumper( $$p );
}
