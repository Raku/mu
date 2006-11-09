use v6-alpha;
use MiniPerl6::Grammar;
use MiniPerl6::Emitter;

{
  my $p = MiniPerl6::Grammar.exp_2( '1 + 1' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp_2( '1 + 2 + 3' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$a := 1 + 2 + 3' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '1 ?? 2 !! 3' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$a := 1 + substr( $a, 2 )' );
  say ($$p).perl;
  say ($$p).emit;
}

