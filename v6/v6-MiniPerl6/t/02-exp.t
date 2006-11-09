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

{
  my $p = MiniPerl6::Grammar.exp( '$a.bool( 10 )' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$a.bool( 10 + (10) )' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$m.to' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '((( ( substr( $str, $m.to, 1) eq \'a\'
  ) )))' );
  say ($$p).perl;
  say ($$p).emit;
}

{
  my $p = MiniPerl6::Grammar.exp( '$m.bool( ((( ( substr( $str, $m.to, 1) eq \'a\'
  )  ?? (1 + $m.to( $m.to + 1 ))  !! (0) ) && ( ( substr( $str, $m.to, 1) eq \'b\'
  )  ?? (1 + $m.to( $m.to + 1 ))  !! (0) ) && ( ( substr( $str, $m.to, 1) eq \'c\'
  )  ?? (1 + $m.to( $m.to + 1 ))  !! (0) ))))' );
  say ($$p).perl;
  say ($$p).emit;
}
