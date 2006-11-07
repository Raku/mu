use v6-alpha;
use MiniPerl6::Grammar;
use MiniPerl6::Emitter;

{
  my $p = MiniPerl6::Grammar.token( 'token { abc }' );
  say ($$p).perl;
  say ($$p).emit;
}


