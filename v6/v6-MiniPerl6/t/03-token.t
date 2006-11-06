use v6-alpha;

use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

{
    my $p = MiniPerl6::Grammar::Regex.term( 'a' );
    say ($$p).perl;
    say ($$p).emit;
}

#{
#    my $p = MiniPerl6::Grammar::Regex.term( '$1' );
#    say ($$p).perl;
#    say ($$p).emit;
#}

{
    my $p = MiniPerl6::Grammar::Regex.term( '<%h>' );
    say ($$p).perl;
    say ($$p).emit;
}

