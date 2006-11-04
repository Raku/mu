use v6-alpha;

use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

{
    my $p = MiniPerl6::Grammar::Regex.term( 'a' );
    say ($$p).perl;
}

