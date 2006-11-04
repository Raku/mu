use v6-alpha;

use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

say "here";
{
    my $p = MiniPerl6::Grammar::Regex.term( 'a' );
    say ($$p).perl;
}

