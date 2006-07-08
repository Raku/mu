token whitespace:<basic_whitespace> { \s+ }

rule identifier { \w+ }
token literal_int { \d+ }

rule simple_call { <identifier><ws><expr>; }
macro statement_control:<a_call> (Match $m --> Match) is parsed(rx:perl5/<simple_call>/) {$m}

multi infix:<*> ($a,$b) {...}
multi infix:<+> ($a,$b) is looser(infix:<*>) {...}

say 3;
say 3+4 * 5;
