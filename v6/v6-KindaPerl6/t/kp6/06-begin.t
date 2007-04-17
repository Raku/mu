use v6-alpha;

module Main {

say '1..1';

my $counter;

BEGIN { 
    $counter = 1;
    my $lex = 2;
};

say "ok ", $counter;

}
