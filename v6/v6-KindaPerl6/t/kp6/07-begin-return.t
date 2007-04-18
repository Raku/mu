use v6-alpha;

module Main {

say '1..2';

my $counter;

my $var =
BEGIN { 
    $counter = 1;
    my $lex = 2;
};

say "ok ", $counter;
say "ok ", $var;

}
