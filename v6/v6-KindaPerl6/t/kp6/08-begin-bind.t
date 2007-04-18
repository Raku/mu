use v6-alpha;

module Main {

say '1..2';

my $counter;
my $counter1;

my $var =
BEGIN { 
    $counter1 = 1;
    $counter := $counter1;
    my $lex = 2;
};

say "ok ", $counter;
say "ok ", $var;

}
