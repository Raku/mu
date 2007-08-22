use v6-alpha;

module Main {

say '1..2';

my $counter;

my $var =
BEGIN { 
    $counter := 1;
    my $lex = 2;
};

if ($counter) {
    say "ok ", $counter;
} else {
    say "not ok 1"
};
if ($var) {
    say "ok ", $var;
} else {
    say "not ok 2"
};

}
