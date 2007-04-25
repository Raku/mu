use v6-alpha;

module Main {

say '1..2';

my $counter;

my $var =
BEGIN { 
    my $c = 1;
    $counter = sub { $c };
};

say "ok ", $counter();

}
