use v6-alpha;

module Main {

say '1..1';

my $counter;

do {
    my $c = 1;
    $counter = sub { $c };
};

say "ok ", $counter.();

}
