use v6-alpha;

module Main {

say '1..2';

my $counter;

do {
    my $c = 0;
    $counter = sub { $c = $c + 1 };
};

say "ok ", $counter.();
say "ok ", $counter.();

}
