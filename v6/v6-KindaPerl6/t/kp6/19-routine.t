use v6-alpha;

module Main {

say '1..1';

my &s;

do {
    my $c = 1;
    &s := sub { $c };
};

say "ok ", &s.();

}
