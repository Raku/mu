#!/usr/bin/perl

my $test_file = @*ARGS[0] || 'test';

print(('-' x 80), "\n");
print "Testing perl5 naive bayesian script with $test_file\n";
print(('-' x 80), "\n");

if ("words.db-p5.pl"~~:!e) {
    run("perl naive_bayesian-p5.pl add apples apples");
    run("perl naive_bayesian-p5.pl add oranges oranges");
    run("perl naive_bayesian-p5.pl add grapes grapes");
}
run("perl naive_bayesian-p5.pl classify $test_file");

print(('-' x 80), "\n");
print "Testing perl6 naive bayesian script with $test_file\n";
print(('-' x 80), "\n");

if ("words.db.pl"~~:!e) {
    run("pugs naive_bayesian.pl add apples apples");
    run("pugs naive_bayesian.pl add oranges oranges");
    run("pugs naive_bayesian.pl add grapes grapes");
}
run("pugs naive_bayesian.pl classify $test_file");
