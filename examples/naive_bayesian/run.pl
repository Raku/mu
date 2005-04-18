#!/usr/bin/perl

my $test_file = shift || 'test';

print(('-' x 80), "\n");
print "Testing perl5 naive bayesian script with $test_file\n";
print(('-' x 80), "\n");

if (! -e "words.db.p5") {
    system("perl naive_bayesian.p5 add apples apples");
    system("perl naive_bayesian.p5 add oranges oranges");
    system("perl naive_bayesian.p5 add grapes grapes");
}
system("perl naive_bayesian.p5 classify $test_file");

print(('-' x 80), "\n");
print "Testing perl6 naive bayesian script with $test_file\n";
print(('-' x 80), "\n");

if (! -e "words.db.p6") {
    system("pugs naive_bayesian.p6 add apples apples");
    system("pugs naive_bayesian.p6 add oranges oranges");
    system("pugs naive_bayesian.p6 add grapes grapes");
}
system("pugs naive_bayesian.p6 classify $test_file");
