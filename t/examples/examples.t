#!/usr/bin/pugs

use v6;

=pod

Test examples

This loads all the scripts in the examples dir and attempts
to compile and run each of them.

=cut


my @examples  = <fp hanoi life mandel quicksort sendmoremoney shuffle>;

my @outputs   = <fp hanoi             quicksort>;

say "1..{ @examples + @outputs }";

my $c = 0;

for (@examples) {
    ++$c;
    say "ok $c # skip Try to compile $_\.p6";
    # when (@outputs) {
    if ($_ eq any(@outputs)) {
        ++$c;
        say "ok $c # skip Try to run $_\.p6 and compare to output/$_";
    }
}
