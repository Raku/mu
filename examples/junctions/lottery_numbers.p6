#!/usr/bin/pugs

use v6;

=pod

This examples exploits the fact that $junction.pick is choosen (pseudo)-randomly

=cut

my $lottery_numbers = any(1 .. 99);
my @your_numbers;
for (0 .. 10) { @your_numbers.push($lottery_numbers.pick()) }
say 'Your lottery numbers are: ' ~ join('-', @your_numbers);