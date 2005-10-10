#!/usr/bin/pugs

use v6;

=pod

This examples exploits the fact that $junction.pick is choosen (pseudo)-randomly

=cut

my $lottery_numbers = any(1 .. 99);
my @your_numbers;
my $new = $lottery_numbers.pick();
for (0 .. 10) {
    $new = $lottery_numbers.pick() while $new == any(@your_numbers);
    @your_numbers.push($new)
}
say "Your lottery numbers are: { join('-', @your_numbers) }";
