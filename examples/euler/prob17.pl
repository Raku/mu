#!/usr/bin/env pugs

# vim: filetype=perl6

=begin Problem
If the numbers 1 to 5 are written out in words: one, two, three, four, five; there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
=end Problem

use v6;
use Benchmark;

sub build {
    my @count = map { $_.chars }, <zero one two three four five six seven eight
                                   nine ten eleven twelve thirteen fourteen
                                   fifteen sixteen seventeen eighteen nineteen>;

    my @tens = map { $_.chars }, <twenty thirty forty fifty sixty seventy eighty
                                  ninety>;

    for @tens -> $t {
        push @count, $t;
        for 1..9 -> $o {
            push @count, $t + @count[$o];
        }
    }

    # caching these values saves *minutes*
    my $hundred = "hundred".chars;
    my $hundredand = "hundredand".chars;

    for 1..9 -> $h {
        push @count, @count[$h] + $hundred;
        for 1..99 -> $ot {
            push @count, @count[$h] + $hundredand + @count[$ot];
        }
    }

    push @count, "onethousand".chars;
    return @count;
}

sub letter_count($n) {
    my @count = build();
    reduce { $^a + @count[$^b] }, 0 .. $n;
}

sub check {
    my $count = letter_count(5);
    say $count == 19 ?? "ok" !! "not ok: got $count, expected 19";
}

sub main {
    say letter_count(1000);
}

my @t = timeit(1, \&main);
say "execution time: @t[0]"

