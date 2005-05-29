#!/usr/bin/pugs

# Generates all the combinations of letters that can be made from all
# the combinations of the last 4 digits of a phone number.
#
# For more details, see:
#    http://www.perlmonks.org/index.pl?node_id=453821
#
# CAUTION: Produces a lot of output and takes a long time to complete.

use v6;

my %digit_letters = (
    2 => [qw(a b c)],
    3 => [qw(d e f)],
    4 => [qw(g h i)],
    5 => [qw(j k l)],
    6 => [qw(m n o)],
    7 => [qw(p r s)],
    8 => [qw(t u v)],
    9 => [qw(w x y)],
);

my @letterchoices;

my $letters;
my $letterchooser = choose([0 .. 2], 4);
while $letters = $letterchooser() {
    push @letterchoices, $letters;
}

my $digits;
my $digitchooser = choose([2 .. 9], 4);
while $digits = $digitchooser() {
    my $letters;
    for @letterchoices -> $letters {
        my @digits  = split '', $digits;
        my @letters = split '', $letters;

        my @word;

        for zip(@digits, @letters) -> $digit, $letter {
            push @word, %digit_letters{$digit}[$letter];
        }

        say "$digits: ", @word;
    }
}

sub basen ($base, $num) {
    my $q = int($num / $base);
    my $r =     $num % $base;

    return $r if $q == 0;
    return basen($base, $q), $r;
}

sub choose ($list, $number) {
    my $iterations = $list.elems ** $number;
    my $current = 0;

    return sub {
        return if $current >= $iterations;

        my @choice = basen($list.elems, $current++);
        unshift @choice, 0 while @choice.elems < $number;

        return @choice.map({$list[$_]}).join("");
    };
}
