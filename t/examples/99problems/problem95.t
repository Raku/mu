use v6;
use Test;
plan 1;

# P95 (**) English number words
# 
# On financial documents, like cheques, numbers must sometimes be written in
# full words. Example: 175 must be written as one-seven-five. Write a predicate
# full-words/1 to print (non-negative) integer numbers in full words.

my %nw{0..9} = <zero one two three four five six seven eight nine>;

sub numword1 (Int $n) {fail if $n<0; join '-', %nw{split '', ~$n} };

is numword1(175),'one-seven-five','Wordify numbers in a specific format.';
