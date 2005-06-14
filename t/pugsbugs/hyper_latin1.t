#!/usr/bin/pugs

use v6;
use Test;

plan 2;

# please do not change the filenecoding of this file!!

# » ~ « only works in utf8 encoded files

# the test works if fileencoding is set to utf8
# it doesn't work if fileencoding is latin1

my $hyper_utf=('a','b','c') »~« (1,2,3);
is($hyper_utf,'a1b2c3',:todo<bug>);


# >> ~ << is working
my $hyper_latin=('a','b','c') >>~<< (1,2,3);
is($hyper_latin,'a1 b2 c3','>>~<< is working');

# hmm, more bugs?  need to to go to bed now, will look into this
# tomorrow
#   say ('a','b','c') >>~<< (1,2,3);
# differs from
#   my $hyper_latin=('a','b','c') >>~<< (1,2,3);
#   say $hyper_latin;
