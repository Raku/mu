#!/usr/bin/pugs

use v6;
use Test;

plan 2;

# Unicode-version of >>~<< only works in utf8 encoded files

#   * Almost all other files in the Pugs Subversion repository are utf-8
#     encoded.
#   * Some editors/editor configurations will always safe files as utf-8.
#   * My add-the-usual-svn-properties script sets svn:mime-type to utf-8.
# Because of these reasons, I felt it'd be better to simply encode the latin1
# "<<"s and ">>"s using the \ddd notation.
my $hyper_utf = eval "(<a b c) \187~\171 (1,2,3)";
is($hyper_utf,'a1 b2 c3', :todo<bug>);


# >>~<< is working
my $hyper_latin=('a','b','c') >>~<< (1,2,3);
is($hyper_latin,'a1 b2 c3','>>~<< is working');

# hmm, more bugs?  need to to go to bed now, will look into this
# tomorrow
#   say ('a','b','c') >>~<< (1,2,3);
# differs from
#   my $hyper_latin=('a','b','c') >>~<< (1,2,3);
#   say $hyper_latin;
