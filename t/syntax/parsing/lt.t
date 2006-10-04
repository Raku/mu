use v6-alpha;

use Test;

plan 1;

# Note to Wim Vanderbauwhede: I threw together a test such as the one
# requested by audreyt in an email to perl6-users. Beg pardon if you
# looked forward to doing it. :/
#
# At least now you're a committer. :) Make sure you add yourself to the
# AUTHORS file, and go look on #perl6 if something needs to be done. As
# of this writing, a lot of tests need to be looked over in various ways.
#
# Good luck! -- masak, 2006-10-04

dies_ok({ 1<2 }, 'the syntax "1<2" is illegal, space must precede "<"');
