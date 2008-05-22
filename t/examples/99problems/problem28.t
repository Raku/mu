use v6;
use Test;
plan 2;

# P28 (**) Sorting a list of lists according to length of sublists
# 
# a) We suppose that a list contains elements that are lists themselves. The
# objective is to sort the elements of this list according to their length. E.g.
# short lists first, longer lists later, or vice versa.
# 
# Example:
# * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
# ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
# 
# b) Again, we suppose that a list contains elements that are lists themselves.
# But this time the objective is to sort the elements of this list according to
# their length frequency; i.e., in the default, where sorting is done
# ascendingly, lists with rare lengths are placed first, others with a more
# frequent length come later.
# 
# Example:
# * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
# ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
# 
# Note that in the above example, the first two lists in the result have length 4
# and 1, both lengths appear just once. The third and forth list have length 3
# which appears twice (there are two list of this length). And finally, the last
# three lists have length 2. This is the most frequent length.
# 
# Arithmetic

my @input= [<a b c>],[<d e>],[<f g h>],[<d e>],[<i j k l>],[<m n>],[<o>];
my @expected= [<o>],[<d e>],[<d e>],[<m n>],[<a b c>],[<f g h>],[<i j k l>];

# we could use 
#  sort: {+$_}
# but pugs seem to not support this yet

my @sorted=@input.sort: {+$^a <=> +$^b};
is @expected, 
   @sorted,
   "We should be able to sort a list of lists according to length of sublists";

# the list is not the same as in the sample text, when two lists have the
# same frequency of length the ordering is unspecified, so this should be ok

@expected= [<o>],[<i j k l>],[<a b c>],[<f g h>],[<d e>],[<d e>],[<m n>];

# group lists by length

my %grouped;
for (@input) {%grouped{+$_}.push($_)}

# now sort the values by frequency, again can't use
#  sort: {+$_}

@sorted= %grouped.values.sort: {+$^a <=> +$^b};
is @expected,@sorted, "..or according to frequency of length of sublists" 
