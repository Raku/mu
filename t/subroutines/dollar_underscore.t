#!/usr/bin/pugs

# Tests for a bug uncovered when Jesse Vincent was testing 
# functionality for Patrick Michaud

use v6;

use Test;

plan 3;


my @list = ('a');


# according to A06:    
#
#   Methods, submethods, macros, rules, and pointy subs all  
#   bind their first argument to C<$_>; ordinary subs declare
#   a lexical C<$_> but leave it undefined.   

# Do pointy subs send along a declared param?

for @list -> $letter { is( $letter , 'a') }

# Do pointy subs send along an implicit param
for @list -> { is($_, 'a') }


# Do pointy subs send along an implicit param even when a param is declared
# (See the quote from A06 above)
for @list -> $letter { is( $_ ,'a', :todo<clarification>) }


