use v6;
use FindBin;

# P27 (**) Group the elements of a set into disjoint subsets.
# 
# a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2,
# 3 and 4 persons? Write a function that generates all the possibilities and
# returns them in a list.
# 
# Example:
# * (group3 '(aldo beat carla david evi flip gary hugo ida))
# ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
# ... )
# 
# b) Generalize the above predicate in a way that we can specify a list of group
# sizes and the predicate will return a list of groups.
# 
# Example:
# * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
# ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
# ... )
# 
# Note that we do not want permutations of the group members; i.e. ((ALDO BEAT)
# ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference
# between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
# 
# You may find more about this combinatorial problem in a good book on discrete
# mathematics under the term "multinomial coefficients".

@INC.push($FindBin::Bin);
require "problem26.t";

# XXX treats @elems as a set; i.e. duplicated values are 
# treated as identical, not distinct.
sub group(@sizes, @elems) {
    return [] if @sizes == 0;
    map -> $e {
        map -> $g {
            [ [@$e], @$g ]
        }, group(@sizes[1..*], grep { not $_ === any(@$e) }, @elems)
    }, combination(@sizes[0], @elems)
}

unless caller {
    use Test;
    plan 1;

    is group((2,1), (1,2,3,4)),
    (((1,2),(3,))
    ,((1,2),(4,))
    ,((1,3),(2,))
    ,((1,3),(4,))
    ,((1,4),(2,))
    ,((1,4),(3,))
    ,((2,3),(1,))
    ,((2,3),(4,))
    ,((2,4),(1,))
    ,((2,4),(3,))
    ,((3,4),(1,))
    ,((3,4),(2,))), 'group works';
}
