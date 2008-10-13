use v6;

# P26 (**) Generate the combinations of K distinct objects chosen from the N
# elements of a list
#
# In how many ways can a committee of 3 be chosen from a group of 12 people? We
# all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
# well-known binomial coefficients). For pure mathematicians, this result may be
# great. But we want to really generate all the possibilities in a list.
#
# Example:
# * (combination 3 '(a b c d e f))
# ((A B C) (A B D) (A B E) ... )

sub combination($n, @xs) {
    if $n > @xs {
        ()
    } elsif $n == 0 {
        ([])
    } elsif $n == @xs {
        [@xs]
    } else {
        ((map { [@xs[0],$_] },combination($n-1,@xs[1..*])),
         combination($n,@xs[1..*]))
    }
}

unless caller() {
    use Test;
    plan 1;
    
    is combination(3, (1..5)),
    ((1, 2, 3),
     (1, 2, 4),
     (1, 2, 5),
     (1, 3, 4),
     (1, 3, 5),
     (1, 4, 5),
     (2, 3, 4),
     (2, 3, 5),
     (2, 4, 5),
     (3, 4, 5)), "combinations work.";
}
