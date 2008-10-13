use v6;

# P49 (**) Gray code.
# 
# An n-bit Gray code is a sequence of n-bit strings constructed according to
# certain rules. For example,
# 
# n = 1: C(1) = ['0','1'].
# n = 2: C(2) = ['00','01','11','10'].
# n = 3: C(3) = ['000','001','011','010',Â´110Â´,Â´111Â´,Â´101Â´,Â´100Â´].
# 
# Find out the construction rules and write a predicate with the following
# specification:
# 
# % gray(N,C) :- C is the N-bit Gray code
# 
# Can you apply the method of "result caching" in order to make the predicate
# more efficient, when it is to be used repeatedly?

sub gray($n) {
    return ('',) if $n == 0;
    '0' >>~<< gray($n-1), '1' >>~<< gray($n-1).reverse;
}

sub gray2($n) {
    return ('',) if $n == 0;
    state @g[$n] //= ('0' >>~<< gray2($n-1), '1' >>~<< gray2($n-1).reverse);
}

unless caller() {
    use Test;
    plan 6;

    is gray(1), <0 1>;
    is gray(2), <00 01 11 10>;
    is gray(3), <000 001 011 010 110 111 101 100>;
    is gray2(1), <0 1>;
    is gray2(2), <00 01 11 10>;
    is gray2(3), <000 001 011 010 110 111 101 100>;
}
