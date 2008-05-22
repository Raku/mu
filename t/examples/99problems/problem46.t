use v6;
use Test;
plan 1;

# P46 (**) Truth tables for logical expressions.
#
# Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
# logical equivalence) which succeed or fail according to the result of their
# respective operations; e.g. and(A,B) will succeed, if and only if both A and B
# succeed. Note that A and B can be Prolog goals (not only the constants true and
# fail).
#
# A logical expression in two variables can then be written in prefix notation,
# as in the following example: and(or(A,B),nand(A,B)).
#
# Now, write a predicate table/3 which prints the truth table of a given logical
# expression in two variables.
#
# Example:
# * table(A,B,and(A,or(A,B))).
# true true true
# true fail true
# fail true fail
# fail fail fail


# --


sub stringify($Thing) {
    if $Thing {
        return 'true';
    } else {
        return 'fail'; # as per problem description
    };
};

# Obviously we can't just make 'or' respective 'and' subs
# because those are builtin operators.  Maybe there's a way
# around that, but I wouldn't know how to call the original
# operator in the sub (core::and?), so I bend the task
# description a little and just prefix the subs with
# an underscore.
sub _or($A, $B) {return ($A or $B)};
sub _and($A, $B) {return ($A and $B)};
sub _nand($A, $B) {return !($A and $B)};
sub _nor($A, $B) {return !($A or $B)};
sub _xor($A, $B) { # FIXME if you know DeMorgan
    return False if $A and $B;
    return ($A or $B);
};
sub _impl($A, $B) {
    if $A and !$B {
        return False;
    } else {
        return True;
    };
};
sub _equ($A, $B) {return $A == $B};

sub table($param) {
    my $expr = $param;
# I have to copy this around or else I get
# "Can't modify constant item: VStr"
# error as soon as I want to modify it

    $expr ~~ s:P5/^A,B,//;
    $expr ~~ s:P5:g/([AB])/$$0/;
# first capture is now $0
    $expr ~~ s:P5:g/([nx]?or|n?and|impl|equ)/_$0/;     #:

    my @table;
    for (True, False) -> $A {
        for (True, False) -> $B {
            push @table, (
                join ' ', (
                    stringify $A,
                    stringify $B,
                    stringify eval $expr
                )
            ) ~ "\n";
        };
    };

    return @table;
};

is q:to''       #: this is a heredoc
true true true
true fail true
fail true fail
fail fail fail

, join('',
    table('A,B,and(A,or(A,B))')
), 'P46 (**) Truth tables for logical expressions.';
