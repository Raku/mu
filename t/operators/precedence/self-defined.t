use v6-alpha;

use Test;

=pod

Tests for precedence of self defined operators

L<S06/Subroutine traits/"relative to an existing">

=cut

plan 6;

do {
    sub prefix:<!> (Num $x) is tighter(&infix:<**>) {
        return 3 * $x;
    }

    is !1**2, 9, "'is tighter' on prefix works";
}

do {
    sub prefix:<foo> (Num $x) is looser(&infix:<+>) {
        return 2 * $x;
    }

    is foo 2 + 3, 10, "'is looser' on prefix works";
}


sub postfix:<!> (Num $x) is tighter(&infix:<**>) {
    return 2 * $x;
}

is 2**1!, 2, "'is tighter' on postfix works";



sub infix:<mul> ($a, $b) is looser(&infix:<+>) {
    return $a * $b;
}

ok 2 mul 3 + 4, 14, "'is looser' infix works 1";
ok 4 + 3 mul 2 , 14, "'is looser' infix works 2";

sub infix:<div> ($a, $b) is equiv(&infix:<*>) {
    return $a / $b;
}

ok(4 div 2 * 3, 6, "'is equiv' works");
