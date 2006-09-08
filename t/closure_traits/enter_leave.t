use v6-alpha;

use Test;

plan 6;

# L<S04/Closure traits/ENTER "at every block entry time">
# L<S04/Closure traits/LEAVE "at every block exit time">

{
    my $str;
    my sub foo ($x, $y) {
        ENTER { $str ~= "(" }
        LEAVE { $str ~= ")" }
        $str ~= "$x,$y";
    }
    foo(3,4);
    is $str, '(3,4)';
    foo(-1,2);
    is $str, '(3,4)(-1,2)';
}

# reversed order
{
    my $str;
    my sub foo ($x, $y) {
        $str ~= "$x,$y";
        LEAVE { $str ~= ")" }
        ENTER { $str ~= "(" }
    }
    foo(7,-8);
    is $str, '(7,-8)';
    foo(5,0);
    is $str, '(7,-8)(5,0)';
}

# multiple ENTER and LEAVE blocks
{
    my $str;
    {
        ENTER { $str ~= '[' }
        LEAVE { $str ~= ']' }

        $str ~= 21;

        ENTER { $str ~= '(' }
        LEAVE { $str ~= ')' }

        ENTER { $str ~= '{' }
        LEAVE { $str ~= '}' }
    }
    is $str, '[({21})]', 'multiple ENTER/LEAVE worked';
}

# L<S04/Closure traits/ENTER "repeats on loop blocks">
{
    my $str;
    for 1..2 {
        $str ~= ',';
        ENTER { $str ~= "E$_" }
        LEAVE { $str ~= "L$_ " }
    }
    is $str, 'E1,L1 E2,L2', 'ENTER/LEAVE repeats on loop blocks';
}
