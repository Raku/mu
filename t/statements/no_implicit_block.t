use v6-alpha;

use Test;

plan 6;

# L<S04/The Relationship of Blocks and Declarations/"no implicit blocks" around 
#   "standard control structures">
{
    my $y;
    if (my $x = 2) == 2 {
        $y = $x + 3;
    }
    is $x, 2, '$x assigned in while\'s condition';
    is $y, 5, '$y assigned in while\'s body';
}

{
    my $y;
    given (my $x = 2) {
        when 2 { $y = $x + 3; }
    }
    is $x, 2, '$x assigned in while\'s condition';
    is $y, 5, '$y assigned in while\'s body';
}

{
    my $y;
    while (my $x = 2) == 2 {
        $y = $x + 3;
        last;
    }
    is $x, 2, '$x assigned in while\'s condition';
    is $y, 5, '$y assigned in while\'s body';
}
