use v6-alpha;

use Test;

plan 2;

{
    my sub foo ($x) { $x.perl }

    my $pair = (a => 1);
    my $Pair = $pair.ref;

    ok try { foo($Pair) }, "passing ::Pair to a sub works";
}

# But this works:
{
    my sub foo ($x) { $x.perl }

    my $int = 42;
    my $Int = $int.ref;

    ok try { foo($Int) }, "passing ::Int to a sub works";
}
