#!/usr/bin/pugs

use v6;
use Test;

=kwid 

Testing array slices.

=cut

plan 19;

{   my @array = (3,7,9);

    is(@array[0,1,2], (3,7,9),   "basic slice");
    is(@array[(0,1,2)], (3,7,9), "basic slice, explicit list");

    is(@array[0,0,2,1,1,2], "3 3 9 7 7 9", "basic slice, duplicate indices");

    my @slice = (1,2);

    is(@array[@slice], "7 9",      "slice from array, part 1");
    is(@array[@slice], (7,9),      "slice from array, part 2");
    is(@array[@slice[1]], (9),     "slice from array slice, part 1");
    is(@array[@slice[0,1]], (7,9), "slice from array slice, part 2");
}

# Behaviour assumed to be the same as Perl 5
{   my @array  = <a b c d>;
    my @slice := @array[1,2];
    is ~(@slice = <A B C D>), "A B",
        "assigning a slice too many items yields a correct return value",
        :todo<bug>;
}

# Binding on array slices
{   my @array = <a b c d>;

    try { @array[1, 2] := <B C> };
    is ~@array, "a B C d", "binding array slices works (1)";
}

{   my @array = <a b c d>;
    my $foo   = "B";
    my $bar   = "C";

    try { @array[1, 2] := ($foo, $bar) };
    is ~@array, "a B C d", "binding array slices works (2)";

    $foo = "BB";
    $bar = "CC";
    is ~@array, "a BB CC d", "binding array slices works (3)";

    @array[1] = "BBB";
    @array[2] = "CCC";
    is ~@array, "a BBB CCC d", "binding array slices works (4)";
    is $foo,    "BBB",         "binding array slices works (5)";
    is $bar,    "CCC",         "binding array slices works (6)";
}

# Slices on array literals
{   is ~(<a b c d>[1,2]),   "b c", "slice on array literal";
    is ~([<a b c d>][1,2]), "b c", "slice on arrayref literal";
}

# Calculated slices
{   my @array = (3,7,9);
    my %slice = (0=>3, 1=>7, 2=>9);
    is((3,7,9), [@array[%slice.keys].sort],    "values from hash keys, part 1");
    is((3,7,9), [@array[%slice.keys.sort]],    "values from hash keys, part 2");
    is((3,7,9), [@array[(0,1,1)>>+<<(0,0,1)]], "calculated slice: hyperop");
}
