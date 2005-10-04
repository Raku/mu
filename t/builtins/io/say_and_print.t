#!/usr/bin/pugs

use v6;

say "1..11";

# Tests for say
{
    say "ok 1 - basic form of say";
}

{
    say "o", "k 2 - say with multiple parame", "ters (1)";

    my @array = ("o", "k 3 - say with multiple parameters (2)");
    say @array;
}

{
    my $arrayref = <ok 4 - say stringifies its args>;
    say $arrayref;
}

{
    "ok 5 - method form of say".say;
}

# Tests for print
{
    print "ok 6 - basic form of print\n";
}

{
    print "o", "k 7 - print with multiple parame", "ters (1)\n";

    my @array = ("o", "k 8 - print with multiple parameters (2)\n");
    print @array;
}

{
    my $arrayref = (<ok 9 - print stringifies its args>, "\n");
    print $arrayref;
}

{
    "ok 10 - method form of print\n".print;
}

{
    print "o";
    print "k 11 - print doesn't add newlines\n";
}
